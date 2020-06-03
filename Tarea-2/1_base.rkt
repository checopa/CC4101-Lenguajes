#lang play


;################################ Interprete visto en clases ###########################

#|
<expr> ::= (num <num>)
         | (add <expr> <expr>)
         | (sub <expr> <expr>)
         | (if0 <expr> <expr> <expr>)
         | (id <id>)
         | (fun <sym> <expr>)
         | (app <expr> <expr>)
|#
(deftype Expr
  (num n)
  (add l r)
  (sub l r)
  (if0 c t f)
  (id x)
  (fun arg body)
  (app f-name f-arg))


;; parse :: s-expr -> Expr
;; converts s-exprs into Exprs where
#|
<s-expr> ::= <num>
           | <sym>
           | (list '+  <s-expr> <s-expr>)
           | (list '-  <s-expr> <s-expr>)
           | (list 'if0  <s-expr> <s-expr> <s-expr>)
           | (list 'fun (list <sym>) <s-expr>)
           | (list <s-expr> <s-expr>)
           | (list 'with (list <sym> <s-expr>) <s-expr>)
|#
(define (parse s-expr)
  (match s-expr
    [ n #:when (number? n) (num n)]
    [ x #:when (symbol? x) (id x)]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list '- l r) (sub (parse l) (parse r))]
    [(list 'if0 c t f) (if0 (parse c) (parse t) (parse f))]
    [(list 'fun (list x) b) (fun x (parse b))]
    [(list f a) (app (parse f) (parse a))]    
    [(list 'with (list x e) b) #:when (symbol? x)
         (app (fun x (parse b)) (parse e))]))


;; Abstract Dada Type (ADT) for handling environments 
;; empty-env  :: Env
;; extend-env :: Symbol Value Env -> Env
;; env-lookup :: Symbol Env -> Value

;; <env> ::= mtEnv
;;         | (aEnv <id> <value> <env>)
(deftype Env
  (mtEnv)
  (aEnv id val env))

(define empty-env (mtEnv))
 
(define extend-env aEnv)
 
(define (env-lookup x env)
  (match env
    [(mtEnv) (error 'env-lookup "free identifier: ~a" x)]
    [(aEnv id val rest) (if (symbol=? id x)
                            val
                            (env-lookup x rest))]))


;; Values of expressions 
;; <value> ::= (numV <number>)
;;          |  (closureV <sym> <s-expr> <env>) 
(deftype Value
  (numV n)
  (closureV id body env))

;; Auxiliary functions handling numeric Values
(define (op-bin f n1 n2)
  (numV (f (numV-n n1) (numV-n n2))))

(define (op-un f n)
  (numV (f (numV-n n))))


;; eval :: Expr Env -> Value
;; evaluates an expression in a given
;; environment using static scoping 
(define (eval expr env)
  (match expr
    [(num n) (numV n)]
    [(fun id body) (closureV id body env)]
    [(id x) (env-lookup x env)]
    [(add l r) (op-bin + (eval l env) (eval r env))]
    [(sub l r) (op-bin - (eval l env) (eval r env))]
    [(if0 c t f) (if  (op-un zero? (eval c env))
                      (eval t env)
                      (eval f env))]
    [(app f e) (def (closureV the-arg the-body the-claus-env) (eval f env))
               (def the-ext-env (extend-env the-arg (eval e env) the-claus-env))
               (eval the-body the-ext-env)]))


;; run :: s-expr -> Value
(define (run prog)
  (eval (parse prog) (mtEnv)))





;################################ Definiciones ###########################

(deftype Type
  (TNum)
  (TFun Targ Tret)
  (TVar Symbol))

(deftype Constraint
  (Cnst T1 T2))

(deftype TEnv
  (mtTEnv)
  (anTEnv id Type env))

(define count 0)

(define (get-id)
  (begin
    (set! count (add1 count))
    count))

(define (reset)
  (set! count 0))

(define (prettyfy T)
  (match T
    [(TNum) "num"]
    [(TVar x) (string-append "(TVar "(number->string x) ")")]
    [(TFun T1 T2) (string-append "(TFun " (prettyfy T1) " " (prettyfy T2) ")")]))




;################################ Su código va aquí ###########################

;;emptyT-env :: TEnv
;;Construye un ambiente vacio
(define emptyT-env mtTEnv)

;;extendT-env :: Sym x Type x TEnv -> TEnv
;;Extiende un ambiente asociando un tipo a un identificador dado
(define extendT-env
  anTEnv)

;;lookupT-env :: Sym x TEnv -> Type
;;Dado un identificador y un ambiente de tipos, retorna el tipo asociado al identificador
(define (lookupT-env x env)
  (match env
    [(mtTEnv) (error "Exception: free identifier "x)]
    [(anTEnv id type nenv)
     (if (symbol=? id x)
         type
         (lookupT-env x nenv)
         )]
    )
  )



;;typeof :: Expr x TEnv -> (Type, List[Constraint])
;;Dada una expresion y un ambiente de tipos, retorna el tipo de la expresion
;;con la lista de constraint que debe ser solucionable para que el programa
;;se valido a nivel de tipos
(define (typeof expr env)
  (match expr
    [(num n) (cons (TNum) '())]
    [(id x) (cons (lookupT-env x env) '())]
    [(add l r) (def L (typeof l env))
               (def R (typeof r env))
               (cons (TNum) (flatten (list (rest L) (rest R) (Cnst (first L) (TNum)) (Cnst (first R) (TNum)))))]

    [(sub l r) (def L (typeof l env))
               (def R (typeof r env))
               (cons (TNum) (flatten (list (rest L) (rest R) (Cnst (first L) (TNum)) (Cnst (first R) (TNum)))))]

    [(if0 e tb fb) (def E (typeof e env))
                   (def TB (typeof tb env))
                   (def FB (typeof fb env))
                   (cons (first TB) (flatten (list (rest E) (rest TB) (rest FB) (Cnst (first E) (TNum)) (Cnst (first TB) (first FB)))))]

    [(fun param body) (def A (TVar (get-id)))
                      (def B (typeof body (extendT-env param A env)))
                      (cons (TFun A (first B)) (rest B))]

    [(app fun arg) (def type (TVar (get-id)))
                   (def FUN (typeof fun env))
                   (def ARG (typeof arg env))
                   (cons type (flatten (list (rest FUN) (rest ARG) (Cnst (first FUN) (TFun (first ARG) type)))))]
    
    )
  )



;;TVAR x Type x Type -> Type
;;Reemplaza una variable de tipo por otro
;;dado un tipo
(define (subs from to type)
  (cond
    [(equal? from type) to]
    [(TFun? type) (def (TFun T1 T2) type) (TFun (subs from to T1) (subs from to T2))]
    [else type]
    )
  )


;;substitute :: TVAR x Type x List[Constraint]-> List[Constraint]
;;Reemplaza una variable de tipo por otro tipo dado
;;en una lista de constraints
(define (substitute from to _list)
  (map (λ (x)
    (match x
    [(Cnst T1 T2) (Cnst (subs from to T1) (subs from to T2))]
    )) _list))

  

;;occurs-in? :: TVAR x Type -> Bool
;;verifica si una variable de tipo ocurre
;;como subexpresion de otro tipo dado
(define (occurs-in? tvar t)
  (match t
    [(TNum) #f]
    [(TVar s) (equal? tvar t)]
    [(TFun type1 type2) (or (occurs-in? tvar type1) (occurs-in? tvar type2))]
    )
  )

;;unify :: List[Constraint] -> List[Constraint]
;;Dada una lista de constraints retorna la lista
;;unificada de constraints
(define (unify _list)
  (if (equal? _list '() )
      '()
      (match (first _list)
        [(Cnst T1 T2)
         (cond
           [(equal? T1 T2) (unify (rest _list))]
           [else (if (and (TVar? T1) (not (occurs-in? T1 T2)))
                     (append (unify(substitute T1 T2 (rest _list))) (list (Cnst T1 T2)))
                     (if (and (TVar? T2) (not (occurs-in? T2 T1)))
                         (append (unify(substitute T2 T1 (rest _list))) (list (Cnst T2 T1)))
                         (if (and (TFun? T1) (TFun? T2))
                             (match T1 [(TFun T1a T1r) (match T2 [(TFun T2a T2r) (unify (append (rest _list) (list (Cnst T1a T2a)) (list (Cnst T1r T2r))))])])
                             (error (string-append "Exception: Type error: cannot unify " (prettyfy T1) " with " (prettyfy T2)))
                             )
                         )
                     )]
           )]
        )
      )
  )

;;auxrun ::Type x List[Constraint] x List[Constraint] -> Type
;;Dado un tipo y una lista de constraints entrega su tipo final (calculado recursivamente)
(define (auxrun type lcons lcons_original)
  (cond
    [(equal? lcons '()) type]
    [(TFun? type) (def (TFun T1 T2) type) (TFun (auxrun T1 lcons_original lcons_original) (auxrun T2 lcons_original lcons_original))]
    [(and (TVar? type) (equal? (Cnst-T1 (first lcons)) type)) (auxrun (Cnst-T2 (first lcons)) lcons_original lcons_original)]
    [else (auxrun type (rest lcons) lcons_original)]
    )
  )
      

;;runType :: S-Expr -> Type
;;Dada una s-expresion retorna su tipo (o arroje un error)
(define (runType s-expr)
  (def expr (parse s-expr))
  (def tyof (typeof expr (emptyT-env) ))
  (def type (first tyof))
  (def constraint (unify (rest tyof)))
  (match type
    [(TVar n) (auxrun type constraint constraint)]
    [(TFun T1 T2) (TFun (auxrun T1 constraint constraint) (auxrun T2 constraint constraint))]
    )

  )

