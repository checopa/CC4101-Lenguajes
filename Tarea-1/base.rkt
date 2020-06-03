#lang play



#|
<Polynomial> ::= (nullp)
              |  (plus <Number> <Integer> >Polynomial>)
|#
(deftype Polynomial
  (nullp)
  (plus coef exp rem))



;;;;;;;;;;;;;;;;;;;;;;     EJERICIO 1      ;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;PARTE A;;;;;;;

;; mayorcoef :: Polynomial -> Bool
;; Verifica que los exponentes del polinomio esten de ordenados mayor a menor
(define (mayorcoef p)
  (match p
    [(nullp) #t]
    [(plus c e r) (match r
                    [(nullp) #t]
                    [(plus c2 e2 r2) (and (> e e2) (mayorcoef r))]
                    )]
    )
  )

;; coefcero :: Polynomial -> Bool
;; Verifica que ningun coeficiente del polinomio sea cero
(define (coefcero p)
  (match p
    [(nullp) #t]
    [(plus c e r) (and (not(zero? c )) (coefcero r))]
    )
  )

;; nf? :: Polynomial -> Bool
;; Verifica que un polinomio este en forma normal
(define (nf? p)
  (and (coefcero p) (mayorcoef p))
)

;;;;;;;PARTE B;;;;;;;

;; sumaMon :: Number Number Polynomial -> Polynomial
;; Suma un monomio de coeficiente c y exponente m al polinomio p
(define (sumaMon c m p)
  (if (zero? c)
      p
  (match p
    [(nullp) (plus c m p)]
    [(plus c1 e r) (cond
                     [(and (equal? m e) (zero? (+ c1 c))) r]
                     [(and (equal? m e) (plus (+ c1 c) e r))]
                     [(> m e) (plus c m p)]
                     [else (plus c1 e (sumaMon c m r))]
                     )]
    )
  )
 )
                   
                   
;; normalize :: Polynomial -> Polynomial
;; Normaliza un polinomio
(define (normalize p)
  (match p
    [(nullp) p]
    [(plus c e r) (sumaMon c e (normalize r))]
)
)



;;;;;;;;;;;;;;;;;;;;;;     EJERICIO 2      ;;;;;;;;;;;;;;;;;;;;;;

;; degreeauxilair :: Polynomial -> Integer
;; Devuelve el grado de un polinomio normalizado
(define (degreeauxiliar p)
  (match p
    [(nullp) (error "El polinomio nulo no tiene grado")]
    [(plus c e r) (match r
                    [(nullp) e]
                    [(plus c1 e1 r1) (max e (degreeauxiliar r))]
                    )]
    )
  )

;; degree :: Polynomial -> Integer
;; Devuelve el grado de un polinomio no necesariamente normalizado
(define (degree p)
  (degreeauxiliar (normalize p))
 )



;; coefficientauxiliar :: Integer Polynomial -> Number
;; Devueve el coeficiente asociado al exponente dado de un polinomio normalizado
(define (coefficientauxiliar n p)
  (match p
    [(nullp) 0]
    [(plus c e r) (if (= n e)
                      c
                      (coefficientauxiliar n r))]
   )
 )

;; coefficient :: Integer Polynomial -> Number
;; Devueve el coeficiente asociado a un exponente dado

(define (coefficient n p)
  (coefficientauxiliar n (normalize p))
 )



;;;;;;;;;;;;;;;;;;;;;;     EJERICIO 3      ;;;;;;;;;;;;;;;;;;;;;;

;; sumaPoly :: Polynomial Polynomial -> Polynomial
;; Suma dos polinomios (no necesariamente normalizados)

(define (sumaPoly p1 p2)
  (match (normalize p1)
    [(nullp) (normalize p2)]
    [(plus c1 e1 r1) (match p2
                       [(nullp) (normalize p1)]
                       [(plus c2 e2 r2) (sumaPoly (sumaMon c2 e2 (normalize p1)) r2)]
                      )
     ]
   )
)

;; mapPoly :: (Number Integer -> Number * Integer)Polynomial -> Polynomial
;; Aplica f a cada cada coeficiente y exponente del polinomio

(define (mapPoly f p)
  (match p
    [(nullp) p]
    [(plus c e r) (plus (car (f c e)) (cdr (f c e)) (mapPoly f r) )] 
    )
  )

;; multPoly :: Polynomial Polynomial -> Polynomial
;; Multiplica dos polinomios (no necesariamente normalizados)
(define (multPoly p1 p2)
  (match p1
    [(nullp) p1]
    [(plus c1 e1 r1) (match p2
                       [(nullp) p2]
                       [(plus c2 e2 r2) (sumaPoly (mapPoly (λ(c m) (cons (* c c1) (+ m e1))) p2) (multPoly r1 p2))] ;;usar mapPoly para multiplicar c1 y e1 a p2 y luego sumar el resultado con multPoly de r1 con p2
                       )
     ]
  )
)




;;;;;;;;;;;;;;;;;;;;;;     EJERICIO 4      ;;;;;;;;;;;;;;;;;;;;;;
;; foldPoly :: A (Number Integer -> A) -> (Polynomial -> A)
(define (foldPoly a f)
  (λ (p)
    (match p
      [(nullp) a]
      [(plus c g r) (f c g ((foldPoly a f) r))])))


;; evalPoly :: Number -> (Polynomial -> Number)
;; Evalua un polinomio en un valor dado
(define (evalPoly v)

  (foldPoly 0 ( λ(c g k) (+ (* c (expt v g)) k)))
  )







