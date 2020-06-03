#lang play
(require "1_base.rkt")
(print-only-errors #t)


;;Parte 1

(test (emptyT-env) (mtTEnv))

(test (extendT-env 'x (TNum) (emptyT-env)) (anTEnv 'x (TNum) (emptyT-env)))

(test (lookupT-env 'x (anTEnv 'x (TNum) (emptyT-env))) (TNum))

(test/exn (lookupT-env 'x (emptyT-env)) "Exception: free identifier  x")



(test (typeof  (num 3) (emptyT-env)) (list (TNum)))

(reset)

(test (typeof  (id'x) (extendT-env'x (TNum) (emptyT-env))) (list (TNum)))

(reset)

(test (typeof  (add (num 10) (num 3)) (emptyT-env)) ( list (TNum) (Cnst (TNum) (TNum)) (Cnst (TNum) (TNum))))

(reset)

(test (typeof  (add (num 10) (id'x)) (extendT-env'x (TVar 1) (emptyT-env)))( list (TNum) (Cnst (TNum) (TNum)) (Cnst (TVar 1) (TNum))))

(reset)

(test (typeof  (sub (num 10) (num 3)) (emptyT-env)) ( list (TNum) (Cnst (TNum) (TNum)) (Cnst (TNum) (TNum))))

(reset)

(test (typeof  (sub (num 10) (id'x)) (extendT-env'x (TVar 1) (emptyT-env)))( list (TNum) (Cnst (TNum) (TNum)) (Cnst (TVar 1) (TNum))))

(reset)

(test (typeof  (if0 (num 2) (num 5) (num 3)) (emptyT-env)) ( list (TNum) (Cnst (TNum) (TNum)) (Cnst (TNum) (TNum))))

(reset)

(test (typeof  (fun'x (add (id'x) (num 1))) (emptyT-env)) ( list (TFun (TVar 1) (TNum)) (Cnst (TVar 1) (TNum)) (Cnst (TNum) (TNum))))

(reset)

(test (typeof  (app (fun'x (id'x)) (num 3)) (emptyT-env)) ( list (TVar 1) (Cnst (TFun (TVar 2) (TVar 2)) (TFun (TNum) (TVar 1)))))

(reset)

(test (typeof  (fun'f (fun'x (app (id'f) (app (id'f) (id'x))))) (emptyT-env))( list (TFun (TVar 1) (TFun (TVar 2) (TVar 3))) (Cnst (TVar 1) (TFun (TVar 2) (TVar 4))) (Cnst (TVar 1) (TFun (TVar 4) (TVar 3)))))

;;Parte 2

;;Subs aux
(test (subs (TVar 2) (TVar 3) (TVar 2)) (TVar 3))

;;Funciones originales
(test (substitute (TVar 1) (TNum) (list (Cnst (TVar 1) (TVar 2)) (Cnst (TVar 2) (TVar 1)))) (list (Cnst (TNum) (TVar 2)) (Cnst (TVar 2) (TNum))))
(test (substitute (TVar 1) (TNum) (list (Cnst (TFun (TFun (TVar 1) (TVar 1)) (TNum)) (TVar 1)))) (list (Cnst (TFun (TFun (TNum) (TNum)) (TNum)) (TNum))))


(test (occurs-in? (TVar 1) (TNum)) #f)
(test (occurs-in? (TVar 1) (TVar 1)) #t)
(test (occurs-in? (TVar 1) (TFun (TVar 2) (TVar 1) )) #t)



(test (unify ( list (Cnst (TFun (TVar 2) (TVar 2)) (TFun (TNum) (TVar 1))))) ( list (Cnst (TVar 1) (TNum)) (Cnst (TVar 2) (TNum))))
(test (unify ( list (Cnst (TVar 1) (TNum)) (Cnst (TNum) (TNum)))) ( list (Cnst (TVar 1) (TNum))))
(test (unify ( list (Cnst (TVar 1) (TFun (TVar 2) (TVar 4))) (Cnst (TVar 1) (TFun (TVar 4) (TVar 3)))))( list (Cnst (TVar 4) (TVar 3)) (Cnst (TVar 2) (TVar 4)) (Cnst (TVar 1) (TFun (TVar 2) (TVar 4)))))
(test/exn (unify (list (Cnst (TVar 2) (TNum)) (Cnst (TNum) (TNum)) (Cnst (TFun (TVar 2) (TNum)) (TFun (TFun (TVar 3) (TVar 3)) (TVar 1))))) "Type error: cannot unify num with (TFun (TVar 3) (TVar 3)")


;;Parte 3

;;Funcion auxiliar
(test (auxrun (TFun (TVar 2) (TNum)) '() '()) (TFun (TVar 2) (TNum)))

(reset)

(test (auxrun (TVar 2) (list (Cnst (TVar 2) (TVar 4))) (list (Cnst (TVar 2) (TVar 4))) ) (TVar 4))


;;Funcion principal
(test  (runType'(fun (x) (+ x 1))) (TFun (TNum) (TNum)) )

(reset)

(test (runType'(fun (x) x))(TFun (TVar 1) (TVar 1)) )

(reset)

(test  (runType'(fun (x) 3))(TFun (TVar 1) (TNum)) )

(reset)

(test/exn  (runType'x) "Exception: free identifier  x")

(reset)

(test/exn  (runType'((fun (x) (+ x 1)) (fun (x) x))) "Exception: Type error: cannot unify num with (TFun (TVar 3) (TVar 3))" )

(reset)

(test  (runType'(fun (f) (fun (x) (f (f x)))))(TFun (TFun (TVar 3) (TVar 3)) (TFun (TVar 3) (TVar 3))) )