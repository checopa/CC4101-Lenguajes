#lang play

(require "base.rkt")

(print-only-errors #t)


;; Agregue aqui todos sus tests

;;;;;;;;;;;;;;;;;;;;;;     EJERICIO 1      ;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;PARTE A;;;;;;;

;; Primera funcion auxiliar que revisa la primera condicion
(test (mayorcoef (plus 4 5 (plus 7 5 (nullp)))) #f)
(test (mayorcoef (plus 3 2 (plus 4 5 (plus 5 0 (nullp))))) #f)
(test (mayorcoef (nullp)) #t)

;; Segunda funcion auxiliar que revisa la segunda condicion
(test (coefcero (plus 0 10 (plus 4 5 (plus 0 3 (plus 3 2 (plus 5 0 (nullp))))))) #f)
(test (coefcero (plus 2 5 (plus 2 6 (nullp)))) #t)
(test (coefcero (nullp)) #t)

;; Verifica si un polinomio esta en forma normal
(test (nf? (plus 3 2 (plus 4 5 (plus 5 0 (nullp))))) #f)
(test (nf? (plus 4 5 (plus 3 2 (plus 5 0 (nullp))))) #t)
(test (nf? (nullp)) #t)


;;;;;;;PARTE B;;;;;;;
;; Test para sumaMon
(test (sumaMon 6 6 (plus 4 4 (plus 2 2 (nullp)))) (plus 6 6 (plus 4 4 (plus 2 2 (nullp)))))
(test (sumaMon 3 3 (plus 4 4 (plus 2 2 (nullp)))) (plus 4 4 (plus 3 3 (plus 2 2 (nullp)))))
(test (sumaMon 10 2 (plus 4 4 (plus 2 2 (nullp)))) (plus 4 4 (plus 12 2 (nullp))))
(test (sumaMon -2 2 (plus 4 4 (plus 2 2 (nullp)))) (plus 4 4 (nullp)))
(test (sumaMon 0 5 (nullp)) (nullp))

;; Test para normalize
(test  (normalize (plus 4 5 (plus 8 10 (plus 0 8 (plus 7 10 (plus 2 7 (nullp))))))) (plus 15 10 (plus 2 7 (plus 4 5 (nullp)))))
(test (normalize (nullp)) (nullp))
;;;;;;;;;;;;;;;;;;;;;;     EJERICIO 2      ;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;PARTE A;;;;;;;

;; Test para degree
(test (degree (plus 4 4 (plus 5 5 (plus 1 1 (nullp))))) 5)
(test/exn (degree (nullp)) "El polinomio nulo no tiene grado")


;;;;;;;PARTE B;;;;;;;

;; Test para coefficient
(test (coefficient 5 (plus 2 1 (plus 5 5 (plus 1 1 (nullp))))) 5)
(test (coefficient 1 (plus 2 1 (plus 5 5 (plus 1 1 (nullp))))) 3)
(test (coefficient 10 (plus 2 1 (plus 5 5 (plus 1 1 (nullp))))) 0)
(test (coefficient 5 (nullp)) 0)


;;;;;;;;;;;;;;;;;;;;;;     EJERICIO 3      ;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;PARTE A;;;;;;;

;; Test para sumaPoly

(test (sumaPoly (plus 0 8 (plus 7 10 (plus 2 7 (nullp)))) (plus 4 5 (plus 8 10 (nullp)))) (plus 15 10 (plus 2 7 (plus 4 5 (nullp)))))
(test (sumaPoly (nullp) (nullp)) (nullp))
(test (sumaPoly (plus 2 2 (nullp)) (plus -2 2 (nullp))) (nullp))

;;;;;;;PARTE B;;;;;;;

;; Test mapPoly

(test (mapPoly (λ(c m) (cons (* c 2) (+ m 1))) (plus 4 5 (plus 3 2 (plus 5 0 (nullp))))) (plus 8 6 (plus 6 3 (plus 10 1 (nullp)))))

;; Test para multPoly

(test (multPoly (plus -3 0 (plus 2 2 (nullp))) (plus -3 2 (plus 4 1 (plus 2 3 (nullp))))) (plus 4 5 (plus -6 4 (plus 2 3 (plus 9 2 (plus -12 1 (nullp)))))))
(test (multPoly (plus 3 5 (plus 2 3 (nullp))) (nullp)) (nullp))
;;;;;;;;;;;;;;;;;;;;;;     EJERICIO 4      ;;;;;;;;;;;;;;;;;;;;;;

;;Test para foldPoly
(test ((foldPoly 2 3) (nullp)) 2)
(test ((foldPoly 0 (λ(a b c) (+ a b c))) (plus 1 1 (plus 2 2 (nullp)))) 6)

;; Test para evalPoly
(test ((evalPoly 3) (plus 2 3 (plus -6 2 (plus 2 1 (plus -1 0 (nullp)))))) 5)
(test ((evalPoly 3) (nullp)) 0)