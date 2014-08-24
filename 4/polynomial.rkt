#lang racket
(require test-engine/racket-tests)
;; Author : Kemal Akkoyun
;; Student ID : 11076004
;; Source : Structure and Interpretation Of Computer Programs - MIT Press
;;          Christopher J. K. Stephenson's Comp 313 Lectures.
;; Information : Comp313 - Project 4


;; Polynomial : poly
;; --- is represented as list of numbers which are coefficients of a polynomial, 
;; ---- starting with the least significant coefficient.

;; eval-poly : poly -> (number->number)
;; Purpose: Evaluate the polynomial for a given value of its single variable, using
;; Horners rule

;; Examples:
;; '() -> '(λ (x) 0) 
;; '(1) -> '(λ (x) 1) 
;; '(0 1) -> '(λ (x) x) 
;; '(1 2 3) -> '(λ (x) (+ 1 (* 2 x) (* 3 x x))

;; Template:
;;(define (eval-poly p)
;;  (λ (x) (...x ...p)))

(define (eval-poly p)
  (λ (x) (foldr (λ (l r) (+ l (* x r))) 0 p)))


(check-expect ((eval-poly empty) 5) 0)
(check-expect ((eval-poly (list 1)) 5) 1)
(check-expect ((eval-poly (list 0 1)) 5) 5)
(check-expect ((eval-poly (list 1 2 3)) 5) 86)

;; deriv-poly : poly -> poly
;; Purpose: To find derivative of a polynomial.

;; Examples:
;; '(1) -> '(0)
;; '(0 1) -> '(1)
;; '(1 2 3) -> '(2 6)
;; '(1 2 3 4) -> '(2 6 12)

;; Template:
;;(define (deriv-poly p)
;;  (list ...))

(define (deriv-poly p)
  (if (= (length p) 1)
      (list 0)
      (rest (map * (build-list (length p) values) p))
      ))

;; Test:
(check-expect (deriv-poly (list 1)) (list 0))
(check-expect (deriv-poly (list 0 1)) (list 1))
(check-expect (deriv-poly (list 1 2 3)) (list 2 6))
(check-expect (deriv-poly (list 1 2 3 4)) (list 2 6 12))
(check-expect (deriv-poly (list 2 3 2 6 5))(list 3 4 18 20))

;; add-poly : poly poly -> poly
;; Purpose: Add two polynomials together, allowing for polynomials
;; of different length.

;; Template:
;;(define (add-poly p1 p2)
;;  (list ...))

(define (add-poly p1 p2)
  (cond
    [(empty? p1) p2]
    [(empty? p2) p1]
    [else
     (cons (+ (first p1) (first p2))
           (add-poly (rest p1) (rest p2)))]
    ))

;; Test:
(check-expect (add-poly (list 1 1) (list 1 1)) (list 2 2))
(check-expect (add-poly (list 1) (list 1 1 1)) (list 2 1 1))
(check-expect (add-poly (list 1 1) (list 1 )) (list 2 1))
(check-expect (add-poly (list 1 1 1 1 1 1) (list 1 1)) (list 2 2 1 1 1 1))


;; repeat : (list -> list) number list -> list
;; in this case : (poly -> poly) degree poly -> poly
;; Purpose: to repeat list function to a list with given times.

; Template:
;(define (repeat f number list)
;  (cond
;  [(= number 0) ...]
;  [else (repeat (sub1 number) ...f ...list)]))

(define (repeat f number list)
  (cond
    [(= number 0) list]
    [else (repeat f (sub1 number) (f list))]
    ))

; Tests:
(check-expect (repeat (curry cons 0) 2 empty) (list 0 0))
(check-expect (repeat (curry cons 1) 2 empty) (list 1 1))
(check-expect (repeat (curry map (curry + 2)) 2 (list 0 0)) (list 4 4))

;; multiply-poly : poly poly -> poly
;; Purpose: multiply two polynomials together, allowing for polynomials
;; of different length

;(define (multiply-poly p1 p2)
;  (local 
;    ((define (inner-mulpoly p1 p2 acc)
;       (cond
;       [(empty? p1) acc]
;       [(empty? p2) empty]
;       [else (inner-mul ...)]
;         )))))

(define (multiply-poly p1 p2)
  (local
    ((define (inner-mul p1 p2 degree acc)
       (cond
         [(empty? p1) acc]
         [(empty? p2) empty]
         [else
          (inner-mul (rest p1) p2 (add1 degree) 
                     (add-poly
                      (repeat (curry cons 0) degree (map (curry * (first p1)) p2))
                      acc))])))
    (inner-mul p1 p2 0 empty)))

;; Tests:
(check-expect (multiply-poly (list 1 2) (list 2 3)) (list 2 7 6))
(check-expect (multiply-poly (list 2 7 6) (list 1 2)) (list 2 11 20 12))
(check-expect (multiply-poly (list 1) (list 2 7 6)) (list 2 7 6))

;; poly-multiply: poly poly -> poly
;; Purpose: multiply two polynomials together, allowing for polynomials
;; of different length

(define (poly-multiply p1 p2)
  (foldr (lambda (l r) (add-poly (map (curry * l) p1) (cons 0 r))) empty
         p2))

;; Tests:
(check-expect (poly-multiply (list 1 2) (list 2 3)) (list 2 7 6))
(check-expect (poly-multiply (list 2 7 6) (list 1 2)) (list 2 11 20 12))
(check-expect (poly-multiply (list 1) (list 2 7 6)) (list 2 7 6))

;; roots-to-poly : listOfNumbers -> poly
;; Purpose: creates a poly where roots are given numbers in list of numbers.
;; Directly from Chris Stephenson's lecture notes.

;; Template:
;;(define (roots-to-poly p)
;;  (list ...))

(define (roots-to-poly p)
  (foldr multiply-poly (list 1) 
         (map (λ (r) (list 1 (- r))) p)))

; Tests:
(check-expect (roots-to-poly (list 2 6)) (list 1 -8 12))
(check-expect (roots-to-poly (list -1 -2)) (list 1 3 2))
(check-expect (roots-to-poly (list 1 2)) (list 1 -3 2))
(check-expect (roots-to-poly (list 1 2 3)) (list 1  -6 11 -6))
(check-expect (roots-to-poly (list 1)) (list 1 -1))
(check-expect (roots-to-poly (list 3 -2 5 -4 3 6)) (list 1 -11 11 215 -564 -612 2160))


;; make a degree n polynomial with coeffs from 1 to k
(define (make-poly n k) (build-list n (lambda (x) (add1 (random k)))))

;; make m degree n polynomials with coeffs up to k
(define (make-polys m n k) (build-list m (lambda (x) (make-poly n k))))

(provide roots-to-poly)
(provide make-poly)
(provide make-polys)
(provide eval-poly)
(provide deriv-poly)
(test)