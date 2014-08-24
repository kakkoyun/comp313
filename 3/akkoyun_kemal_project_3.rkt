#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Kemal Akkoyun         ;;  
;; 11076004              ;;
;; Comp 313 - Project 3  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require htdp/testing)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Classwork ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Polynomial : poly
;; --- is a list of numbers that represents coefficients of a polynomial, 
;; ---- stored with lowest order coefficient.


;; eval-poly : poly -> (number->number)
;; Purpose: Evaluate the polynomial for a given value of its single variable.

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
;; Purpose: to add 2 polynomials.

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
;; Purpose: to multiply 2 polynomials.

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

;; roots-to-poly : listOfNumbers -> poly
;; Purpose: creates a poly where roots are given numbers in list of numbers.
;; Directly from Chris Stephenson's lecture notes.

;; Template:
;;(define (roots-to-poly p)
;;  (list ...))

(define (roots-to-poly p)
  (foldr multiply-poly (list 1) 
         (map (λ (r) (list 1 (- r))) p)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Assignments ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A constant, used over and over my guesses start with 1,
(define INITIAL-GUESS 1.0)
;; A constant, an error value for approximations.
(define ERROR 0.000000000000000000000001)
;; A constant, that states an upper limit of iterations.
(define ITERATION-LIMIT 10)

;; is-good-enough? : poly -> (number -> boolean)
;; Purpose : a function that creates a function that compares expected value
;; and error after apllying given procedure.

;; Template :
;;(define (good-enough? poly)
;; (λ ...
;;  (<= (abs (- ... x)))

(define (is-good-enough? p)
  (lambda (x)
    (<= (abs ((eval-poly p) x)) ERROR)))

; Tests:


;; make-fixed-point : poly -> (number -> number)
;; purpose : basically, a generalization over improve-guess function.
;; This a helper function that try to find a fixed point which is P(x)=0.
;; As Chris's stated in lecture:
;; new-guess = guess - f(guess)/f'(guess)
;; In this case, it is trying to find a better approximation for an x which makes
;; a polynomial P(x)=0.

;; Template:
;;(define (make-fixed-point p)
;;  (λ (x)
;;    (... x  ... (p x)
;;       ))

(define (make-fixed-point p)
  (λ (x)
    (- x (/ ((eval-poly p) x)
            ((eval-poly (deriv-poly p)) x))
       )))

;; Tests:
;; Since it is returning a procedure cannot test it without a value.
(check-expect ((make-fixed-point (list 0 1)) 1) 0)
(check-range ((make-fixed-point (list 0 1 1 1)) 1) 0 1)
(check-range ((make-fixed-point (list 0 1 3 4 5)) 1) 0 1)


;; find-fixed-point : p number -> number
;; Purpose : To find a fixed point of a given function f(x)=0.

;; template :
;;(define (find-fixed-point p guess error)
;;  (if ... ...
;;  (find-fixed-point p ...guess error)

(define (find-fixed-point p guess)
  (if
   ;; is it really fixed point?
   ((is-good-enough? p) guess)
   guess
   (find-fixed-point p
                     ;; good guess but not enough, 
                     ;; try to make another fixed-point and try again!
                     ((make-fixed-point p) guess)
                     )))

; Tests:
(check-expect (find-fixed-point (list 0 1) 1) 0)
(check-range (find-fixed-point (list 0 1 2) 1) 0 1)
(check-range (find-fixed-point (list 0 1 2 3) 1) 0 1)
(check-range (find-fixed-point (list 0 5 2 1) 1) 0 1)

;; find-root : (number -> number) -> number
;; Purpose : To find root of a function of we say polynomial.

;; Template :
;;(define (find-root f)
;;  (find-fixed-point ... )

(define (find-root p)
  (find-fixed-point
   p
   INITIAL-GUESS
   ))

;; Tests:
(check-range (find-root (list 0 0 1)) 0 1)
(check-range (find-root (list 0 0 0 1)) -1 1)
(check-range (find-root (list 0 1 1)) 0 1)
(check-range (find-root (list 0 2)) 0 1)
(check-range (find-root (list 5 0 1 1)) -3 -2)

;; find-fixed-point-iter-count : poly number number -> pairOfnumbers
;; In this case number pair represents : (guess numberOfiterations)
;; Purpose : To find a fixed point of a given function f(x)=0.

;; template :
;;(define (find-fixed-point-iter-count p guess error)
;;  (find-fixed-point p ...guess error))

(define (find-fixed-point-iter-count p guess limit)
  (local
    ((define (inner p guess limit count)
       (cond
         [(<= limit count)(list 0 (add1 limit))]
         [((is-good-enough? p) guess)
          ;; round guesses to be able to test them.
          (list (round guess) count)]
         [else
          (inner p 
                 ((make-fixed-point p) guess)
                 limit
                 (add1 count)
                 )]
         )))
    (inner p guess limit 0)))

; Tests:
(check-expect (find-fixed-point-iter-count (list 0 1) 1 10) (list 0 1))
(check-expect (find-fixed-point-iter-count (list 0 1 2) 1 10) (list 0 8))
(check-expect (find-fixed-point-iter-count (list 0 1 2 3) 1 10) (list 0 9))
(check-expect (find-fixed-point-iter-count (list 0 5 2 1) 1 10) (list 0 6))


;; find-root-with-iteration-count : poly -> pairOfnumbers
;; In this case number pair represents : (guess numberOfiterations)
;; Purpose : To find root of a function of we say polynomial while keeping iter count.

(define (find-root-with-iteration-count p)
  (find-fixed-point-iter-count
   p
   INITIAL-GUESS
   ITERATION-LIMIT
   ))

;; Tests:
;; Test it manualy, because of inexact numbers!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(generate-report)