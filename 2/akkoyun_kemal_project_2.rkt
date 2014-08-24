#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Kemal Akkoyun         ;;  
;; 11076004              ;;
;; Comp 313 - Project 2  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require htdp/testing)
;; Imported because I need a check-range function.
(require lang/htdp-intermediate-lambda)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;; NEWTON'S METHOD for ROOT FIND ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Following code based on Chris Stephenson's lecture notes and SICP book.

;; sqrt-iter : number number number -> number
;; purpose : a helper function which tries guesses iteratively to find sqrt.
(define (sqrt-iter guess x error)
  (if (good-enough? guess x error)
      guess
      (sqrt-iter (improve-guess guess x)
                 x
                 error)))

;; average : number number -> number
;; purpose : a funtion to find average of given 2 number.
(define (average x y)
  (/ (+ x y) 2))

;; square : number number -> number
;; purpose : a function produce square of a number
(define (square x)
  (* x x))

;; improve-guess : number number -> number
;; purpose : a funtion attempt to find better guess.
(define (improve-guess guess x)
  (average guess (/ x guess)))

;; good-enough? : number number number -> boolean
;; purpose : a function to check whether given guess is good enough or not.
(define (good-enough? guess x error)
  (<= (abs (- (square guess) x)) error))

;; sqrt : number -> number
;; purpose : a function to find square root of given number.

(define (newtons-sqrt x)
  (sqrt-iter 1.0 x 000000000000000000000000000000000000000000000000000000000000.1))

;; Tests:
(check-range (newtons-sqrt 0) 0 1)
(check-range (newtons-sqrt 2) 1 2)
(check-range (newtons-sqrt 4) 2 3)
(check-range (newtons-sqrt 9) 3 4)
(check-range (newtons-sqrt 5) 2 3)
(check-range (newtons-sqrt 9) 3 4)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;; Assignment & Classwork ;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; is-good-enough? : (number -> number) -> (number -> boolean)
;; Purpose : a function that creates a function that compares expected value
;; and error after apllying given procedure.

;; Template :
;;(define (good-enough? ... error)
;; (λ ...
;;  (<= (abs (- ... x)) error))

(define (is-good-enough? f error)
  (lambda (x)
    (<= (abs (f x)) error)))


;; A constant, an epsilon value to use at approximations.
(define DX 000000000000.1)

;; deriv : (number -> number) -> (number -> number) 
;; purpose: a function to calculate derivative of a given function

;; Template:
;;(define (deriv f)
;;  (lambda (x)
;;    (...f...)
;;          ))

(define (deriv f)
  (λ (x)
    (/ (- (f (+ x DX)) (f x))
       DX)))


;; make-fixed-point : (number -> number) -> (number -> number)
;; purpose : basically, a generalization over improve-guess function.
;; This a helper function that try to find a fixed point which is f(x)=0.
;; As Chris's stated in lecture:
;; new-guess = guess - f(guess)/f'(guess)
;; In this case, it is trying to find a better approximation for an x which makes
;; f(x)=0.

;; Template:
;;(define (make-fixed-point f)
;;  (λ (x)
;;    (... x  ... (f x)
;;       ))

(define (make-fixed-point f)
  (λ (x)
    (- x (/ (f x)
            ((deriv f) x))
       )))

;; find-fixed-point : (number -> number) number number -> number
;; Purpose : To find a fixed point of a given function f(x)=0.

;; template :
;;(define (find-fixed-point f guess error)
;;  (if ... ...
;;  (find-fixed-point f ...guess error)

(define (find-fixed-point f guess error)
  (if
   ;; is it really fixed point?
   ((is-good-enough? f error) guess)
   guess
   (find-fixed-point f 
                     ;; good guess but not enough, 
                     ;; try to make another fixed-point and try again!
                     ((make-fixed-point f) guess)
                     error
                     )
   ))

;; A constant, used over and over my guesses start with 1,
(define INITIAL-GUESS 1.0)
;; A constant, an error value for approximations.
(define ERROR 0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000.1)
;; A constant, that states an upper limit of iterations.
(define ITERATION-LIMIT 30)

;; find-root : (number -> number) -> number
;; Purpose : To find root of a function of we say polynomial.

;; Template :
;;(define (find-root f)
;;  (find-fixed-point ... )

(define (find-root f)
  (find-fixed-point
   (make-fixed-point f)
   INITIAL-GUESS
   ERROR
   ))

;; Tests:
(check-range (find-root (λ (x) (* x x))) 0 1)
(check-range (find-root (λ (x) (* x x x))) -1 1)
(check-range (find-root (λ (x) (+ x (* x x)))) 0 1)
(check-range (find-root (λ (x) (* x 2))) 0 1)
(check-range (find-root (λ (x) (+ 5 (* x (+ x (* x x))))))  1 2)



;; find-fixed-point-iter-count : (number -> number) number number number -> pairOfnumbers
;; In this case number pair represents : (guess numberOfiterations)
;; Purpose : To find a fixed point of a given function f(x)=0.

;; template :
;;(define (find-fixed-point-iter-count f guess error)
;;  (find-fixed-point f ...guess error))

(define (find-fixed-point-iter-count f guess error limit)
  (local
    ((define (inner f guess error limit count)
       (cond
         [(<= limit 0)(list 0 (add1 limit))]
         [((is-good-enough? f error) guess)
          (list guess count)]
         [else
          (inner f 
                 ((make-fixed-point f) guess)
                 error
                 limit
                 (add1 count)
                 )]
         )))
    (inner f guess error limit 0)))

;; find-root-with-iteration-count : (number -> number) -> pairOfnumbers
;; In this case number pair represents : (guess numberOfiterations)
;; Purpose : To find root of a function of we say polynomial while keeping iter count.

(define (find-root-with-iteration-count f)
  (find-fixed-point-iter-count
   (make-fixed-point f)
   INITIAL-GUESS
   ERROR
   ITERATION-LIMIT
   ))

;; Tests:
;; Test it manualy, because of inexact numbers!

;; multiple-root-find: (number -> number) number -> listof listOfnumbers
;; Purpose : to find multiple roots of a polynomial by trying multiple guesses. 

;; Template:
;(define (multiple-root-find f guess-count)
;  (map
;  ....(find-fixed-point-iter-count f guess error limit)))
; ...)

(define (multiple-root-find f guess-count)
  (map
   (λ (guess)
     (cons guess (find-fixed-point-iter-count f guess ERROR ITERATION-LIMIT)))
   (create-list-of-guess guess-count)))

;; Tests:
;; Test it manualy, because of inexact numbers!

;; create-list-of-guess : number -> listOfnumbers
;; Purpose: To create a list of guesses
;; Template:
;; (define (create-list-of-guess number)
;;   (build-list...) ...)

(define (create-list-of-guess number)
  (append
   (build-list number (λ (x)  x))
   (build-list number (λ (x) (- x)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(generate-report)