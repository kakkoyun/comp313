#lang racket
(require test-engine/racket-tests)
(require "polynomial.rkt" "root_finder.rkt")

;; Author : Kemal Akkoyun
;; Student ID : 11076004
;; Source : Structure and Interpretation Of Computer Programs - MIT Press
;;          Christopher J. K. Stephenson's Comp 313 Lectures.
;; Information : Comp313 - Project 4

;; is-good-enough-poly? : poly number -> (number -> boolean)
;; Purpose : a function that creates a function that compares expected value
;; and error after applying given procedure.

;; Template :
;;(define (good-enough? poly error)
;; (λ ...
;;  (<= (abs (- ... x)))

(define (is-good-enough-poly?-v1 p error)
  (is-good-enough? (eval-poly p) error))

(define (is-good-enough-poly? p error)
  (lambda (x)
    (let ((guess ((eval-poly p) x))) 
      (if (complex? guess)
          (<= (magnitude guess) error)
          (<= (abs guess) error)
          ))))


; Tests:
(check-expect ((is-good-enough-poly? '(1 1 1) 0.0001) 1) false)
(check-expect ((is-good-enough-poly? '(1 3 2) 0.0001) 4.0) false)


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

(define (find-fixed-point p guess error)
  (if
   ;; is it really fixed point?
   ((is-good-enough-poly? p error) guess)
   guess
   (find-fixed-point p
                     ;; good guess but not enough, 
                     ;; try to make another fixed-point and try again!
                     ((make-fixed-point p) guess)
                     error
                     )))

; Tests:
(check-expect (find-fixed-point (list 0 1) 1 0.0000001) 0)
(check-range (find-fixed-point (list 0 1 2) 1 0.0000001) 0 1)
(check-range (find-fixed-point (list 0 1 2 3) 1 0.0000001) 0 1)
(check-range (find-fixed-point (list 0 5 2 1) 1 0.01) 0 1)

;; find-root : (number -> number) -> number
;; Purpose : To find root of a function of we say polynomial.

;; Template :
;;(define (find-root f)
;;  (find-fixed-point ... )

(define (find-root-poly p guess error)
  (find-fixed-point
   p
   guess
   error
   ))

;;; Tests:
(check-range (find-root-poly (list 0 0 1) 1 0.0001) 0 1)
(check-range (find-root-poly (list 0 0 0 1) 1 0.0001) -1 1)
(check-range (find-root-poly (list 0 1 1) 1 0.001) 0 1)
(check-range (find-root-poly (list 0 2) 1 0.00001) 0 1)
(check-range (find-root-poly (list 5 1 1 1) 1 0.01) -8 0)

;; find-fixed-point-iter-count : poly number number number -> pairOfnumbers
;; In this case number pair represents : (guess numberOfiterations)
;; Purpose : To find a fixed point of a given function f(x)=0.

;; template :
;;(define (find-fixed-point-iter-count p guess error limit)
;;  (find-fixed-point p ...guess error))

(define (find-fixed-point-iter-count p guess error limit)
  (local
    ((define (inner p guess limit count)
       (cond
         [(<= limit count)(list 0 (add1 limit))]
         [((is-good-enough-poly? p error) guess)
          ;; round guesses to be able to test them.
          (if (not (real? guess)) 
              (list guess count)
          (list (round guess) count))]
         [else
          (inner p 
                 ((make-fixed-point p) guess)
                 limit
                 (add1 count)
                 )]
         )))
    (inner p guess limit 0)))

; Tests:
(check-expect (find-fixed-point-iter-count (list 0 0 1) 1 0.0001 10) (list 0 7))
(check-expect (find-fixed-point-iter-count (list 0 0 0 1) 1 0.0001 10) (list 0 8))
(check-expect (find-fixed-point-iter-count (list 0 2) 1 0.00001 10) (list 0 1))
;(check-expect (find-fixed-point-iter-count (list 5 0 1 1) 1 0.000001 11) (list 0 12))


;; find-root-with-iteration-count : poly -> pairOfnumbers
;; In this case number pair represents : (guess numberOfiterations)
;; Purpose : To find root of a function of we say polynomial while keeping iter count.

(define (find-root-with-iteration-count-poly p guess error limit)
  (find-fixed-point-iter-count
   p
   guess
   error
   limit
   ))

;; Tests:
(check-expect (find-root-with-iteration-count-poly (list 0 2) 1 0.00001 10) (list 0 1))
(check-expect (find-root-with-iteration-count-poly (list 0 -2 -1 -3) 1 0.00001 10) (list 0 5))
(check-expect (find-root-with-iteration-count-poly (list 0+5i 1-2i) 1 0.00001 10) (list 2-1i 1))
(check-expect (find-root-with-iteration-count-poly (list 23-5i -1+2i) 1 0.00001 10) (list 33/5+41/5i 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide find-root-with-iteration-count)
(provide find-root-with-iteration-count-poly)
(provide find-root-poly)
(test)