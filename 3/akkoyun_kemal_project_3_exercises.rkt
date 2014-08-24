#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Kemal Akkoyun         ;;  
;; 11076004              ;;
;; Comp 313 - Project 3  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require htdp/testing)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Exercise 2.33 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; accumulate : (x -> y) y listofX -> y
;; Purpose: An accumulate function for list operations.
;; Template:
;(define (accumulate op initial sequence)
;  (cond
;    [(empty? sequence) ...]
;    [else (accumulater ...op ...initial ...sequence]
;    ))

(define (accumulate op initial sequence)
  (cond
    [(empty? sequence) initial]
    [else (op (first sequence)
              (accumulate op initial (rest sequence)))]
    ))

;; Tests:
(check-expect (accumulate + 0 (list 1 2 3 4 5)) 15)
(check-expect (accumulate * 1 (list 1 2 3 4 5)) 120)
(check-expect (accumulate cons empty (list 1 2 3 4 5)) (list 1 2 3 4 5))

;; acc-map : (x -> y) listOfX -> listOfY
;; Purpose: a map implemetation over a list with a procedure p.
;; Template:
;(define (map p sequence)
;  (accumulate (λ (x y) (...) empty sequence))
(define (acc-map p sequence)
  (accumulate (λ (x y) (cons (p x) y)) empty sequence))

; Tests:
(check-expect (acc-map (λ (x) (* x x)) (list 1 2 3 4 5)) (list 1 4 9 16 25))
(check-expect (acc-map (λ (x) (+ x 5)) (list 1 2 3 4 5)) (list 6 7 8 9 10))
(check-expect (acc-map (λ (x) (* x x x)) (list 1 2 3 4 5)) (list 1 8 27 64 125))
(check-expect (acc-map (λ (x) 5) (list 1 2 3 4 5)) (list 5 5 5 5 5))

;; acc-append : listOFx listOfx -> listOfx
;; Purpose: a append implemetation of given two lists.
;; Template:
;(define (append seq1 seq2)
;  (accumulate cons ... ...)
(define (acc-append seq1 seq2)
  (accumulate cons seq2 seq1))
; Tests:
(check-expect (acc-append (list 1 2) (list 3 4 5)) (list 1 2 3 4 5))
(check-expect (acc-append (list 'k 'e) (list 'm 'a 'l)) (list 'k 'e 'm 'a 'l))

;; acc-length : listOfX -> number
;; Purpose: a function to find length of a number.
;; Template:
;(define (length sequence)
;  (accumulate ... 0 sequence))
(define (acc-length sequence)
  (accumulate (λ (x y)(add1 y)) 0 sequence))

; Tests:
(check-expect (acc-length (list 1 2 3 4 5)) 5)
(check-expect (acc-length empty) 0)
(check-expect (acc-length (list 'k 'e 'm 'a 'l)) 5)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Exercise 2.36 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; accumulate-n: (x -> y) y listOf(listOfx) -> listOfy
;; Purpose: To combine first elemets of lists and second elements of list and so on.
;; Returns a sequnece of results.
;; Example:
;; '('( 1 2) '( 2 3) '( 3 4)) -> '(6 9)

;; Template:
;(define (accumulate-n op initial sequence)
;  (cond
;    [(empty? sequence) ...]
;    [else (accumulate-n ...op ...initial ...sequence]
;    ))

(define (accumulate-n op initial sequence)
  (cond
    [(empty? (first sequence)) empty]
    [else (cons
           (accumulate op initial (acc-map first sequence))
           (accumulate-n op initial (acc-map rest sequence)))]
    ))

;; Tests:
(check-expect (accumulate-n + 0 (list (list 1 2)(list 2 3) (list 3 4)))(list 6 9))
(check-expect (accumulate-n + 0 (list (list 1 2 3)(list 4 5 6) (list 7 8 9) (list 10 11 12)))
              (list 22 26 30))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Exercise 2.37 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data definition:
;; A matrix is a list of list of numbers.
;; either empty or
;; (cons (cons ... ...) matrix)

;; dot-product : matrix matrix -> number
;; Purpose: to calculate dot product of given two matrix.
;; Template:
;(define (dot-product v w)
;  (accumulate + 0 (map * v w)))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

;; Test:
(check-expect (dot-product (list 1 2)(list 2 3)) 8)
(check-expect (dot-product (list 3 2)(list 4 3)) 18)
(check-expect (dot-product (list 1 )(list 2)) 2)
(check-expect (dot-product (list 0)(list 2)) 0)

;; matrix-*-vector : matrix vector -> number
;; Purpose: to calculate product of a vector and a matrix.
;; Template:
;(define (matrix-*-vector m v)
;  (map ... m)

(define (matrix-*-vector m v)
  (map (λ (x) (dot-product x v)) m))

;; Test:
(check-expect (matrix-*-vector (list (list 1 2)(list 2 3) (list 3 4))(list 6 9))(list 24 39 54))
(check-expect (matrix-*-vector (list (list 1 2 3)(list 4 5 6) (list 7 8 9)(list 10 11 12)) (list 24 39 54))
              (list 264 615 966 1317))
(check-expect (matrix-*-vector (list (list 1 2 3)(list 4 5 6) (list 7 8 9)(list 10 11 12)) (list 0 0 0))
              '(0 0 0 0))

;; transpose : matrix matrix -> number
;; Purpose: to find transpose of given matrix.
;; Template:
;(define (transpose m)
;  (accumulate-n ... ... m))

(define (transpose m)
  (accumulate-n cons empty m))

;; Test:
(check-expect (transpose (list (list 1 2)(list 2 3) (list 3 4)))(list (list 1 2 3) (list 2 3 4)))
(check-expect (transpose (list (list 1 2 3)(list 4 5 6)(list 7 8 9)(list 10 11 12)))
              (list (list 1 4 7 10) (list 2 5 8 11) (list 3 6 9 12)))

;; matrix-*-matrix : matrix matrix -> number
;; Purpose: to calculate product of given two matrix.
;; Template:
;(define (matrix-*-matrix m n)
;  (let ((cols (transpose n)))
;    map ... m))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (λ (x) (matrix-*-vector cols x)) m)))

;; Test:
(check-expect 
 (matrix-*-matrix (list (list 1 2)(list 2 3)(list 3 4))
                  (list (list 1 2 3) (list 2 3 4))) (list (list 5 8 11) (list 8 13 18) (list 11 18 25)))

(check-expect (matrix-*-matrix 
               (list (list 1 2 3)(list 4 5 6)(list 7 8 9)(list 10 11 12))
               (list (list 1 4 7 10) (list 2 5 8 11) (list 3 6 9 12))) 
              (list (list 14 32 50 68) (list 32 77 122 167) (list 50 122 194 266) (list 68 167 266 365)))


(generate-report)