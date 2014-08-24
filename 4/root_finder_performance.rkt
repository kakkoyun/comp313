#lang racket
;; Author : Kemal Akkoyun
;; Student ID : 11076004
;; Source : Structure and Interpretation Of Computer Programs - MIT Press
;;          Christopher J. K. Stephenson's Comp 313 Lectures.
;; Information : Comp313 - Project 4

;; BECAUSE OF MESS IN MY OTHER LIBRARIES, THIS IS DIRECT AND BASIC IMPLEMENTATION OF ROOT FINDER ADDED.

(define ERROR 0.000001)
(define DX 0.001)
(define LIMIT 1000)

(define (poly->func c)
  (lambda (x)
    (let* ((degrees (reverse (build-list (length c) (λ (x) x)))))
      (foldr + 0 (map (lambda (degree c) (* (expt x degree) c)) degrees c)))))

(define (find-root guess good-enough? improve counter)
  (cond
    ((>= counter LIMIT) (list 0 (add1 LIMIT)))
    ((good-enough? guess) (list guess counter))
    (else
     (find-root (improve guess) good-enough? improve (add1 counter)))))

(define (derivative f)(lambda (x)(/ (- (f (+ x DX)) (f x))DX)))
(define (is-good-enough? f)(lambda (x)(< (abs (magnitude (f x))) ERROR)))
(define (improve f)(lambda (g)(- g (/ (f g) ((derivative f) g)))))

(define (root-find f initial-guess error iter-limit)
  (begin
    (set! ERROR error)
    (set! LIMIT iter-limit)
    (find-root initial-guess
               (is-good-enough? f)
               (improve f)
               0)))

(provide root-find)
(provide poly->func)