#lang racket

; num-positive - returns the number of positive elements in a list
; implement this in a tail recursive way, similar to fact2
;
; (num-positive '(1 -2 0 100 77)) -> 3

(define num-positive
  (lambda (lon)
    (cond [(null? lon) 0]
          [(> (car lon) 0) (add1 (num-positive (cdr lon)))]
          [else (num-positive (cdr lon))])))

; you'll want a helper function
;(define num-positive-recur
;  ????)


; second largest - returns the second largest element in a list of numbers
;
; (second-largest '( 7 4 5 3 6 2 1)) -> 6
;
; you can assume the list has 2 elements
; implement this in a tail recursive way with a helper function

(define second-largest
  (lambda (lon)
    (cond [(car lon) (car lon)]
          [else (second-largest (cdr lon))])))
(define largerthannum
  (lambda (lon n)
    (nyi)))