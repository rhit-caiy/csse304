#lang racket

(provide sn-list-recur sn-list-sum sn-list-map sn-list-paren-count sn-list-reverse sn-list-occur sn-list-depth bt-recur bt-sum bt-inorder)

;a: base value, b: list proc, c: symbol or number proc
(define sn-list-recur
  (lambda (a b c)
    (letrec [(helper (lambda (ls)
                       (cond [(null? ls) a]
                             [(list? (car ls)) (b (helper (car ls)) (helper (cdr ls)))]
                             [else (b (c (car ls)) (helper (cdr ls)))])))]
      helper)))

(define sn-list-sum
  (lambda (a)
    ((sn-list-recur 0 + +) a)))

(define sn-list-map
  (lambda (a b)
    ((sn-list-recur '() cons a) b)))

(define sn-list-paren-count
  (lambda (a)
    (+ 2 ((sn-list-recur 0 (lambda (x y) (+ x y 2)) (lambda (x) -2)) a))))

(define sn-list-reverse
  (lambda (a)
    ((sn-list-recur null (lambda (x y) (if (list? x) (cons (reverse x) y) (cons x y))) (lambda (x) x)) (reverse a))))

(define sn-list-occur
  (lambda (a b)
    ((sn-list-recur 0 + (lambda (x) (if (eqv? a x) 1 0))) b)))

(define sn-list-depth
  (lambda (a)
    ((sn-list-recur 1 (lambda (x y) (max (add1 x) y)) (lambda (x) 0)) a)))

(define bt-recur
  (lambda (a b)
    (letrec [(helper (lambda (t)
                       (if (list? t)
                           (b (car t) (helper (cadr t)) (helper (caddr t)))
                           (a t))))]
      helper)))

(define bt-sum
  (lambda (a)
    ((bt-recur (lambda (x) (if (number? x) x 0)) (lambda (x y z) (+ y z))) a)))

(define bt-inorder
  (lambda (a)
    (flatten ((bt-recur (lambda (x) null) (lambda (x y z) (append (list y) (list x) (list z)))) a))))

;;--------  Used by the testing mechanism   ------------------

(define-syntax nyi
  (syntax-rules ()
    ([_]
     [error "nyi"])))
