#lang racket

(provide minimize-interval-list exists? product replace remove-last)

(define minimize-interval-list
  (lambda (a)
    (sorted-mil (sort a #:key  car <))))
(define sorted-mil
  (lambda (a)
    (cond [(null? a) '()]
          [(null? (car a)) '()]
          [(null? (cdr a)) (list (car a))]
          [(>= (cadar a) (cadadr a)) (sorted-mil (cons (car a) (cddr a)))]
          [(>= (cadar a) (caadr a)) (sorted-mil (cons (list (caar a) (cadadr a)) (cddr a)))]
          [else (cons (car a) (sorted-mil (cdr a)))])))

(define exists?
  (lambda (a b)
    (cond [(null? b) #f]
          [else (or (a (car b)) (exists? a (cdr b)))])))

(define product
  (lambda (a b)
    (cond [(null? a) null]
          [(null? b) null]
          [else (append (product-line (car a) b) (product (cdr a) b))])))
(define product-line
  (lambda (a b)
    (cond [(null? (cdr b)) (list (list a (car b)))]
          [else (cons (list a (car b)) (product-line a (cdr b)))])))

(define replace
  (lambda (a b c)
    (cond [(null? c) '()]
          [(equal? (car c) a) (cons b (replace a b (cdr c)))]
          [else (cons (car c) (replace a b (cdr c)))])))

(define remove-last
  (lambda (a b)
    (cond [(not (member a b)) b]
          [else (reverse (remove-first a (reverse b)))]
          )))
(define remove-first
  (lambda (a b)
    (cond [(equal? a (car b)) (cdr b)]
          [else (cons (car b) (remove-first a (cdr b)))]
          )))

;;--------  Used by the testing mechanism   ------------------

(define-syntax nyi
  (syntax-rules ()
    ([_]
     [error "nyi"])))
