#lang racket

(provide matrix-ref matrix? matrix-transpose filter-in invert pascal-triangle)

(define matrix-ref
  (lambda (a b c)
    (list-ref (list-ref a b) c)))

(define matrix?
  (lambda (a)
    (cond [(not (list? a)) #f]
          [(not (list? (car a))) #f]
          [(not (list? (cdr a))) #f]
          [(equal? (car a) '()) #f]
          [(null? (cdr a)) #t]
          [else (and (= (size (car a)) (size (cadr a))) (matrix? (cdr a)) (allnumber (car a)) (allnumber (cadr a)))])))

(define size
  (lambda (a)
    (cond [(null? a) 0]
          [(not (list? a)) 0]
          [else (add1 (size (cdr a)))])
    ))
(define allnumber
  (lambda (a)
    (cond [(null? a) #t]
          [else (and (number? (car a)) (allnumber (cdr a)))])
    ))


(define matrix-transpose
  (lambda (a)
    (cond [(null? a) '()]
          [(null? (car a)) '()]
          [else (cons (column-to-row a) (matrix-transpose (reduce-first-column a)))])))
(define column-to-row
  (lambda (a)
    (cond [(null? a) null]
          [(null? (car a)) null]
          [else (cons (car (car a)) (column-to-row (cdr a)))])))
(define reduce-first-column
  (lambda (a)
    (cond [(null? a) '()]
          [(null? (car a)) '()]
          [else (cons (cdr (car a)) (reduce-first-column (cdr a)))])))


(define filter-in
  (lambda (a b)
    (cond [(null? b) '()]
          [(a (car b)) (cons (car b) (filter-in a (cdr b)))]
          [else (filter-in a (cdr b))])))

(define invert
  (lambda (a)
    (cond [(null? a) '()]
          [else (cons (reverse (car a)) (invert (cdr a)))])))
(define reverse
  (lambda (a)
    (cond [(null? (cdr a)) (car a)]
          [else (cons (reverse (cdr a)) (list (car a)))])))

(define pascal-triangle
  (lambda (a)
    (cond [(< a 0) '()]
          [(= a 0) (list (list 1))]
          [else (cons (pascal-helper a 0) (pascal-triangle (- a 1)))])))

(define pascal-helper
  (lambda (a b)
    (cond [(= a b) (list 1)]
          [(< b a) (cons (choose a b) (pascal-helper a (+ b 1)))]
          )))
(define choose
  (lambda (a b)
    (cond [(= b 0) 1]
          [(= b 1) a]
          [else (* (/ a b) (choose (- a 1) (- b 1)))])
    ))

;;--------  Used by the testing mechanism   ------------------

(define-syntax nyi
  (syntax-rules ()
    ([_]
     [error "nyi"])))
