#lang racket

(provide intersection subset? relation? domain reflexive? multi-set? ms-size last all-but-last)

(define intersection
  (lambda (a b)
    (cond [(and (not (null? a)) (not (null? b))) (if (exist (car a) b)
                                                     (cons (car a) (intersection (cdr a) b))
                                                     (intersection (cdr a) b))]
          [else '()])))
(define exist
  (lambda (a b)
    (cond [(null? b) #f]
          [(equal? a (car b)) #t]
          [else (exist a (cdr b))]
    )
    ))

(define subset?
  (lambda (a b)
    (cond [(null? a) #t]
          [else (and (exist (car a) b) (subset? (cdr a) b))])))

(define relation?
  (lambda (a)
    (cond [(null? a) #t]
          [(not (list? a)) #f]
          [(not (list? (car a))) #f]
          [else (and (not (exist (car a) (cdr a))) (= (size (car a)) 2) (relation? (cdr a)))]
          )))
(define size
  (lambda (a)
    (cond [(null? a) 0]
          [(not (list? a)) 0]
          [else (add1 (size (cdr a)))])
    ))

(define domain
  (lambda (a)
    (cond [(null? a) '()]
          [else (cons (car (car a)) (domain (cdr a)))])))

(define range
  (lambda (a)
    (cond [(null? a) '()]
          [else (cons (car (cdr (car a))) (range (cdr a)))]
          )))

(define reflexive?
  (lambda (a)
    (cond [(null? a) #t]
          [else (reflexivehelp (union (domain a) (range a)) a)])
    ))
(define union
  (lambda (a b)
    (cond [(and (null? a) (null? b)) '()]
          [(not (null? a)) (cons (car a) (union (cdr a) b))]
          [(null? a) (cons (car b) (union a (cdr b)))]
          )))
(define reflexivehelp
  (lambda (a b)
    (cond [(null? a) #t]
          [else (and (reflexivehelp (cdr a) b) (exist (list (car a) (car a)) b))]
          )))

(define multi-set?
  (lambda (a)
    (cond [(null? a) #t]
          [(not (uniquedomain a)) #f]
          [(not (list? a)) #f]
          [else (and (multi-set? (cdr a)) (properpair (car a)) (uniquedomain a))])))

(define properpair
  (lambda (a)
    (and (= (size a) 2) (integer? (car (cdr a))) (> (car (cdr a)) 0) (not (integer? (car a)))
    )
    ))
(define uniquedomain
  (lambda (a)
    (cond [(null? a) #t]
          [(and (list? a) (= (size (car a)) 2) (uniquedomain (cdr a)))
           (not (exist (car (car a)) (domain (cdr a))))]
          [else #f])
    )
    )


(define ms-size
  (lambda (a)
    (cond [(null? a) 0]
          [else (+ (car (cdr (car a))) (ms-size (cdr a)))])
    ))

(define last
  (lambda (a)
    (cond [(null? (cdr a)) (car a)]
          [else (last (cdr a))])))

(define all-but-last
  (lambda (a)
    (cond [(null? (cdr a)) '()]
          [else (cons (car a) (all-but-last (cdr a)))])))

;;--------  Used by the testing mechanism   ------------------

(define-syntax nyi
  (syntax-rules ()
    ([_]
     [error "nyi"])))
