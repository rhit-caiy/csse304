#lang racket

(provide choose sum-of-squares range my-set? union more-positives? nearest-pair)

(define choose
  (lambda (a b)
    
    (cond [(eq? b 0) 1]
          [(eq? b 1) a]
          [else (* (/ a b) (choose (- a 1) (- b 1)))])
    ))

(define sum-of-squares
  (lambda (a)
    (cond [(null? a) 0]
          [else (+ (* (car a) (car a)) (sum-of-squares (cdr a)))]
          )))

(define range
  (lambda (a b)
    (cond [(>= a b) '()]
          [else (cons a (range (+ a 1) b))]
    )))

(define my-set?
  (lambda (a)
    (cond [(null? a) #t]
          [else (and (my-set? (cdr a)) (notcontains (car a) (cdr a)))]
          )))
(define notcontains
  (lambda (a b)
    (cond [(null? b) #t]
          [else (and (notcontains a (cdr b)) (not (equal? a (car b))))])))

(define union
  (lambda (a b)
    (cond [(and (null? a) (null? b)) '()]
          [(not (null? a)) (cons (car a) (union (cdr a) b))]
          [(null? a) (cons (car b) (union a (cdr b)))]
          )))
    
(define more-positives?
  (lambda (lon)
    (> (dif lon) 0)))
(define dif
  (lambda (lon)
    (cond [(null? lon) 0]
          [(> (car lon) 0) (add1 (dif (cdr lon)))]
          [else (sub1 (dif (cdr lon)))])))

(define nearest-pair
  (lambda (lon)
    (sorted-nearest-pair (sort lon <))
    ))
(define sorted-nearest-pair
  (lambda (lon)
    (cond [(null? (cddr lon)) (cons (car lon) (car (cdr lon)))]
          [else (if (< (- (car (cdr lon)) (car lon)) (- (cdr (sorted-nearest-pair (cdr lon))) (car (sorted-nearest-pair (cdr lon)))))
                    (cons (car lon) (car (cdr lon)))
                    (sorted-nearest-pair (cdr lon)))])
    ))

;;--------  Used by the testing mechanism   ------------------

(define-syntax nyi
  (syntax-rules ()
    ([_]
     [error "nyi"])))
