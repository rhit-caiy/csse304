#lang racket

(provide interval-contains? interval-intersects? interval-union my-first my-second my-third make-vec-from-points dot-product vector-magnitude distance)

(define interval-contains?
  (lambda (a b)
    (and (<= (car a) b) (<= b (car (cdr a)))
    )))

(define interval-intersects?
  (lambda (a b)
    (or (and (<= (car a) (car (cdr b)))
             (>= (car (cdr a)) (car (cdr b))))
        (and (<= (car b) (car (cdr a)))
             (>= (car (cdr b)) (car (cdr a))))
        )))

(define interval-union
  (lambda (a b)
    (if (< (car (cdr a)) (car (cdr b)))
        (if (< (car (cdr a)) (car b))
            (list a b)
            (if (< (car a) (car b))
                   (list (list (car a) (car (cdr b))))
                   (list b)))
        (if (= (car (cdr a)) (car (cdr b)))
            (if (< (car a) (car b))
                (list (list (car a) (car (cdr b))))
                (list (list (car b) (car (cdr b)))))
            (if (< (car (cdr b)) (car a))
                (list a b)
                (if (< (car a) (car b))
                    (list a)
                    (list (list (car b) (car (cdr a))))
                    )))
        )))

(define my-first
  (lambda (a)
    (car a)))

(define my-second
  (lambda (a)
    (car (cdr a))))

(define my-third
  (lambda (a)
    (car (cddr a))))

(define make-vec-from-points
  (lambda (a b)
    (list (- (my-first b) (my-first a)) (- (my-second b) (my-second a)) (- (my-third b) (my-third a)))
    ))

(define dot-product
  (lambda (a b)
    (+ (* (my-first b) (my-first a))
          (* (my-second b) (my-second a))
          (* (my-third b) (my-third a))
            )))

(define vector-magnitude
  (lambda (a)
    (sqrt (+ (* (my-first a) (my-first a)) (* (my-second a) (my-second a)) (* (my-third a) (my-third a))))))

(define distance
  (lambda (a b)
    (vector-magnitude (make-vec-from-points a b))))

;;--------  Used by the testing mechanism   ------------------

(define-syntax nyi
  (syntax-rules ()
    ([_]
     [error "nyi"])))
