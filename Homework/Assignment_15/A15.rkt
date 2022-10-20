#lang racket
;The Fib in class directly use the value down the recursion to get the value
;The memoize function here need to first calculate through function and insert it into hash table, then look it up or return its value from table after table operation. 
;This might cause it to use more time than direct let. 
(require "../chez-init.rkt")
(provide set-of-cps make-k apply-k 1st-cps map-cps make-cps domain-cps member?-cps andmap-cps free-vars-cps continuation? init-k list-k union-cps remove-cps memoize subst-leftmost)
(require racket/trace)

(define set-of-cps
  (lambda (a b)
    (if (null? a)
        (apply-k b '())
        (set-of-cps (cdr a) (make-k (lambda (c)
                                      (member?-cps (car a) c (lambda (d)
                                                               (if d
                                                                   (apply-k b c)
                                                                   (apply-k b (cons (car a) c))
                                                                   )))))))))

(define make-k
  (lambda (a)
    a))

(define apply-k
  (lambda (a b)
    (a b)))
    ;(if (procedure? a)
    ;    (a b)
    ;    (cases continuation? a
    ;      [init-k () (a b)]
    ;      [list-k () (list (a b))]))))

(define 1st-cps
  (lambda (a b)
    (apply-k b (car a))))

(define map-cps
  (lambda (a b c)
    (if (null? b)
        (apply-k c b)
        (map-cps a (cdr b) (make-k (lambda (d)
                                     (a (car b) (make-k (lambda (e)
                                     (apply-k c (cons e d)))))))))))

(define make-cps
  (lambda (a)
    (lambda (b c)
      (c (a b)))))

;(define domain-cps
;  (lambda (a b)
;    (if (null? a)
;        (apply-k b a)
;        (domain-cps (cdr a) (make-k (lambda (d) (apply-k b (cons (car a) d))))))))
(define 1st car)

;(define set-of ; removes duplicates to make a set
;  (lambda (s)
;    (cond [(null? s) '()]
;          [(member (car s) (cdr s))
;           (set-of (cdr s))]
;          [else (cons (car s)
;                      (set-of (cdr s)))])))
(define domain-cps ; finds the domain of a relation.
  (lambda (a b)
    (if (null? a)
        (apply-k b a)
        (domain-cps (cdr a) (make-k (lambda (d)
                                      (set-of-cps (cons (caar a) d) b)))))))
    ;(set-of (map 1st a))))

(define member?-cps
  (lambda (a b c)
    (cond [(null? b) (apply-k c #f)]
          [(eqv? a (car b)) (apply-k c #t)]
          [else (member?-cps a (cdr b) c)])))

(define andmap-cps
  (lambda (a b c)
    (if (null? b)
        (apply-k c #t)
        (a (car b) (lambda (d)
                     (if d
                         (andmap-cps a (cdr b) c)
                         (apply-k c #f)
                         ))))))

(define 2nd cadr)

(define 3rd caddr)

(define memq-cps
  (lambda (sym ls k)
    (cond [(null? ls)          
	   (apply-k k #f)] 
	  [(eq? (car ls) sym)
	   (apply-k k #t)] 
	  [else (memq-cps sym (cdr ls) k)])
	 ))

(define union ; s1 and s2 are sets of symbols.
  (lambda (s1 s2)
    (let loop ([s1 s1])
      (cond [(null? s1) s2]
            [(memq (car s1) s2) (loop (cdr s1))]
            [else (cons (car s1) (loop (cdr s1)))]))))
(define remove ; removes the first occurrence of sym from los.
  (lambda (sym los)
    (cond [(null? los) '()]
          [(eq? sym (car los)) (cdr los)]
          [else (cons (car los) (remove sym (cdr los)))])))
(define free-vars ; convert to CPS. You should first convert
  (lambda (exp) ; union and remove.
    (cond [(symbol? exp) (list exp)]
          [(eq? (1st exp) 'lambda)
           (remove (car (2nd exp))
                   (free-vars (3rd exp)))]
          [else (union (free-vars (1st exp)
                                  (free-vars (2nd exp))))])))


(define free-vars-cps
  (lambda (a b)
    ;(print a)
    ;(if (continuation? b)
    ;(cases continuation? b
    ;  [init-k () (free-vars-cps a make-k)]
    ;  [list-k () (list (free-vars-cps a make-k))])
    (cond [(continuation? b) (free-vars-cps a make-k)]
          [(null? a) (apply-k b a)]
          [(symbol? a) (apply-k b (list a))]
          ;[(eq? (1st a) 'lambda)
          ; (remove-cps (car (2nd a))
          ;         (free-vars (3rd a)) b)]
          ;[else (union-cps (free-vars-cps (1st a) b)
          ;                        (free-vars-cps (2nd a)) b)]
          ;[(eqv? (car a) 'lambda) (free-vars-cps (cdr a) (make-k (lambda (d) (apply-k b d))))]
          [(eq? (car a) 'lambda) (remove (car (2nd a))
                                             (free-vars-cps (3rd a) b)
                                             )];(make-k (lambda (d) (apply-k b d)))
          ;[else (union-cps (free-vars-cps (1st a) b) (free-vars-cps (2nd a) b) ;b)]
          ;                 (make-k (lambda (ls) (apply-k b (cons (free-vars-cps (1st a) b) (free-vars-cps (2nd a) b))))))]
          
          ;[(eq? (1st a) 'lambda)
          ; (remove-cps (2nd a)
          ;         (free-vars-cps (3rd a) b) b)]
          ;[else (union-cps (free-vars-cps (1st a)
          ;                        (free-vars-cps (2nd a) b)))]

          ;[(eqv? (car a) 'lambda) (free-vars-cps (cdr a) (make-k (lambda (d) (apply-k b d))))]
          [else (union (free-vars-cps (1st a) b) (free-vars-cps (2nd a) b))]
          )))
;(trace free-vars-cps)

(define-datatype continuation continuation? 
[init-k] 
[list-k])

(define union-cps
  (lambda (a b c)
    (cond [(null? a) b]
          [(not (memq (car a) b)) (cons (car a)
                                  (union-cps (cdr a) b c))]
          [else (union-cps (cdr a) b c)]
          )))
;(define union-cps
;  (lambda (a b c)
;    (let loop ([a a])
;      (cond [(null? a) (apply-k c b)]
;            [(memq-cps (car a) b c) (apply-k c (loop (cdr a)))]
;            [else (apply-k c (cons (car a) (loop (cdr a))))]))))
(define remove-cps
  (lambda (a b c)
    (cond [(continuation? c) (remove-cps a b make-k)]
          [(null? b) b]
          [(eqv? a (car b)) (apply-k c (cdr b))]
          [else (remove-cps a (cdr b) (make-k (lambda (ls)
                                                (apply-k c (cons (car b) ls)))))]
          )))
;          [(eq? sym (car los)) (cdr los)]
;          [else (cons (car los) (remove sym (cdr los)))])))


;(trace remove-cps)

(define memoize
  (lambda (f hash equiv?)
    (let ((hashtable (make-custom-hash equiv? hash)))
      (lambda k
        (if (dict-has-key? hashtable k)
            (dict-ref hashtable k)
            (let () (dict-set! hashtable k (apply f k))
                   (apply f k))
        ))
      )))

(define subst-leftmost
  (lambda (a b c d)
    (letrec ((tree (cond [(null? c) c]
                         [(d b c) a]
                         [(symbol? c) c]
                         [else (let ((left (subst-leftmost a b (car c) d)))
                                 (if (eq? left (car c))
                                     (cons left (cdr c))
                                     (cons left (subst-leftmost a b (cdr c) d))))])))
      tree)))

;;--------  Used by the testing mechanism   ------------------

(define-syntax nyi
  (syntax-rules ()
    ([_]
     [error "nyi"])))
