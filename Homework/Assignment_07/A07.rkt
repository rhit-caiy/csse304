#lang racket

(provide vector-append-list group-by-two group-by-n bt-leaf-sum bt-inorder-list bt-max bt-max-interior slist-map slist-reverse slist-paren-count slist-depth slist-symbols-at-depth path-to make-c...r)
(require racket/trace)

(define vector-append-list
  (lambda (a b)
    (make-vector (+ (vector-length a) (length b)))
    (let ([new-vector (make-vector (+ (vector-length a) (length b)))])
      (copy-from-vector new-vector a 0)
      (copy-from-list new-vector b (vector-length a))
      new-vector)))

(define copy-from-vector
  (lambda (a v i)
    (if (> (vector-length v) i)
        (let ([new-v a])
          (vector-set! new-v i (vector-ref v i))
          (copy-from-vector new-v v (add1 i)))
        a)))
(define copy-from-list
  (lambda (a ls i)
    (if (< i (vector-length a))
          (let ([new-v a])
             (vector-set! new-v i (car ls))
             (copy-from-list new-v (cdr ls) (add1 i)))
           a)))

(define group-by-two
  (lambda (a)
    (cond [(= (length a) 0) '()]
          [(= (length a) 1) (list a)]
          [else (cons (list (car a) (cadr a)) (group-by-two (cddr a)))])))

(define group-by-n
  (lambda (a b)
    (remove '() (decode (nextn (list '() a) b 0)))))
(define nextn
  (lambda (a b i)
    (cond [(null? (cadr a)) a]
          [(< i b) (nextn (list (append (car a) (list (caadr a))) (cdadr a)) b (add1 i))]
          [else (list (car a) (nextn (list '() (cadr a)) b 0))])))

(define decode
  (lambda (a)
    (cond [(null? a) '()]
          [(null? (cdr a)) (list (car a))]
          [(null? (cadr a)) (list (car a))]
          [else (append (list (car a)) (decode (cadr a)))])))




(define bt-leaf-sum
  (lambda (a)
    (cond [(null? a) 0]
          [(number? a) a]
          [(list? (car a)) (+ (bt-leaf-sum (car a)) (bt-leaf-sum (cdr a)))]
          [(number? (car a)) (+ (car a) (bt-leaf-sum (cdr a)))]
          [else (bt-leaf-sum (cdr a))])))

(define bt-inorder-list
  (lambda (a)
    (cond [(null? a) '()]
          [(number? a) '()]
          [else (append (bt-inorder-list (cadr a)) (list (car a)) (bt-inorder-list (caddr a)))])))

(define bt-max
  (lambda (a)
    (cond [(number? a) a]
          [(null? (cdr a)) (car a)]
          [(number? (car a)) (max (car a) (bt-max (cadr a)) (bt-max (caddr a)))]
          [else (max (bt-max (cadr a)) (bt-max (caddr a)))])))

(define bt-max-interior
  (lambda (a)
    (caddr (maxnode a))))
; current value, symbol value, symbol
(define maxnode
  (lambda (a)
    (cond [(and (number? (cadr a)) (number? (caddr a))) (list (+ (cadr a) (caddr a)) (+ (cadr a) (caddr a)) (car a))]
          [(number? (cadr a)) (if (and (positive? (cadr a)) (< (cadr (maxnode (caddr a))) (+ (car (maxnode (caddr a))) (cadr a))))
                                  (list (+ (car (maxnode (caddr a))) (cadr a)) (+ (car (maxnode (caddr a))) (cadr a)) (car a))
                                  (cons (+ (car (maxnode (caddr a))) (cadr a)) (cdr (maxnode (caddr a)))))]
          [(number? (caddr a)) (if (and (positive? (caddr a)) (< (cadr (maxnode (cadr a))) (+ (car (maxnode (cadr a))) (caddr a))))
                                  (list (+ (car (maxnode (cadr a))) (caddr a)) (+ (car (maxnode (cadr a))) (caddr a)) (car a))
                                  (cons (+ (car (maxnode (cadr a))) (caddr a)) (cdr (maxnode (cadr a)))))]
          [else (nodemax (maxnode (cadr a)) (maxnode (caddr a)) (car a))])))

(define nodemax
  (lambda (a b c)
    (if (and (> (+ (car a) (car b)) (cadr a)) (> (+ (car a) (car b)) (cadr b)))
        (list (+ (car a) (car b)) (+ (car a) (car b)) c)
        (cons (+ (car a) (car b)) (cdr (argmax cadr (list a b)))))))

(define slist-map
  (lambda (a b)
    (cond [(null? b) '()]
          [(list? (car b)) (cons (slist-map a (car b)) (slist-map a (cdr b)))]
          [else (cons (a (car b)) (slist-map a (cdr b)))])))

(define slist-reverse
  (lambda (a)
    (cond [(null? a) a]
          [(list? a) (reverse (map slist-reverse a))]
          [else a])))

(define slist-paren-count
  (lambda (a)
    (+ 2 (spchelper a))))
(define spchelper
  (lambda (a)
    (cond [(null? a) 0]
          [(list? (car a)) (+ 2 (spchelper (car a)) (spchelper (cdr a)))]
          [else (spchelper (cdr a))])))

(define slist-depth
  (lambda (a)
    (cond [(null? a) 1]
          [(list? (car a)) (max (+ 1 (slist-depth (car a))) (slist-depth (cdr a)))]
          [else (max 0 (slist-depth (cdr a)))])))

(define slist-symbols-at-depth
  (lambda (a b)
    (cond [(null? a) '()]
          [(= b 1) (filter symbol? a)]
          [else (slist-symbols-at-depth (merge (filter list? a)) (- b 1))])))
(define merge
  (lambda (a)
    (cond [(null? a) '()]
          [else (append (car a) (merge (cdr a)))])))

(define path-to
  (lambda (a b)
    (if (member b (flatten a))
        (pathto a b)
        #f)))
(define pathto
  (lambda (a b)
    (cond ;[(null? a) null]
          [(equal? (car a) b) (list 'car)]
          [(list? (car a)) (if (member b (flatten (cdr a)))
                               (cons 'cdr (pathto (cdr a) b))
                               (cons 'car (pathto (car a) b)))]
          [else (cons 'cdr (pathto (cdr a) b))])));else (pathto (cdr a) b)

(define make-c...r
  (lambda (a)
    (lambda (b)
      (foldr (lambda (c d) (c d)) b (replace (string->list a))))))

(define replace
  (lambda (a)
    (cond [(null? a) '()]
          [(equal? (car a) #\a) (cons car (replace (cdr a)))]
          [else (cons cdr (replace (cdr a)))])))

;(define compose
;  (lambda a
;    (composehelper a)))
;(define composehelper
;  (lambda (a)
;    (cond [(not (list? a)) (composehelper (list a))]
;          [(list? (car a)) (composehelper (caar a))]
;          [(null? (cdr a)) (car a)]
;          [else (lambda (b) ((car a) ((composehelper (cdr a)) b)))])
;      ))

;;--------  Used by the testing mechanism   ------------------

(define-syntax nyi
  (syntax-rules ()
    ([_]
     [error "nyi"])))
