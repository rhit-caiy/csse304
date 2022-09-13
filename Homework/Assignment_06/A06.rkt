#lang racket

(provide curry2 curried-compose compose make-list-c reverse-it map-by-position empty-BST empty-BST? BST-insert BST-inorder BST? BST-element BST-left BST-right BST-insert-nodes BST-contains? BST-height let->application let*->let qsort sort-list-of-symbols)

(require racket/trace)

(define curry2
  (lambda (a)
    (lambda (b)
      (lambda (c)
        (a b c)))
    ))

(define curried-compose
  (lambda (a)
    (lambda (b)
      (lambda (c)
        (a (b c))))))

(define compose
  (lambda a
    (composehelper a)))
    ;(map (lambda (c) (compose1 c)) a)))
;    (cond [(not (list? a)) (compose (list a))]
          ;[(list? (car a)) (compose (caar a))]
;          [(null? a) null]
;          [(null? (cdr a)) (car a)]
          ;[(= (length a) 1) (car a)]
          ;[(null? (car a)) null]
          ;[else (lambda (b) ((car a) ((compose (cdr a)) b)))]
;          [else (lambda (b) ((compose1 a) b))]
;        )))
(define composehelper
  (lambda (a)
    (cond [(not (list? a)) (composehelper (list a))]
          [(list? (car a)) (composehelper (caar a))]
          [(null? (cdr a)) (car a)]
          [else (lambda (b) ((car a) ((composehelper (cdr a)) b)))])
      ))

(define make-list-c
  (lambda (a)
    (lambda (b)
      (make-list a b))))

(define reverse-it
  (lambda (a)
    (cond [(null? a) null]
          [(null? (cdr a)) (list (car a))]
          [else (append (reverse-it (cdr a)) (list (car a)))])))

(define map-by-position
  (lambda (a b)
    (map (lambda (a b)
           (a b)) a b)))

(define empty-BST
  (lambda ()
    '()))

(define empty-BST?
  (lambda (a)
    (null? a)))

(define BST-insert
  (lambda (a b)
    (cond [(empty-BST? b) (node a)]
          [(list? (car b)) (BST-insert a (car b))]
          [(equal? '() b) (node a)]
          [(= a (car b)) b]
          [(< a (car b)) (list (car b) (BST-insert a (cadr b)) (caddr b))]
          [else (list (car b) (cadr b) (BST-insert a (caddr b)))])))

(define node
  (lambda (a)
    (list a '() '())))

(define BST-inorder
  (lambda (a)
    (cond [(null? a) '()]
          [(null? (car a))'()]
          [(null? (BST-inorder (cadr a))) (cons (car a) (BST-inorder (caddr a)))]
          [else (append (BST-inorder (cadr a)) (cons (car a) (BST-inorder (caddr a))))])));[else ((BST-inorder (cadr a)) (car a) (BST-inorder (caddr a)))])))

(define BST?
  (lambda (a)
    (and (cond [(null? a) #t]
          [(not (list? a)) #f]
          [(list? (car a)) #f]
          [(not (number? (car a))) #f]
          [(not (= (length a) 3)) #f]
          [(not (list? (cadr a))) #f]
          [(not (list? (caddr a))) #f]
          [else (and (BST? (BST-left a)) (BST? (BST-right a)))])
         (equal? (BST-inorder a) (sort (BST-inorder a) <))
         )))

(define BST-element
  (lambda (a)
    (car a)))

(define BST-left
  (lambda (a)
    (cadr a)))

(define BST-right
  (lambda (a)
    (caddr a)))

(define BST-insert-nodes
  (lambda (a b)
     (cond [(null? b) a]
           [else (BST-insert-nodes (BST-insert (car b) a) (cdr b))])));map (lambda (c) (BST-insert c a)) b

(define BST-contains?
  (lambda (a b)
    (cond [(empty-BST? a) #f]
          ;[(null? a) #f]
          ;[(null? (car a)) #f]
          [(= b (car a)) #t]
          [(< b (car a)) (BST-contains? (BST-left a) b)]
          [else (BST-contains? (BST-right a) b)])))

(define BST-height
  (lambda (a)
    (cond [(null? a) -1]
          [(null? (car a)) -1]
          [else (add1 (max (BST-height (cadr a)) (BST-height (caddr a))))])))

(define let->application
  (lambda (a)
    (cons (list 'lambda (map (lambda (b) (car b)) (cadr a)) (caddr a)) (map (lambda (b) (cadr b)) (cadr a)))))

(define let*->let
  (lambda (a)
    (if (= (length (cadr a)) 1)
        (list 'let (list (caadr a)) (removebracket (cddr a)))
        (list 'let (list (caadr a)) (let*->let (list 'let* (cdadr a) (cddr a)))))))

(define removebracket
  (lambda (a)
    (if (list? (car a))
        (removebracket (car a))
        (car a)
        )))

(define qsort
  (lambda (a b)
    (cond [(null? b) '()]
          [else (append (qsort a (filter (lambda (c) (a c (car b))) (cdr b))) (list (car b)) (qsort a (filter (lambda (c) (not (a c (car b)))) (cdr b))))])))

(define sort-list-of-symbols
  (lambda (a)
    (map (lambda (b) (string->symbol b)) (qsort string<=? (map (lambda (c) (symbol->string c)) a)))))

;;--------  Used by the testing mechanism   ------------------

(define-syntax nyi
  (syntax-rules ()
    ([_]
     [error "nyi"])))
