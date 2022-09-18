#lang racket

(provide make-slist-leaf-iterator subst-leftmost)

(define make-slist-leaf-iterator
  (lambda (a)
    (let [(s (make-stack))]
      (s 'push a)
      (lambda (arg)
        (cond [(equal? arg 'next) (cond [(s 'empty?) #f]
                                        [else (let gettop [(top (s 'pop))]
                                                (cond [(null? top) (if (s 'empty?)
                                                                       #f
                                                                       (gettop (s 'pop)))]
                                                      [(list? top) (let []
                                                                     (filter null? top)
                                                                     (for-each (lambda (element) (s 'push element)) (reverse top))
                                                                     (gettop (s 'pop)))]
                                                      [else top]))])]
              [else #f])))))

(define make-stack
  (lambda ()
    (let ([stk '()])
      (lambda (msg . args ) 
        (case msg ; Scheme's case is a similar to switch in some other languages.
          [(empty?) (null? stk)]
          [(push) (set! stk (cons (car args) stk))]
          [(pop) (let ([top (car stk)])
                   (set! stk (cdr stk))
                   top)]
          [else (error 'stack "illegal message to stack object: ~a" msg)])))))


(define subst-leftmost
  (lambda (a b c d)
    (car (slhelper a b c d))))

;return: list, t/f that whether appear
(define slhelper
  (lambda (a b c d)
    (cond [(null? c) (list c #f)];null
          [(list? (car c)) (if (cadr (slhelper a b (car c) d))
                               (list (cons (car (slhelper a b (car c) d)) (cdr c)) #t)
                               (list (cons (car c) (car (slhelper a b (cdr c) d))) (cadr (slhelper a b (cdr c) d))))];first is list, don't run on cdr if if is true
          [(d (car c) b) (list (cons a (cdr c)) #t)];is first element
          [(list? c) (list (cons (car c) (car (slhelper a b (cdr c) d))) (cadr (slhelper a b (cdr c) d)))];it is list
          [(d c b) (list a #t)];self equal
          [else (list c #f)])));single element


;;--------  Used by the testing mechanism   ------------------

(define-syntax nyi
  (syntax-rules ()
    ([_]
     [error "nyi"])))
