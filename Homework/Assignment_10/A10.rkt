#lang racket

(provide free-vars bound-vars lexical-address un-lexical-address convert-multip-calls convert-multip-lambdas convert-ifs)
(require racket/trace)
(define free-vars
  (lambda (a)
    (remove-duplicates (flatten (cond [(null? a) '()]
                                      [(not (list? a)) (list a)]
                                      [(eqv? 'lambda (car a)) (filter (lambda (b) (not (or (equal? b (cadr a)) (member b (cadr a))))) (free-vars (caddr a)))];length=3
                                      [(list? (car a)) (list (free-vars (car a)) (free-vars (cadr a)))];length 2
                                      [else (list (car a) (free-vars (cadr a)))])))))

(define bound-vars
  (lambda (a)
    (remove-duplicates (flatten (cond [(null? a) '()]
                                      [(not (list? a)) '()];single
                                      [(eqv? 'lambda (car a)) (list (filter (lambda (b) (or (equal? b (caddr a)) (member b (flatten (caddr a))))) (cadr a)) (bound-vars (caddr a)))];length=3
                                      [(list? (car a)) (list (bound-vars (car a)) (bound-vars (cadr a)))];length 2
                                      [else (list (bound-vars (car a)) (bound-vars (cadr a)))])))))


(define convert-multip-calls
  (lambda (lcexp)
    (cond [(null? lcexp) '()]
          [(not (list? lcexp)) lcexp]
          [(eqv? 'lambda (car lcexp)) (list (car lcexp) (cadr lcexp) (convert-multip-calls (caddr lcexp)))]
          [(= 1 (length lcexp)) (convert-multip-calls (car lcexp))]
          [(= 2 (length lcexp)) (list (convert-multip-calls (car lcexp)) (convert-multip-calls (cadr lcexp)))]
          [else (convert-multip-calls (cons (list (convert-multip-calls (car lcexp)) (convert-multip-calls (cadr lcexp))) (cddr lcexp)))])))


(define convert-multip-lambdas
  (lambda (lcexp)
    (cond [(null? lcexp) '()]
          [(not (list? lcexp)) lcexp]
          [(eqv? 'lambda (car lcexp)) (if (and (list? (cadr lcexp)) (< 1 (length (cadr lcexp))))
                                          (list 'lambda (list (caadr lcexp)) (convert-multip-lambdas (list 'lambda (cdadr lcexp) (caddr lcexp))))
                                          (list 'lambda (cadr lcexp) (convert-multip-lambdas (caddr lcexp))))]
          [(= 1 (length lcexp)) (convert-multip-lambdas (car lcexp))]
          [else (list (convert-multip-lambdas (car lcexp)) (convert-multip-lambdas (cadr lcexp)))])))

(define convert-ifs
  (lambda (exp)
    (cond [(null? exp) '()]
          [(not (list? exp)) (cond [(equal? exp 'if) '()]
                                   [(equal? exp '#t) '(lambda (thenval elseval) thenval)]
                                   [(equal? exp '#f) '(lambda (thenval elseval) elseval)]
                                   [else exp])]
          [(list? (car exp)) (cons (convert-ifs (car exp)) (convert-ifs (cdr exp)))]
          [else (if (equal? 'if (car exp))
                    (convert-ifs (cdr exp))
                    (cons (convert-ifs (car exp)) (convert-ifs (cdr exp))))])))



(define lexical-address
  (lambda (a)
    (lahelper a (list '()))))

;((a b c) (a 0 1) (b 1 1) (c 1 2))) format of ls
(define lahelper
  (lambda (a ls)
    (cond [(null? a) null]
          [(list? a) (cond [(null? a) '()]
                           [(equal? 'lambda (car a)) (list 'lambda (cadr a) (lahelper (caddr a) (addls (adddepth ls) (cadr a) 0)))];lambda
                           [(equal? 'let (car a)) (list 'let (map (lambda (v) (list (car v) (lahelper (cadr v) ls))) (cadr a)) (lahelper (caddr a) (addls (adddepth ls) (map car (cadr a)) 0)))];let
                           [(equal? 'if (car a)) (list 'if (lahelper (cadr a) ls) (lahelper (caddr a) ls) (lahelper (cadddr a) ls))];if
                           [else (cons (lahelper (car a) ls) (lahelper (cdr a) ls))])];(a b) form
          [(number? a) a];number
          [else (process a ls)];variable
          )))
(define process
  (lambda (a ls)
    (if  (member a (car ls))
        (cons ': (cdr (address a (cdr ls))))
        (list ': 'free a))))
(define address
  (lambda (a ls)
    (if (eqv? (caar ls) a)
        (car ls)
        (address a (cdr ls)))))
(define addls
  (lambda (ls v i)
    (cond [(null? v) ls]
          [else (append (list (cons (car v) (car (addls ls (cdr v) (add1 i))))) (list (list (car v) 0 i)) (cdr (addls ls (cdr v) (add1 i))))])))
(define adddepth
  (lambda (ls)
    (cons (car ls) (map (lambda (v) (list (car v) (add1 (cadr v)) (caddr v))) (cdr ls)))))


(define un-lexical-address
  (lambda (a)
    (ulahelper a (list '()))))
(define ulahelper
  (lambda (a ls)
    (cond [(null? a) null]
          [(list? a) (cond [(null? a) '()]
                           [(and (equal? ': (car a)) (equal? 'free (cadr a))) (caddr a)];free
                           [(equal? ': (car a)) (caar (filter (lambda (v) (and (equal? (cadr v) (cadr a)) (equal? (caddr v) (caddr a)))) (cdr ls)))];not free
                           [(equal? 'lambda (car a)) (list 'lambda (cadr a) (ulahelper (caddr a) (addls (adddepth ls) (cadr a) 0)))];lambda
                           [(equal? 'let (car a)) (list 'let (map (lambda (v) (list (car v) (ulahelper (cadr v) ls))) (cadr a)) (ulahelper (caddr a) (addls (adddepth ls) (map car (cadr a)) 0)))];let
                           [(equal? 'if (car a)) (list 'if (ulahelper (cadr a) ls) (ulahelper (caddr a) ls) (ulahelper (cadddr a) ls))];if
                           [else (cons (ulahelper (car a) ls) (ulahelper (cdr a) ls))])];(a b) form
          [(number? a) a];number
          [else (process a ls)];variable
          )))

;;--------  Used by the testing mechanism   ------------------

(define-syntax nyi
  (syntax-rules ()
    ([_]
     [error "nyi"])))
