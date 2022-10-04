#lang racket

;(require "../chez-init.rkt")
(require eopl/eopl)
(provide parse-exp unparse-exp)


; This is a parser for simple Scheme expressions, 
; such as those in EOPL, 3.1 thru 3.3.

; You will want to replace this with your parser that includes more expression types, more options for these types, and error-checking.

; STEP 1: Modify to accept lambdas/apps with multiple & zero params
(require eopl/eopl)
(provide parse-exp unparse-exp)

(define-datatype expression expression?
  [var-exp
   (id symbol?)]
  [lit-exp
   (data lit?)]
  [lc-mult
   (args (list-of var-exp?))
   (body lambda-body?)]
  [lc-indef
   (arg var-exp?)
   (body lambda-body?)]
;  [let-exp
;   (exp let-variant?)]
  [let-basic
   (ids list?)
   (body list?)]
  [let-named
   (name var-exp?)
   (ids list?)
   (body list?)]
  [let-star
   (ids list?)
   (body list?)] ;; think of under the hood representation
  [let-rec
   (ids list?)
   (body list?)]


  [if-else
   (cond expression?)
   (if-true expression?)
   (else expression?)]
  [if-no-else
   (cond expression?)
   (if-true expression?)]
  ;[if-exp
   ;(exp if-variant?)]
  [set-exp
   (var var-exp?)
   (set-to expression?)]
  
  [app-exp
   (rator expression?)
   (rand (list-of expression?))])

;(define-datatype let-variant let-variant?
;  [let-basic
;   (ids list?)
;   (body (list-of expression?))]
;  [let-named
;   (name var-exp?)
;   (ids list?)
;   (body (list-of expression?))]
;  [let-star
;   (ids list?)
;   (body (list-of expression?))] ;; think of under the hood representation
;  [let-rec
;   (ids list?)
;   (body (list-of expression?))])
;
;(define-datatype if-variant if-variant?
;  [if-else
;   (cond expression?)
;   (if-true expression?)
;   (else expression?)]
;  [if-no-else
;   (cond expression?)
;   (if-true expression?)])

;(define-datatype binding binding?
;  [single-binding
;   (sym var-exp?)
;   (val expression?)])

;(define-datatype lambda-variant lambda-variant?
;  [lc-mult
;   (args (list-of var-exp?))
;   (body lambda-body?)]
;  [lc-indef
;   (arg var-exp?)
;   (body lambda-body?)])

;(define-datatype lambda-body lambda-body?
;  [lbody-single])
;  [lbody-single
;   (body expression?)]
;  [lbody-multi
;   (first expression?)
;   (rest lambda-body?)])
(define lambda-body?
  (lambda (a)
    (or (symbol? a) (list? a))))

; Procedures to make the parser a little bit saner.
(define 1st car)
(define 2nd cadr)
(define 3rd caddr)
(define 4th cadddr)

(define check
  (lambda (a)
    (cond [(or (symbol? a) (number? a) (string? a)) #t]
          ;[(symbol? a) (var-exp a)]
          ;[(number? a) (lit-exp a)]
          [(list? a) (cond [(eqv? (car a) 'if) (if (or (= 3 (length a)) (= 4 (length a))) #t #f)]
                           [(eqv? (car a) 'lambda) (if (and (<= 3 (length a)) (or (symbol? (cadr a)) (= 0 (length (filter-not symbol? (cadr a)))))) #t #f)]
                           [(eqv? (car a) 'set!) (= 3 (length a))]
                           [(eqv? (car a) 'let) (checklet (cdr a))]
                           [(eqv? (car a) 'letrec) (checklet (cdr a))]
                           [(eqv? (car a) 'let*) (checklet (cdr a))]
                           [else #t])]
          [(pair? a) #f])
    ))
(define checklet
  (lambda (a)
    ;(print a)
    (cond [(not (list? a)) #f]
          [(null? (cdr a)) #f]
          [(symbol? (car a)) (checklet (cdr a))]
          [(not (null? (car a))) (and (list? (car a)) (list? (cdr a)) (andmap (lambda (b) (and (list? b) (= (length b) 2) (symbol? (car b)))) (car a)))]
          [else #t])))

;(define checkletrec
;  (lambda (a)
;    (print a)
;    (cond [(not (list? a)) #f]
;          [(null? (cdr a)) #f]
;          [(symbol? (car a)) (checklet (cdr a))]
;          [(not (null? (car a))) (and (list? (car a)) (list? (cdr a)) (andmap (lambda (b) (and (list? b) (= (length b) 2) (symbol? (car b)))) (car a)))]
;          [else #t])))

(define parse-exp         
  (lambda (datum)
    (if (not (check datum))
        (error 'parse-exp "bad expression: ~s" datum)
    (cond
      [(symbol? datum) (var-exp datum)]
      ;[(lit? datum) (lit-exp datum)]
      [(number? datum) (lit-exp datum)]
      [(string? datum) (lit-exp datum)]
      [(vector? datum) (lit-exp datum)]
      [(pair? datum)
       (cond
         [(eqv? (car datum) 'if)
          (if (not (or (= (length datum) 3) (= (length datum) 4)))
              (error 'parse-error "error")
              (parse-if datum))]
         [(eqv? (car datum) 'lambda)
          (if (< (length datum) 3)
              (error 'parse-error "error")
          (parse-lambda datum))]
         [(or (eqv? (car datum) 'let)
              (eqv? (car datum) 'let*)
              (eqv? (car datum) 'letrec))
          (parse-let datum)]
         [(= 2 (length datum)) (app-exp (parse-exp (1st datum)) (map parse-exp (cdr datum)))]
         [else (app-exp (parse-exp (car datum)) (map parse-exp (cdr datum)))])]
      [else (error 'parse-exp "bad expression: ~s" datum)]))))


(define unparse-exp
  (lambda (exp)
    ;(print exp)
    (cases expression exp
      [var-exp (id) id]
      [lit-exp (data) data]
      [lc-mult (exp1 exp2) (cons 'lambda (cons (map unparse-exp exp1) (map unparse-exp exp2)))]
      [lc-indef (exp1 exp2) (cons 'lambda (cons (unparse-exp exp1) (map unparse-exp exp2)))]
      [let-basic (ids body) (unparse-let exp)]
      [let-star (ids body) (unparse-let exp)]
      [let-named (name ids body) (unparse-let exp)]
      [let-rec (ids body) (unparse-let exp)]
      ;[if-exp (if-exp) (unparse-if exp)]
      [if-else (a b c) (unparse-if exp)]
      [if-no-else (exp1 exp2) (unparse-if exp)]
      [set-exp (var exp) (list 'set! (unparse-exp var) (unparse-exp exp))]
      [app-exp (rator rand) (cons (unparse-exp rator) (map unparse-exp rand))]
      [else (map unparse-exp exp)]
      )))

(define parse-let
  (lambda (datum)
    ;(print datum)
    (cond [(and (eqv? (car datum) 'let) (symbol? (2nd datum))) (let-named (parse-exp (cadr datum)) (map (lambda (b) (cons (parse-exp (car b)) (list (parse-exp (cadr b))))) (caddr datum)) (map parse-exp (cdddr datum)))]
          [(eqv? (car datum) 'let) (let-basic (map (lambda (b) (append (list (parse-exp (car b))) (list (parse-exp (cadr b))))) (cadr datum)) (map parse-exp (cddr datum)))]
          [(eqv? (car datum) 'let*) (let-star (map (lambda (b) (cons (parse-exp (car b)) (list (parse-exp (cadr b))))) (cadr datum)) (map parse-exp (cddr datum)))]
          [(eqv? (car datum) 'letrec) (let-rec (map (lambda (b) (cons (parse-exp (car b)) (list (parse-exp (cadr b))))) (cadr datum)) (map parse-exp (cddr datum)))])))

(define unparse-let
  (lambda (exp)
    (cases expression exp
      [let-basic (ids body) (append (list 'let) (list (map (lambda (b) (list (unparse-exp (car b)) (unparse-exp (cadr b)))) ids)) (map unparse-exp (if (expression? (car body))
                                                                                                                                                       body
                                                                                                                                                       (car body))))]
      [let-named (name ids body) (append (list 'let) (list (unparse-exp name)) (list (map (lambda (b) (list (unparse-exp (car b)) (unparse-exp (cadr b)))) ids)) (map unparse-exp body))]
      [let-star (ids body) (append (list 'let*) (list (map (lambda (b) (list (unparse-exp (car b)) (unparse-exp (cadr b)))) ids)) (map unparse-exp body))]
      [let-rec (ids body) (append (list 'letrec) (list (map (lambda (b) (list (unparse-exp (car b)) (unparse-exp (cadr b)))) ids)) (map unparse-exp body))]
      [else '()])))

(define parse-if
  (lambda (datum)
    ;(print datum)
    (if (= (length datum) 4)
        (if-else (parse-exp (2nd datum)) (parse-exp (3rd datum)) (parse-exp (4th datum)))
        (if-no-else (parse-exp (2nd datum)) (parse-exp (3rd datum))))))

(define unparse-if
  (lambda (exp)
    (cases expression exp
      [if-else (cond if-true else) (list 'if (unparse-exp cond) (unparse-exp if-true) (unparse-exp else))]
      [if-no-else (cond if-true) (list 'if (unparse-exp cond) (unparse-exp if-true))]
      [else '()])))

;(define parse-binding
;  (lambda (datum)
;    (list (1st datum) (2nd datum))))

;(define unparse-binding
;  (lambda (exp)
;    (cases binding exp
;      [single-binding (sym val) ((unparse-exp sym) (unparse-exp val))])))

(define parse-lambda
  (lambda (datum)
    ;(print datum)
    (if (list? (2nd datum))
        (lc-mult (map parse-exp (2nd datum)) (map parse-exp (cddr datum)))
        (lc-indef (parse-exp (2nd datum)) (map parse-exp (cddr datum))))))

;(define unparse-lambda
;  (lambda (exp)
;    (print exp)
;    (if (list? (cadr exp))
;      [(cons 'lambda (cons (map unparse-exp (2nd exp)) (map unparse-exp (cddr exp))))]
;      [(cons 'lambda (cons (2nd exp) (map unparse-exp (cddr exp))))])))

;(define parse-lambda-body
;  (lambda (datum)
;    (map parse-exp datum)))


; An auxiliary procedure that could be helpful.
(define var-exp?
  (lambda (x)
    (cases expression x
      [var-exp (id) #t]
      [else #f])))

(define lit?
  (lambda (x)
    (or (number? x)
        (string? x)
        (vector? x)
        (symbol? x)
        (boolean? x)
        (null? x)
        (eqv? (car x) 'quote))))

;;--------  Used by the testing mechanism   ------------------

(define-syntax nyi
  (syntax-rules ()
    ([_]
     [error "nyi"])))
