#lang racket
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
  [app-exp
   (rator expression?)
   (rand expression?)]
  [let-exp
   (exp let-variant?)]
  [if-exp
   (exp if-variant?)]
  [set-exp
   (var var-exp?)
   (set-to expression?)])

(define-datatype let-variant let-variant?
  [let-basic
   (ids (list-of binding?))
   (body lambda-body?)]
  [let-named
   (name var-exp?)
   (ids (list-of binding?))
   (body lambda-body?)]
  [let-star
   (ids (list-of binding?))
   (body lambda-body?)] ;; think of under the hood representation
  [let-rec
   (ids (list-of binding?))
   (body lambda-body?)])

(define-datatype if-variant if-variant?
  [if-else
   (cond expression?)
   (if-true expression?)
   (else expression?)]
  [if-no-else
   (cond expression?)
   (if-true expression?)])

(define-datatype binding binding?
  [single-binding
   (sym var-exp?)
   (val expression?)])

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

(define parse-exp         
  (lambda (datum)
    (cond
      [(symbol? datum)
       (var-exp datum)]
      [(lit? datum)
       (lit-exp datum)]
      [(pair? datum)
       (cond
         [(eqv? (car datum) 'lambda)
          (print datum)
              (if (symbol? (2nd (cdr datum)))
                  (lc-indef (parse-exp (2nd datum)) (map parse-exp (cddr datum)))
                  (lc-mult (map parse-exp (2nd datum)) (map parse-exp (cddr datum))))]
          ;(parse-lambda datum)]
         [(or (eqv? (car datum) 'let)
              (eqv? (car datum) 'let*)
              (eqv? (car datum) 'letrec))
          (parse-let exp)]
         [(= 2 (length datum)) (app-exp (parse-exp (1st datum)) (parse-exp (2nd datum)))]
         [else (map parse-exp datum)])]
      [else (error 'parse-exp "bad expression: ~s" datum)])))

(define unparse-exp
  (lambda (exp)
    (cases expression exp
      [var-exp (id) id]
      [lit-exp (data) data]
      [lc-mult (exp1 exp2) (cons 'lambda (cons (map unparse-exp exp1) (list (map unparse-exp (car exp2)))))]
      [lc-indef (exp1 exp2) (cons 'lambda (cons (unparse-exp exp1) (map unparse-exp exp2)))]
      [app-exp (rator rand) (list (unparse-exp rator) (unparse-exp rand))]
      [let-exp (exp) (unparse-let exp)]
      [if-exp (if-exp) (unparse-if exp)]
      [set-exp (var exp) (list 'set! (unparse-exp var) (unparse-exp exp))]
      )))

(define parse-let
  (lambda (datum)
    (cond [(and (eqv? (car datum) 'let)
                (var-exp? (2nd datum)))
           (let-named (2nd datum) (3rd datum) (if (= (length datum) 4)
                                                  (cadddr datum)
                                                  (cdddr datum)))]
          [(eqv? (car datum) 'let)
           (let-basic (2nd datum) (if (= (length datum) 3)
                                      (cadddr datum)
                                      (cdddr datum)))]
          [(eqv? (car datum) 'let*)
           (let-star (2nd datum) (if (= (length datum) 3)
                                     (cadddr datum)
                                     (cdddr datum)))]
          [(eqv? (car datum) 'letrec)
           (let-rec (2nd datum) (if (= (length datum) 3)
                                    (cadddr datum)
                                    (cdddr datum)))])))

(define unparse-let
  (lambda (exp)
    (cases let-variant exp
      [let-basic (ids body) (list 'let (unparse-exp ids) (unparse-exp body))]
      [let-named (name ids body) (list 'let (unparse-exp name) (unparse-exp ids) (unparse-exp body))]
      [let-star (ids body) (list 'let* (unparse-exp ids) (unparse-exp body))]
      [let-rec (ids body) (list 'letrec (unparse-exp ids) (unparse-exp body))])))

(define parse-if
  (lambda (datum)
    (if (= (length datum) 3)
        (if-else (2nd datum) (3rd datum) (4th datum))
        (if-no-else (2nd datum) (3rd datum)))))

(define unparse-if
  (lambda (exp)
    (cases if-variant exp
      [if-else (cond if-true else) (list 'if (unparse-exp cond) (unparse-exp if-true) (unparse-exp else))]
      [if-no-else (cond if-true) (list 'if (unparse-exp cond) (unparse-exp if-true))])))

(define parse-binding
  (lambda (datum)
    (list (1st datum) (2nd datum))))

(define unparse-binding
  (lambda (exp)
    (cases binding exp
      [single-binding (sym val) ((unparse-exp sym) (unparse-exp val))])))

(define parse-lambda
  (lambda (datum)
    (print datum)
    (if (symbol? (2nd (cdr datum)))
        (lc-indef (parse-exp (2nd datum)) (map parse-exp (cddr datum)))
        (lc-mult (map parse-exp (2nd datum)) (map parse-exp (cddr datum))))))

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
        (and (list? x) (symbol? (car (list x)))))))

;;--------  Used by the testing mechanism   ------------------

(define-syntax nyi
  (syntax-rules ()
    ([_]
     [error "nyi"])))
