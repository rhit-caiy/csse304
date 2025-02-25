#lang racket

(require "../chez-init.rkt")
(provide typecheck)

(define-datatype type type?
  [number-t]
  [boolean-t]
  [proc-t (param type?) (ret type?)])

(define unparse-type
  (lambda (t)
    (if (eqv? t 'unknown-expression)
        'unknown-expression ; just allow our little error type to pass through
        (cases type t
          [number-t () 'num]
          [boolean-t () 'bool]
          [proc-t (p r) (list (unparse-type p) '-> (unparse-type r))]))))
            

(define parse-type
  (lambda (type-exp)
    (cond [(eqv? 'num type-exp) (number-t)]
          [(eqv? 'bool type-exp) (boolean-t)]
          [(and (list? type-exp)
                (= (length type-exp) 3)
                (eqv? '-> (second type-exp)))
           (proc-t (parse-type (first type-exp))
                   (parse-type (third type-exp)))]
          [else (error 'parse-type "unknown type ~s" type-exp)])))

(define-datatype expression expression?
  [var-exp (name symbol?)]
  [lit-exp (val (lambda(x) (or (number? x) (boolean? x))))]
  [if-exp (test-exp expression?) (then-exp expression?) (else-exp expression?)]
  [lam-exp (var symbol?) (ptype type?) (body expression?)]
  [letrec-exp (recurse-var symbol?) (ret-type type?) (lambda lam-exp?) (body expression?)]
  [app-exp (rator expression?) (rand expression?)])

; our letrec expression can only contain lambda initializers
(define lam-exp?
  (lambda (exp)
    (if (expression? exp)
        (cases expression exp
          [lam-exp (var ptype body) #t]
          [else #f])
        #f)))

(define parse
  (lambda (code)
    (cond [(symbol? code) (var-exp code)]
          [(number? code) (lit-exp code)]
          [(boolean? code) (lit-exp code)]
          [(list? code)
           (when (< (length code) 2) (error 'short-app "param list too short ~s" code))
           (if (symbol? (car code))
               (case (car code)
                 [(if) (unless (= (length code) 4) (error 'bad-if "bad if"))
                       
                       (if-exp (parse (second code))
                               (parse (third code))
                               (parse (fourth code)))]
                 [(lambda) (unless (= (length code) 4) (error 'bad-lambda "bad lambda"))
                           (let ([type (second code)]
                                 [param (third code)])
                             (unless (and
                                      (pair? param)
                                      (symbol? (car param)))
                               (error 'bad-param "bad lambda param ~s" (cadr code)))
                             (lam-exp (car param) (parse-type type) (parse (fourth code))))
                                 ]
                 [(letrec) (unless (= (length code) 5) (error 'bad-letrec "wrong length"))
                           (let [(ret (parse-type (second code)))
                                 (var (third code))
                                 (lam (parse (fourth code)))
                                 (body (parse (fifth code)))]
                             (unless (symbol? var) (error 'bad-lectrec "bad var"))
                             (unless (lam-exp? lam) (error 'bad-lectrec "lamdba required"))
                             (letrec-exp var ret lam body))]
                             
                 [else (parse-app code)])
               (parse-app code))]
           )))

(define parse-app
  (lambda (code)
    (app-exp (parse (first code))
                   (parse (second code)))))

(define typecheck
  (lambda (code)
    (unparse-type (typecheck-exp (parse code) '()))))

(define getenv
  (lambda (env a)
    (cond [(null? env) (print a) (raise 'unbound-var)]
          [(equal? (caar env) a) (cadar env)]
          [else (getenv (cdr env) a)])))

(define typecheck-exp
  (lambda (exp env)
    (cases expression exp
      [lit-exp (value) (if (number? value) (number-t) (boolean-t))]
      ; lots more expression types goe here
      [var-exp (name) (cond [(equal? name 'zero?) (proc-t (number-t) (boolean-t))]
                            [(equal? name '-) (proc-t (number-t) (proc-t (number-t) (number-t)))]
                            [else (getenv env name)])]
      [if-exp (test-exp then-exp else-exp) (cond [(not (equal? 'bool (unparse-type (typecheck-exp test-exp env)))) (raise 'bad-if-test)]
                                                 [(not (equal? (unparse-type (typecheck-exp then-exp env)) (unparse-type (typecheck-exp else-exp env)))) (raise 'bad-if-branches)]
                                                 [else (typecheck-exp then-exp env)])]
      [lam-exp (var ptype body) (proc-t ptype (typecheck-exp body (cons (list var ptype) env)))]
      [letrec-exp (recurse-var ret-type lambda body)
                  (if (equal? (car ret-type) (car (reverse (flatten (typecheck-exp lambda (cons (list recurse-var (proc-t (caddr lambda) ret-type)) env))))))
                      (typecheck-exp body (cons (list recurse-var (proc-t (caddr lambda) ret-type)) env))
                      (raise 'bad-letrec-types))]
      [app-exp (rator rand)
               (if (= 1 (length (typecheck-exp rator env)))
                           (raise 'bad-procedure)
                           (if (equal? (typecheck-exp rand env) (cadr (typecheck-exp rator env)))
                               (caddr (typecheck-exp rator env))
                               (raise 'bad-parameter)))]
      ; of course when you're finished this else should never happen
      [else 'unknown-expression] )))
(define expect-error
  (lambda (code)
    (with-handlers ([symbol? (lambda (x) x)])
      (typecheck code))))