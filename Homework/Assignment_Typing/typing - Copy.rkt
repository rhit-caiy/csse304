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
    (cond [(null? env) (raise 'unbound-var)]
          [(equal? (caar env) a) (cadar env)]
          [else (getenv (cdr env) a)])))

(define typecheck-exp
  (lambda (exp env)
    ;(print exp)
    ;(print env)
    (cases expression exp
      [lit-exp (value) (if (number? value) (number-t) (boolean-t))]
      ; lots more expression types goe here
      [var-exp (name) (cond [(equal? name 'zero?) (proc-t (number-t) (boolean-t))]
                            [(equal? name '-) (proc-t (number-t) (proc-t (number-t) (number-t)))]
                            [else (getenv env name)])]
                            ;[else (number-t)])](print (list (unparse-type (typecheck-exp then-exp env)) (unparse-type (typecheck-exp else-exp env))))
      [if-exp (test-exp then-exp else-exp) (cond [(not (equal? 'bool (unparse-type (typecheck-exp test-exp env)))) (raise 'bad-if-test)]
                                                 [(not (equal? (unparse-type (typecheck-exp then-exp env)) (unparse-type (typecheck-exp else-exp env)))) (raise 'bad-if-branches)]
                                                 [else (typecheck-exp then-exp env)])]
      [lam-exp (var ptype body) (proc-t ptype (typecheck-exp body (cons (list var ptype) env)))]
      [letrec-exp (recurse-var ret-type lambda body); (print ret-type) (print (typecheck-exp body (cons (list recurse-var ret-type) env)))
                  (typecheck-exp lambda (cons (list recurse-var ret-type) env))
                  (typecheck-exp body (cons (list recurse-var ret-type) env))]
                  ;(if (equal? ret-type (typecheck-exp body (cons (list recurse-var (getenv env (typecheck-exp recurse-var env))) env)))
                  ;                                       (typecheck-exp body (cons (list recurse-var ret-type) env))
                  ;                                       (raise 'bad-letrec-types))];(proc-t (ret-type)
      [app-exp (rator rand) (print (car (typecheck-exp rand env))) (print (car (flatten (list (car (typecheck-exp rator env)))))) (print '------------)
               (cond ;[(equal? 'num (unparse-type (typecheck-exp rator env))) (raise 'bad-procedure)]
                 [(equal? rator (parse '-)) (if (or (equal? 'lit-exp (car (typecheck-exp rand env))) (equal? 'number-t (car (typecheck-exp rand env))))
                                                (proc-t (number-t) (number-t))
                                                (raise 'bad-parameter))]
                 
                 [(equal? 'lit-exp (car rator)) (raise 'bad-procedure)]
                 ;[(equal? 'lam-exp (car rator)) ;(if (equal? (car (typecheck-exp rand env)) (car (flatten (list (car (typecheck-exp rator env))))))
                 ;                                   (typecheck-exp rator env)]
                                                ;    (raise 'bad-parameter))]
                 [(and (equal? 'app-exp (car rator)) (list? (car rator)) (equal? '- (cadar rator)) (not (equal? 'number-t (car (typecheck-exp rand env))))) (raise 'bad-procedure)]
                 ;[(and (not (null? (flatten (list (car (typecheck-exp rator env))))))
                 ;      (not (equal? (car (typecheck-exp rand env)) (car (flatten (list (car (typecheck-exp rator env))))))))
                 ; (raise 'bad-parameter)]
                 [else (typecheck-exp rator env) (if (equal? 'var-exp (car rator))
                                                     (if (equal? 'lit-exp (car (getenv env (cadr rator))))
                                                         #t
                                                         (raise 'bad-procedure))
                                                     #t)
                       (typecheck-exp rand env)])]
                       ;(if (typecheck-exp rator env)
                       ;    (proc-t (typecheck-exp rand (cons (list rator (typecheck-exp rator env)) env)) (typecheck-exp rator env))
                       ;    (typecheck-exp rand env))])]
      ; of course when you're finished this else should never happen
      [else 'unknown-expression] )))