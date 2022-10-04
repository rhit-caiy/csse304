#lang racket

(define-syntax my-let
  (syntax-rules ()
    [(_ ((x v) ...) e1 e2 ...)
    ((lambda (x ...) e1 e2 ...)
     v ...)]
    [(_ name ((x v) ...) e1 e2 ...)
     (letrec ((name (lambda (x ...) e1))) (name v ...))]
    ))

(define-syntax my-or
  (syntax-rules ()
    [(_) #f]
    [(_ exp) exp]
    [(_ e1 e2 ...)
     (let [(a e1)] (if a
         a
	 (my-or e2 ...)
	 ))]))

(define-syntax +=
  (syntax-rules ()
    ;[(+= args ...)
     ;(nyi)]))
    [(_ var value)
     (let ()
     (set! var (+ var value))
     var)]
    ))

(define-syntax return-first
  (syntax-rules ()
    [(_ exp exp2 ...)
     (let ([a exp]) (begin exp2 ...
            exp)
       a)]))


(define-datatype bintree bintree?
  (leaf-node
   (datum number?))
  (interior-node
   (key symbol?)
   (left-tree bintree?)
   (right-tree bintree?)))

(define bintree-to-list
  (lambda (a)
    (cases bintree a
      [leaf-node (datum) (list 'leaf-node datum)]
      [interior-node (key left-tree right-tree) (list 'interior-node key (bintree-to-list left-tree) (bintree-to-list right-tree))])))

(define max-interior
  (lambda (a)
    (car (mihelper a))))

;(list maxnodename maxvalue currentvalue)
;(define mihelper
;  (lambda (a)
;    (cases bintree a
;      [leaf-node (datum) (list '() datum datum)]
;      [interior-node (key left right) (let* [(leftreturn (mihelper left)) (rightreturn (mihelper right)) (leftmax (cadr leftreturn)) (rightmax (cadr rightreturn)) (leftcurrent (caddr leftreturn)) (rightcurrent (caddr rightreturn)) (thisvalue (+ leftcurrent rightcurrent))]
;                                        (cond [(and (< leftmax thisvalue) (< rightmax thisvalue)) [list key thisvalue thisvalue]]
;                                              [(and (null? (car leftreturn)) (null? (car rightreturn))) (list key thisvalue thisvalue)]
;                                              [(> leftmax rightmax) (if (null? (car leftreturn))
;                                                                        (if (< rightmax thisvalue)
;                                                                            (list key thisvalue thisvalue)
;                                                                            (list (car leftreturn) leftmax thisvalue))
;                                                                        (if (< leftmax thisvalue)
;                                                                            (list key thisvalue thisvalue)
;                                                                            (list (car leftreturn) leftmax thisvalue)))]
;                                              [else (if (null? (car rightreturn))
;                                                        (if (< leftmax thisvalue)
;                                                                            (list key thisvalue thisvalue)
;                                                                            (list (car rightreturn) rightmax thisvalue))
;                                                        (if (< rightmax (+ leftcurrent rightcurrent))
;                                                                            (list key thisvalue thisvalue)
;                                                                            (list (car rightreturn) rightmax thisvalue)))]))])))

(define mihelper
  (lambda (a)
    (cases bintree a
      [leaf-node (datum) (list '() datum datum)]
      [interior-node (key left right) (let* [(leftreturn (mihelper left)) (rightreturn (mihelper right)) (leftmax (cadr leftreturn)) (rightmax (cadr rightreturn)) (leftcurrent (caddr leftreturn)) (rightcurrent (caddr rightreturn)) (thisvalue (+ leftcurrent rightcurrent))]
                                        (cond [(and (< leftmax thisvalue) (< rightmax thisvalue)) [list key thisvalue thisvalue]]
                                              [(and (null? (car leftreturn)) (null? (car rightreturn))) (list key thisvalue thisvalue)]
                                              [(> leftmax rightmax) (if (null? (car leftreturn))
                                                                        (if (< thisvalue rightmax)
                                                                            (list (car rightreturn) rightmax thisvalue)
                                                                            (list key thisvalue thisvalue))
                                                                        (list (car leftreturn) leftmax thisvalue))]
                                              [else (if (null? (car rightreturn))
                                                        (if (< thisvalue leftmax)
                                                                            (list (car leftreturn) leftmax thisvalue)
                                                                            (list key thisvalue thisvalue))
                                                        (list (car rightreturn) rightmax thisvalue))]))])))




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
  [lambda-exp
   (exp lambda-variant?)]
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

(define-datatype lambda-variant lambda-variant?
  [lc-mult
   (args (list-of var-exp?))
   (body lambda-body?)]
  [lc-indef
   (arg var-exp?)
   (body lambda-body?)])

(define-datatype lambda-body lambda-body?
  [lbody-single
   (body expression?)]
  [lbody-multi
   (first expression?)
   (rest lambda-body?)])

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
          (parse-lambda datum)]
         [(or (eqv? (car datum) 'let)
              (eqv? (car datum) 'let*)
              (eqv? (car datum) 'letrec))
          (parse-let exp)]
         [else (app-exp (parse-exp (1st datum))
                        (parse-exp (2nd datum)))])]
      [else (error 'parse-exp "bad expression: ~s" datum)])))

(define unparse-exp
  (lambda (exp)
    (cases expression exp
      [var-exp (id) id]
      [lit-exp (data) data]
      [lambda-exp (exp) (unparse-lambda exp)]
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
      [let-basic (ids body) (list 'let (unparse-exp ids) (unparse-lambda-body body))]
      [let-named (name ids body) (list 'let (unparse-exp name) (unparse-exp ids) (unparse-lambda-body body))]
      [let-star (ids body) (list 'let* (unparse-exp ids) (unparse-lambda-body body))]
      [let-rec (ids body) (list 'letrec (unparse-exp ids) (unparse-lambda-body body))])))

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
    (if (var-exp? (2nd datum))
        (lc-indef (2nd datum) (if (= (length datum) 3)
                                  (parse-lambda-body (3rd datum))
                                  (parse-lambda-body (cddr datum))))
        (lc-mult (2nd datum) (if (= (length datum) 3)
                                  (parse-lambda-body (3rd datum))
                                  (parse-lambda-body (cddr datum)))))))

(define unparse-lambda
  (lambda (exp)
    (cases lambda-variant exp
      [lc-mult (args body) (list 'lambda (unparse-exp args) (unparse-lambda-body body))];
      [lc-indef (arg body) (list 'lambda arg (unparse-lambda-body body))])))

(define parse-lambda-body
  (lambda (datum)
    (nyi)))

(define unparse-lambda-body
  (lambda (exp)
    (cases lambda-body exp
      [lbody-single (body) (unparse-exp body)]
      [lbody-multi (first rest) (list (unparse-exp first) (unparse-lambda-body rest))])))

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
        (and (list? x) (lit? (car x))))))
;(define lit?
;  (lambda (x)
;    (or (number? x)
;        (string? x)
;        (vector? x)
;        (symbol? x)
;        (and (list? x) (symbol? (car (list (x))))))))

;;--------  Used by the testing mechanism   ------------------

(define-syntax nyi
  (syntax-rules ()
    ([_]
     [error "nyi"])))
