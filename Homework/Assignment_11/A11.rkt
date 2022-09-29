#lang racket

(require "../chez-init.rkt")
(provide my-let my-or += return-first bintree? leaf-node interior-node bintree-to-list max-interior parse-exp unparse-exp)

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

(define-datatype expression expression?
  [var-exp
   (id symbol?)]
  [lit-exp
   (data number?)]
  [lambda-exp
   (id symbol?)
   (body expression?)]
  [app-exp
   (rator expression?)
   (rand expression?)])

; Procedures to make the parser a little bit saner.
(define 1st car)
(define 2nd cadr)
(define 3rd caddr)

(define parse-exp         
  (lambda (datum)
    (cond
      [(symbol? datum) (var-exp datum)]
      [(number? datum) (lit-exp datum)]
      [(pair? datum)
       (cond
         [(eqv? (car datum) 'lambda)
          (lambda-exp (car (2nd  datum))
                      (parse-exp (3rd datum)))]
         [else (app-exp (parse-exp (1st datum))
                        (parse-exp (2nd datum)))])]
      [else (error 'parse-exp "bad expression: ~s" datum)])))

(define unparse-exp
  (lambda (exp)
    (nyi)))

; An auxiliary procedure that could be helpful.
(define var-exp?
  (lambda (x)
    (cases expression x
      [var-exp (id) #t]
      [else #f])))

;;--------  Used by the testing mechanism   ------------------

(define-syntax nyi
  (syntax-rules ()
    ([_]
     [error "nyi"])))
