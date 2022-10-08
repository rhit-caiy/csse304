#lang racket

(provide eval-one-exp)

(require eopl/eopl)

;-------------------+
;                   |
;   sec:DATATYPES   |
;                   |
;-------------------+

; parsed expression.  You'll probably want to replace this 
; code with your expression datatype from A11b

;; expression flavors
; [var-exp]
; [lit-exp]
; [lc-mult]
; [lc-indef]
; [let-basic]
; [let-named]
; [let-star]
; [let-rec]
; [if-else]
; [if-no-else]
; [set-exp]
; [app-exp]

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
  [let-basic
   (ids list?)
   (body list?)]
  [let-named
   (name var-exp?)
   (ids list?)
   (body list?)]
  [let-star
   (ids list?)
   (body list?)]
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
  [set-exp
   (var var-exp?)
   (set-to expression?)]
  [app-exp
   (rator expression?)
   (rand (list-of expression?))])
	

;; environment type definitions

(define scheme-value?
  (lambda (x) #t))
  
(define-datatype environment environment?
  [empty-env-record]
  [extended-env-record
   (syms (list-of symbol?))
   (vals (list-of scheme-value?))
   (env environment?)])


; datatype for procedures.  At first there is only one
; kind of procedure, but more kinds will be added later.

(define-datatype proc-val proc-val?
  [prim-proc
   (name symbol?)]
  [closure
   (args (list-of var-exp?))
   (code lambda-body?)
   (env environment?)])
  
;-------------------+
;                   |
;    sec:PARSER     |
;                   |
;-------------------+

; This is a parser for simple Scheme expressions, such as those in EOPL 3.1 thru 3.3.

; You will want to replace this with your parser that includes more expression types, more options for these types, and error-checking.

; Helper procedures to make the parser a little bit saner.
(define 1st car)
(define 2nd cadr)
(define 3rd caddr)
(define 4th cadddr)

; Again, you'll probably want to use your code from A11b

(define parse-exp         
  (lambda (datum)
    (if (not (check datum))
        (error 'parse-exp "bad expression: ~s" datum)
    (cond
      [(symbol? datum) (var-exp datum)]
      [(number? datum) (lit-exp datum)]
      [(string? datum) (lit-exp datum)]
      [(vector? datum) (lit-exp datum)]
      [(boolean? datum) (lit-exp datum)]
      [(pair? datum)
       (cond
         [(eqv? (car datum) 'quote) (lit-exp (cadr datum))]
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

; Additional subtype parsing
(define parse-lambda
  (lambda (datum)
    (if (list? (2nd datum))
        (lc-mult (map parse-exp (2nd datum)) (map parse-exp (cddr datum)))
        (lc-indef (parse-exp (2nd datum)) (map parse-exp (cddr datum))))))

(define parse-if
  (lambda (datum)
    (if (= (length datum) 4)
        (if-else (parse-exp (2nd datum)) (parse-exp (3rd datum)) (parse-exp (4th datum)))
        (if-no-else (parse-exp (2nd datum)) (parse-exp (3rd datum))))))

(define parse-let
  (lambda (datum)
    (cond [(and (eqv? (car datum) 'let) (symbol? (2nd datum))) (let-named (parse-exp (cadr datum)) (map (lambda (b) (cons (parse-exp (car b)) (list (parse-exp (cadr b))))) (caddr datum)) (map parse-exp (cdddr datum)))]
          [(eqv? (car datum) 'let) (let-basic (map (lambda (b) (append (list (parse-exp (car b))) (list (parse-exp (cadr b))))) (cadr datum)) (map parse-exp (cddr datum)))]
          [(eqv? (car datum) 'let*) (let-star (map (lambda (b) (cons (parse-exp (car b)) (list (parse-exp (cadr b))))) (cadr datum)) (map parse-exp (cddr datum)))]
          [(eqv? (car datum) 'letrec) (let-rec (map (lambda (b) (cons (parse-exp (car b)) (list (parse-exp (cadr b))))) (cadr datum)) (map parse-exp (cddr datum)))])))

; Unparsing
(define unparse-exp
  (lambda (exp)
    (cases expression exp
      [var-exp (id) id]
      [lit-exp (data) data]
      [lc-mult (exp1 exp2) (cons 'lambda (cons (map unparse-exp exp1) (map unparse-exp exp2)))]
      [lc-indef (exp1 exp2) (cons 'lambda (cons (unparse-exp exp1) (map unparse-exp exp2)))]
      [let-basic (ids body) (unparse-let exp)]
      [let-star (ids body) (unparse-let exp)]
      [let-named (name ids body) (unparse-let exp)]
      [let-rec (ids body) (unparse-let exp)]
      [if-else (a b c) (unparse-if exp)]
      [if-no-else (exp1 exp2) (unparse-if exp)]
      [set-exp (var exp) (list 'set! (unparse-exp var) (unparse-exp exp))]
      [app-exp (rator rand) (cons (unparse-exp rator) (map unparse-exp rand))]
      [else (map unparse-exp exp)]
      )))

; Additional subtype unparsing
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

(define unparse-if
  (lambda (exp)
    (cases expression exp
      [if-else (cond if-true else) (list 'if (unparse-exp cond) (unparse-exp if-true) (unparse-exp else))]
      [if-no-else (cond if-true) (list 'if (unparse-exp cond) (unparse-exp if-true))]
      [else '()])))

; Error Checking
(define check
  (lambda (a)
    (cond [(or (symbol? a) (number? a) (string? a)) #t]
          [(null? a) #t]
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
    (cond [(not (list? a)) #f]
          [(null? (cdr a)) #f]
          [(symbol? (car a)) (checklet (cdr a))]
          [(not (null? (car a))) (and (list? (car a)) (list? (cdr a)) (andmap (lambda (b) (and (list? b) (= (length b) 2) (symbol? (car b)))) (car a)))]
          [else #t])))

; Some parser expression type predicates for convenience
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
        (list? x)
        (pair? x))))

(define lambda-body?
  (lambda (a)
    (or (symbol? a) (list? a))))


;-------------------+
;                   |
; sec:ENVIRONMENTS  |
;                   |
;-------------------+


; Environment definitions for CSSE 304 Scheme interpreter.  
; Based on EoPL sections 2.2 and 2.3

(define empty-env
  (lambda ()
    (empty-env-record)))

(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms vals env)))

(define list-find-position
  (lambda (sym los)
    (let loop ([los los] [pos 0])
      (cond [(null? los) #f]
            [(eq? sym (car los)) pos]
            [else (loop (cdr los) (add1 pos))]))))
	    
(define apply-env
  (lambda (env sym) 
    (cases environment env 
      [empty-env-record ()      
                        (error 'env "variable ~s not found." sym)]
      [extended-env-record (syms vals env)
                           (let ((pos (list-find-position sym syms)))
                             (if (number? pos)
                                 (list-ref vals pos)
                                 (apply-env env sym)))])))


;-----------------------+
;                       |
;  sec:SYNTAX EXPANSION |
;                       |
;-----------------------+

; To be added in assignment 14.

;---------------------------------------+
;                                       |
; sec:CONTINUATION DATATYPE and APPLY-K |
;                                       |
;---------------------------------------+

; To be added in assignment 18a.


;-------------------+
;                   |
;  sec:INTERPRETER  |
;                   |
;-------------------+

; top-level-eval evaluates a form in the global environment

(define top-level-eval
  (lambda (form)
    ; later we may add things that are not expressions.
    (eval-exp form init-env)))

; eval-exp is the main component of the interpreter


(define eval-exp
  (lambda (exp env)
    (cases expression exp
      [lc-mult (args body) (closure args body env)]
      [lc-indef (args body) (closure 'args body)]
      [let-basic (ids body)
                                       ;(set! env (extend-env (map unparse-exp (apply list (map car ids))) (map (lambda (a) (eval-exp a env)) (apply list (map cadr ids))) env))
                                       ;(pretty-print env)
                                       ;(map (lambda (a) (eval-exp a env)) body)]
                 (let [(newenv (extend-env (map unparse-exp (apply list (map car ids))) (map unparse-exp (apply list (map cadr ids))) env))]
                   (pretty-print newenv)
                              ;(print newenv)
                              ;(print "--------------------------------------------")
                              ;(print (car body))
                              ;(print (eval-exp (car body) newenv))
                              (if (= (length body) 1)
                                  (eval-exp (car body) newenv)
                                  (map (lambda (a) (eval-exp a newenv)) body)))]
      ;[let-basic (ids body) (eval-exp (car body) (extend-env (map unparse-exp (apply list (map car ids))) (map unparse-exp (apply list (map cadr ids))) env))]
      
      [if-else
       (cond if-true else)
       (if (eval-exp cond env)
           (eval-exp if-true env)
           (eval-exp else env))]
      [if-no-else
       (cond if-true)
       (if (eval-exp cond env)
           (eval-exp if-true env)
           #f)]
      [set-exp (var set-to) (set! var set-to)]
      [lit-exp (datum) datum]
      [var-exp (id)
               (apply-env env id)]
      [app-exp (rator rands)
               (let ([proc-value (eval-exp rator env)]
                     [args (eval-rands rands env)])
                 (apply-proc proc-value args))]
      [else (error 'eval-exp "Bad abstract syntax: ~a" exp)])))

; evaluate the list of operands, putting results into a list

(define eval-rands
  (lambda (rands env)
    (map (lambda (a) (eval-exp a env)) rands)))

;  Apply a procedure to its arguments.
;  At this point, we only have primitive procedures.  
;  User-defined procedures will be added later.

(define apply-proc
  (lambda (proc-value arg-vals)
    (cases proc-val proc-value
      [prim-proc (op) (apply-prim-proc op arg-vals)]
      [closure (args code env) (apply-closure args arg-vals code env)]
      ; You will add other cases
      [else (error 'apply-proc
                   "Attempt to apply bad procedure: ~s" 
                   proc-value)])))

(define *prim-proc-names* '(+ - * / add1 sub1 cons not = >= <= > < cons car cdr caar cadr cdar cddr caaar caadr cadar caddr cdaar cdadr cddar cdddr list null? eq? equal? atom? assq length list->vector make-vector vector-ref vector-set! list? pair? vector->list vector? number? zero? symbol? procedure? display newline))

(define init-env         ; for now, our initial global environment only contains 
  (extend-env            ; procedure names.  Recall that an environment associates
   *prim-proc-names*   ;  a value (not an expression) with an identifier.
   (map prim-proc      
        *prim-proc-names*)
   (empty-env)))

;(define current-env
;  (empty-env
;   ))

; Usually an interpreter must define each 
; built-in procedure individually.  We are "cheating" a little bit.

(define apply-prim-proc
  (lambda (prim-proc args)
    (case prim-proc
      ;[(+) (+ (1st args) (2nd args))]
      ;[(-) (- (1st args) (2nd args))]
      ;[(*) (* (1st args) (2nd args))]
      [(+) (apply + args)]
      [(-) (apply - args)]
      [(*) (apply * args)]
      [(/) (apply / args)]
      [(add1) (+ (1st args) 1)]
      [(sub1) (- (1st args) 1)]
      [(cons) (cons (1st args) (2nd args))]
      [(not) (not (1st args))]
      [(=) (= (1st args) (2nd args))]
      [(>=) (>= (1st args) (2nd args))]
      [(<=) (<= (1st args) (2nd args))]
      [(>) (> (1st args) (2nd args))]
      [(<) (< (1st args) (2nd args))]
      [(car) (car (1st args))]
      [(cdr) (cdr (1st args))]
      [(caar) (caar (1st args))]
      [(cadr) (cadr (1st args))]
      [(cdar) (cdar (1st args))]
      [(cddr) (cddr (1st args))]
      [(caaar) (caaar (1st args))]
      [(caadr) (caadr (1st args))]
      [(cadar) (cadar (1st args))]
      [(caddr) (caddr (1st args))]
      [(cdaar) (cdaar (1st args))]
      [(cdadr) (cdadr (1st args))]
      [(cddar) (cddar (1st args))]
      [(cdddr) (cdddr (1st args))]
      [(list) (apply list args)]
      [(null?) (null? (1st args))]
      [(eq?) (eq? (1st args) (2nd args))]
      [(equal?) (equal? (1st args) (2nd args))]
      [(length) (length (1st args))]
      [(list?) (list? (1st args))]
      [(pair?) (pair? (1st args))]
      [(vector?) (vector? (1st args))]
      [(number?) (number? (1st args))]
      [(symbol?) (symbol? (1st args))]
      [(zero?) (zero? (1st args))]
      ;[(atom?) (atom? (1st args))]
      [(assq) (assq (1st args))]
      [(procedure?) (proc-val? (1st args))]
      [(list->vector) (list->vector (1st args))]
      [(vector->list) (vector->list (1st args))]
      [(make-vector) (make-vector (1st args))]
      [(vector-ref) (vector-ref (1st args) (2nd args))]
      [(vector-set!) (vector-set! (1st args) (2nd args) (3rd args))]
      [(display) (apply display args)]
      [(newline) (apply newline args)]
      [else (error 'apply-prim-proc 
                   "Bad primitive procedure name: ~s" 
                   prim-proc)])))

(define apply-closure
  (lambda (args vals code env)
    (closure args code env)))
;(define apply-closure
;  (lambda (args vals code env)
;    (set! temp-env current-env)
;    (set! current-env (extend-env (map unparse-exp args) vals env))
;    (pretty-print current-env)
;    (pretty-print temp-env)
;    (pretty-print env)
;    (let ([result (map eval-exp code)])
;      (set! current-env temp-env)
;      result)))
(define rep      ; "read-eval-print" loop.
  (lambda ()
    (display "8==D ")
    ;; notice that we don't save changes to the environment...
    (let ([answer (top-level-eval (parse-exp (read)))])
      ;; TODO: are there answers that should display differently?
      (pretty-print answer) (newline)
      (rep))))  ; tail-recursive, so stack doesn't grow.

(define eval-one-exp
  (lambda (x) (top-level-eval (parse-exp x))))
