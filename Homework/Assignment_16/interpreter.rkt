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
; [var-exp] (id)
; [lit-exp] (data)
; [lc-mult] (args body)
; [lc-indef] (arg body)
; [let-basic] (ids body)
; [let-named] (name ids body)
; [let-star] (ids body)
; [let-rec] (ids body)
; [if-else]
; [if-no-else]
; [set-exp]
; [app-exp]
; [cond-exp] (body)
; [and-exp] (body)
; [or-exp] (body)
; [begin-exp] (body)
; [while-exp] (body)

(define-datatype expression expression?
  [var-exp
   (id symbol?)]
  [lit-exp
   (data lit?)]
  [lc-mult
   (args (list-of expression?))
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
   (proc-names (list-of symbol?))
   (idss (list-of (list-of symbol?)))
   (bodiess (list-of (list-of expression?)))
   (letrec-bodies (list-of expression?))]
   ;(ids list?)
   ;(body list?)]
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
  [set!-exp
    (id var-exp?)
    (body expression?)]
  [app-exp
   (rator expression?)
   (rand (list-of expression?))]
  [cond-exp
   (body (list-of expression?))]
  [and-exp
   (body (list-of expression?))]
  [or-exp
   (body (list-of expression?))]
  [begin-exp
    (body (list-of expression?))]
  [while-exp
   (test expression?)
   (body (list-of expression?))])
	

;; environment type definitions

(define scheme-value?
  (lambda (x) #t))
  
(define-datatype environment environment?
  [empty-env-record]
  [extended-env-record
   (syms (list-of symbol?))
   (vals (list-of scheme-value?))
   (env environment?)]
  [recursively-extended-env-record
   (proc-names (list-of symbol?))
   (idss (list-of (list-of symbol?)))
   (bodiess (list-of (list-of expression?)))
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
             [(eqv? (car datum) 'and) (and-exp (map parse-exp (cdr datum)))]
             [(eqv? (car datum) 'or) (or-exp (map parse-exp (cdr datum)))]
             [(eqv? (car datum) 'cond) (cond-exp (map parse-exp (cdr datum)))]
             [(eqv? (car datum) 'begin) (begin-exp (map parse-exp (cdr datum)))]
             [(eqv? (car datum) 'while) (while-exp (parse-exp (2nd datum)) (map parse-exp (cddr datum)))]
             [(eqv? (car datum) 'lambda)
              (if (< (length datum) 3)
                  (error 'parse-error "error")
                  (parse-lambda datum))]
             [(or (eqv? (car datum) 'let)
                  (eqv? (car datum) 'let*)
                  (eqv? (car datum) 'letrec))
              (parse-let datum)]
             [(not (list? datum)) (if (symbol? (cdr datum))
                                      (list (parse-exp (1st datum)) (parse-exp (cdr datum)))
                                      (app-exp (parse-exp (1st datum)) (parse-exp (cdr datum))))]
             [(= 2 (length datum)) (app-exp (parse-exp (1st datum)) (map parse-exp (cdr datum)))]
             [else (app-exp (parse-exp (car datum)) (map parse-exp (cdr datum)))])]
          [else (error 'parse-exp "bad expression: ~s" datum)]))))

; Additional subtype parsing
(define parse-lambda
  (lambda (datum)
    ;(print datum)
    (if (list? (2nd datum))
        (lc-mult (map parse-exp (2nd datum)) (map parse-exp (cddr datum)))
        (if (pair? (2nd datum))
            (parse-lambda (cons 'lambda (cons (pairtolist (2nd datum)) (cddr datum))))
            (lc-indef (parse-exp (2nd datum)) (map parse-exp (cddr datum)))))))

(define pairtolist
  (lambda (datum)
    (if (symbol? (cdr datum))
        (list (1st datum) (cdr datum))
        (cons (1st datum) (pairtolist (cdr datum))))))

(define parse-if
  (lambda (datum)
    (if (= (length datum) 4)
        (if-else (parse-exp (2nd datum)) (parse-exp (3rd datum)) (parse-exp (4th datum)))
        (if-no-else (parse-exp (2nd datum)) (parse-exp (3rd datum))))))

(define parse-let
  (lambda (datum)
    ;(print datum)
    (cond [(and (eqv? (car datum) 'let) (symbol? (2nd datum))) (let-named (parse-exp (cadr datum)) (map (lambda (b) (cons (parse-exp (car b)) (list (parse-exp (cadr b))))) (caddr datum)) (map parse-exp (cdddr datum)))]
          [(eqv? (car datum) 'let) (let-basic (map (lambda (b) (append (list (parse-exp (car b))) (list (parse-exp (cadr b))))) (cadr datum)) (map parse-exp (cddr datum)))]
          [(eqv? (car datum) 'let*) (let-star (map (lambda (b) (cons (parse-exp (car b)) (list (parse-exp (cadr b))))) (cadr datum)) (map parse-exp (cddr datum)))]
          ;[(eqv? (car datum) 'letrec) (let-rec (map (lambda (b) (cons (parse-exp (car b)) (list (parse-exp (cadr b))))) (cadr datum)) (map parse-exp (cddr datum)))]
          [(eqv? (car datum) 'letrec) (let-rec (map car (cadr datum))
                                               (map cadr (map cadr (cadr datum)))
                                               (map list (map parse-exp (map caddr (map cadr (cadr datum)))))
                                               (map parse-exp (cddr datum)))]
          )))

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
      ;[let-rec (ids body) (unparse-let exp)]
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
      ;[let-rec (ids body) (append (list 'letrec) (list (map (lambda (b) (list (unparse-exp (car b)) (unparse-exp (cadr b)))) ids)) (map unparse-exp body))]
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
    #t))
    ;(cond [(or (symbol? a) (number? a) (string? a)) #t]
    ;      [(null? a) #t]
    ;      [(list? a) (cond [(eqv? (car a) 'if) (if (or (= 3 (length a)) (= 4 (length a))) #t #f)]
    ;                       [(eqv? (car a) 'lambda) #t];(if (and #f (or (symbol? (cadr a)) (= 0 (length (filter-not symbol? (cadr a)))))) #t #f)]
    ;                       [(eqv? (car a) 'set!) (= 3 (length a))]
    ;                       [(eqv? (car a) 'let) (checklet (cdr a))]
    ;                       [(eqv? (car a) 'letrec) (checklet (cdr a))]
    ;                       [(eqv? (car a) 'let*) (checklet (cdr a))]
    ;                       [else #t])]
    ;      [(pair? a) #f])
    ;))

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

;(define extend-env
;  (lambda (syms vals env)
;    (extended-env-record syms vals env)))


(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms (_box-vals vals) env)))

(define _box-vals
  (lambda (vals)
    (let hlp ([vals vals]
              [ret '()])
      (cond [(null? vals) (reverse ret)]
            [else (hlp (cdr vals) (cons (box (car vals)) ret))]))))

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
                                 (apply-env env sym)))]
      [recursively-extended-env-record (procnames idss bodiess old-env)
                                     (let ([pos (list-find-position sym procnames)])
                                       (if (number? pos)
                                           (closure (list-ref idss pos)
                                                    (list-ref bodiess pos)
                                                    env)
                                           (apply-env old-env sym)))]
      )))


;-----------------------+
;                       |
;  sec:SYNTAX EXPANSION |
;                       |
;-----------------------+

; To be added in assignment 14.
(define syntax-expand
  (lambda (exp)
    (cases expression exp
      [let-basic (ids body)
                 (app-exp (lc-mult (map 1st ids) (map syntax-expand body)) (map syntax-expand (map 2nd ids)))]
      [let-star (ids body)
                (s/e-letstar (cdr ids) body (1st (1st ids)) (2nd (1st ids)))]
      [let-named (name ids body)
                 (s/e-let-named name ids body)] ;; TODO: Assignment 16 (likely along with adding "define")
      ;[let-rec (ids body)
      ;         (s/e-letrec (cdr ids) body (1st (1st ids)) (2nd (1st ids)))] ;; ????
      [let-rec (proc-names idss bodiess letrec-bodies)
               (s/e-letrec proc-names idss bodiess letrec-bodies)]
      [cond-exp (body)
                (s/e-cond (1st body) (cdr body))]
      [and-exp (body)
               (s/e-and body)]
      [or-exp (body)
              (s/e-or body)]
      [begin-exp (body)
                 (s/e-begin body)]
      [lc-mult (args body)
               (lc-mult args (map syntax-expand body))]
      [lc-indef (arg body)
                (lc-mult (list arg) (map syntax-expand body))]
      [while-exp (test body)
                 (while-exp (syntax-expand test) (map syntax-expand body))]
      [app-exp (rator rands)
                   (app-exp (syntax-expand rator) (map syntax-expand rands))]
      [if-else (test if-true else)
               (if-else (syntax-expand test) (syntax-expand if-true) (syntax-expand else))]
      [if-no-else (test if-true)
               (if-no-else (syntax-expand test) (syntax-expand if-true))]
      [else exp])))

; [let-basic] (ids body)                [x] [x]
; [let-named] (name ids body)           [N/A] [N/A]
; [let-star] (ids body)                 [x] [x]
; [let-rec] (ids body)                  [N/A] [N/A]
; [cond-exp] (body)                     [x] [x]
; [and-exp] (body)                      [x] [x]
; [or-exp] (body)                       [x] [x]
; [begin-exp] (body)                    [x] [x]
; [while-exp] (body)                    [ ] [ ]

; some procedures to syntax expand specific expression types
(define s/e-letstar
  (lambda (ids body cur-var cur-val)
    (cond [(null? ids) (app-exp (lc-mult (list cur-var) (map syntax-expand body)) (list (syntax-expand cur-val)))]
          [else (app-exp (lc-mult (list cur-var) (list (s/e-letstar (cdr ids) body (1st (1st ids)) (2nd (1st ids))))) (list (syntax-expand cur-val)))])))



;let basic
;(app-exp (lc-mult (map 1st ids) (map syntax-expand body)) (map syntax-expand (map 2nd ids)))

;------------------------------------------------------------------------------------------------------------------------------------------------------------------
(define s/e-letnamed
  (lambda (name ids body)
    (syntax-expand (app-exp (syntax-expand (lc-mult (list name) (list (syntax-expand (let-basic ids body))))) (list (lc-mult (map car ids) body))))))
(define s/e-let-named
  (lambda (name ids body)
    (syntax-expand (let-basic (list (list name (lit-exp #f))) (list (let-basic ids (cons (set-exp (unparse-exp name) (lc-mult (map 1st ids) body)) body)))))))
(require racket/trace)
;(trace s/e-letnamed)
;(trace syntax-expand)

;(s/e-letrec (cdr ids) body (1st (1st ids)) (2nd (1st ids)))
;(define s/e-letrec 
;  (lambda (proc-names idss bodiess letrec-bodies)
;    (print (caar bodiess))
;    (let-rec proc-names
;                idss
;                (list (map syntax-expand (car bodiess)))
;                (map syntax-expand letrec-bodies))))
    ;(lc-mult '() (list (set! cur-var cur-val) (app-exp (lc-mult (map 1st ids) (map syntax-expand body)) (map syntax-expand (map 2nd ids)))))))
;(s/e-letrec proc-names idss (map (lambda (x) (map syntax-expand x)) bodiess) (syntax-expand letrec-bodies))))

(define s/e-letrec
  (lambda (proc-names idss bodiess letrec-bodies)
    (syntax-expand (let-basic (map (lambda (proc-name) (list proc-name (lit-exp #f))) proc-names)
                              (append (map (lambda (proc-name ids body) (set-exp (unparse-exp proc-name) (lc-mult ids body))) proc-names idss bodiess) letrec-bodies)))))



(define s/e-cond
  (lambda (cur-exp remaining)
    (cond
      [(null? remaining)
       (cases expression cur-exp
         [app-exp (rator rands)
                  (cases expression rator
                    [var-exp (id)
                             (if (eqv? id 'else)
                                 (if (= (length rands) 1)
                                     (syntax-expand (1st rands))
                                     (app-exp (syntax-expand (1st rands)) (map syntax-expand (cdr rands))))
                                 (if-no-else (syntax-expand rator) (if (= (length rands) 1)
                                                                       (syntax-expand (1st rands))
                                                                       (app-exp (syntax-expand (1st rands)) (map syntax-expand (cdr rands))))))]
                    [else (if-no-else (syntax-expand rator) (if (= (length rands) 1)
                                                                (syntax-expand (1st rands))
                                                                (app-exp (syntax-expand (1st rands)) (map syntax-expand (cdr rands)))))])]
         [else (error 'syntax-cond "bad syntax in cond")])]
      [else
       (cases expression cur-exp
         [app-exp (rator rands)
                  (if-else (syntax-expand rator)
                           (if (= (length rands) 1)
                               (1st rands)
                               (app-exp (syntax-expand (1st rands)) (map syntax-expand (cdr rands))))
                           (s/e-cond (1st remaining) (cdr remaining)))]
         [else (error 'syntax-cond "bad syntax in cond")])])))

(define s/e-and
  (lambda (body)
    (cond [(null? body) (lit-exp #f)]
          [(null? (cdr body)) (if-else (syntax-expand (1st body)) (syntax-expand (1st body)) (lit-exp #f))]  
          [else (if-else (syntax-expand (1st body)) (s/e-and (cdr body)) (lit-exp #f))])))

(define s/e-or
  (lambda (body)
    (cond [(null? body) (lit-exp #f)]
          [(null? (cdr body)) (let ((a (syntax-expand (1st body)))) (if a
                                                                        a
                                                                       (lit-exp #f)))]
          [else (if-else (syntax-expand (1st body)) (syntax-expand (1st body)) (s/e-or (cdr body)))])))

(define s/e-begin
  (lambda (body)
    (app-exp (lc-mult '() (map syntax-expand body)) '())))



(define s/e-lcnfix
  (lambda (args body values);(parse-exp '(lambda (a b) (+ a b) 1 2)) parse into (lc-mult (list args) (list body value1 ...)), var is car of lc-mult, body is caadr, and values is cdadr
    (lc-mult args (cons body (lasttolist values (length args))))));maybe pair length is 1? In that case, change 1 in lasttolist to 0

(define lasttolist
  (lambda (values argslength)
    (cond [(= 1 argslength) (list values)]
          [else (cons (car values) (lasttolist (cdr values) (- argslength 1)))])))

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
      [lc-indef (arg body) (closure (list arg) body env)]
      [if-else
       (cond if-true else)
       (if (eval-exp cond env)
           (eval-exp if-true env)
           (eval-exp else env))]
      [if-no-else
       (cond if-true)
       (if (eval-exp cond env)
           (eval-exp if-true env)
           (void))]
      [set-exp (var set-to) (set! var set-to)]
      [lit-exp (datum) datum]
      [var-exp (id) ;(apply-env env id)]
               (with-handlers ([exn:fail? (lambda (exn) (unbox (apply-env init-env id)))]) (unbox (apply-env env id)))]
      [app-exp (rator rands)
               (let ([proc-value (eval-exp rator env)]
                     [args (eval-rands rands env)])
                 (apply-proc proc-value args))] ;; Need to CAR internally, but not on last iteration?
      [while-exp (test body)
                 (apply-while test body env)]
      [let-rec (proc-names idss bodiess letrec-bodies)
                  (eval-exp letrec-bodies
                               (extend-env-recursively
                               proc-names idss bodiess env))]
      [else (error 'eval-exp "Bad abstract syntax: ~a" exp)])))

(define apply-while
  (lambda (test body env)
    (if (eval-exp test env)
        (apply-while-hlp test body env (map (lambda (a) (eval-exp a env)) body))
        (void))))

(define apply-while-hlp
  (lambda (test body env prev)
    (if (eval-exp test env)        
        (apply-while-hlp test body env (map (lambda (a) (eval-exp a env)) body))
        prev)))

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
      [closure (args code env) (apply-closure args arg-vals code (extend-env (map unparse-exp args) arg-vals env))]
      ;[lc-closure (args code env) (apply-closure args arg-vals code (extend-env (map unparse-exp args) arg-vals env))]
      ; You will add other cases
      [else (error 'apply-proc
                   "Attempt to apply bad procedure: ~s" 
                   proc-value)])))

(define *prim-proc-names* '(map apply procedure? + - * / quotient positive? negative? add1 sub1 cons not = >= <= > < cons car cdr caar cadr cdar cddr caaar caadr cadar caddr cdaar cdadr cddar cdddr list null? eq? equal? atom? assq length list->vector make-vector vector-ref vector-set! list? pair? vector->list vector? number? zero? symbol? display newline vector define set!))

(define init-env         ; for now, our initial global environment only contains 
  (extend-env            ; procedure names.  Recall that an environment associates
   *prim-proc-names*   ;  a value (not an expression) with an identifier.
   (map prim-proc      
        *prim-proc-names*)
   (empty-env)))

;(define current-env
;  (empty-env
;   ))

;(define temp-env
;  (empty-env))

; Usually an interpreter must define each 
; built-in procedure individually.  We are "cheating" a little bit.

(define apply-prim-proc
  (lambda (prim-proc args)
    (case prim-proc
      [(+) (apply + args)]
      [(-) (apply - args)]
      [(*) (apply * args)]
      [(/) (apply / args)]
      [(quotient) (quotient (1st args) (2nd args))]
      [(positive?) (positive? (1st args))]
      [(negative?) (negative? (1st args))]
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
      [(vector) (apply vector args)]
      [(map) (apply map (lambda (x) (apply-proc (1st args) (list x))) (cdr args))]
      [(apply) (apply apply-proc args)]
      ;[(define) (define (1st args) (2nd args))]
      ;[(set!) (set! (1st args) (2nd args))]
      [else (error 'apply-prim-proc 
                   "Bad primitive procedure name: ~s" 
                   prim-proc)])))

;(define apply-closure
;  (lambda (args vals code env)
;    (car (reverse (map (lambda (a) (eval-exp a (extend-env-helper (map unparse-exp args) vals env))) code)))))
(define apply-closure
  (lambda (args vals code env)
    (let ([new-env (extend-env-helper (map unparse-exp args) vals env)])
      (car (reverse (map (lambda (a) (eval-exp a new-env)) code))))))

(define extend-env-helper
  (lambda (args vals env)
    (if (pair? args)
        (if (and (not (null? (cdr vals))) (null? (cdr args)))
            (extend-env args (list vals) env)
            (extend-env-helper (cdr args) (cdr vals) (extend-env (list (car args)) (list (car vals)) env)))
        (extend-env args vals env))))

(define extend-env-recursively
  (lambda (proc-names idss bodiess old-env)
    (let* ([len (length proc-names)] [vec (make-vector len)] [env (extended-env-record proc-names vec old-env)])
      (for-each
       (lambda (pos ids bodies)
         (vector-set! vec pos (closure ids bodies env)))
       (iota len) idss bodiess)
      env)))

(define iota
  (lambda (len)
    (if (= len 0)
        '()
        (append (iota (- len 1)) (list (- len 1))))))

(define rep      ; "read-eval-print" loop.
  (lambda ()
    (display "8===D ")
    ;; notice that we don't save changes to the environment...
    (let ([answer (top-level-eval (syntax-expand (parse-exp (read))))])
      ;; TODO: are there answers that should display differently?
      (pretty-print answer) (newline)
      (rep))))  ; tail-recursive, so stack doesn't grow.

(define eval-one-exp
  (lambda (x)
    (top-level-eval (syntax-expand (parse-exp x)))))

;(define last
;  (lambda (lst)
;    (cond [(null? lst) lst]
;          ;[(= (length lst) 1) (car lst)]
;          [else (car (reverse lst))])))

;---------------------------------------+
;                                       |
; sec:TESTING DEFINITIONS               |
;                                       |
;---------------------------------------+

; Simple exps
(define simple-lambda
  (parse-exp '(lambda (x) x)))
(define simple-let
  (parse-exp '(let ([a 1] [b 2]) (+ a b))))
(define simple-and
  (parse-exp '(and #t 1)))
(define simple-and-2
  (parse-exp '(and #t #f)))
(define simple-or
  (parse-exp '(or #f #t)))
(define simple-or-2
  (parse-exp '(or #f #f)))
(define simple-letstar
  (parse-exp '(let* ([a 1] [b (+ a 1)]) (+ a b))))
(define simple-begin
  (parse-exp '(begin (+ 1 1)
                     (- 1 1)
                     (* 10 1))))

; Nested exps
(define nested-let
  (parse-exp '(let ([a 1]) (let ([b 2]) (+ a b)))))
(define nested-letstar
  (parse-exp '(let* ([a 1] [b (+ a 1)]) (and 1 1))))
