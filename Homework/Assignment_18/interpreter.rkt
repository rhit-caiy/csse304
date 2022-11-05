#lang racket

(provide eval-one-exp reset-global-env)

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
   (proc-names (list-of var-exp?))
   (idss (list-of (list-of var-exp?)))
   (bodiess (list-of (list-of expression?)))
   (letrec-bodies (list-of expression?))]
  [if-else
   (cond expression?)
   (if-true expression?)
   (else expression?)]
  [if-no-else
   (cond expression?)
   (if-true expression?)]
  [set-exp
   (var symbol?)
   (set-to expression?)]
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
   (body (list-of expression?))]
  [define-exp
    (id symbol?)
    (body expression?)]
  )
	

;; environment type definitions

(define scheme-value?
  (lambda (x) #t))
  
(define-datatype environment environment?
  [empty-env-record]
  [extended-env-record
   (syms (list-of symbol?))
   (vals (list-of box?))
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
   (env environment?)]
  [k-proc (k continuation?)])
  
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
             [(eqv? (car datum) 'set!)
              (if (not (= (length datum) 3))
                  (error 'parse-error "invalid set expression in: ~s" datum)
                  (set-exp (2nd datum) (parse-exp (3rd datum))))]
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
             [(eqv? (car datum) 'define) (define-exp (2nd datum) (parse-exp (3rd datum)))]
             [(not (list? datum)) (if (symbol? (cdr datum))
                                      (list (parse-exp (1st datum)) (parse-exp (cdr datum)))
                                      (app-exp (parse-exp (1st datum)) (parse-exp (cdr datum))))]
             [(= 2 (length datum)) (app-exp (parse-exp (1st datum)) (map parse-exp (cdr datum)))]
             [else (app-exp (parse-exp (car datum)) (map parse-exp (cdr datum)))])]
          [else (error 'parse-exp "bad expression: ~s" datum)]))))

; Additional subtype parsing
(define parse-lambda
  (lambda (datum)
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
    (cond [(and (eqv? (car datum) 'let) (symbol? (2nd datum))) (let-named (parse-exp (cadr datum)) (map (lambda (b) (cons (parse-exp (car b)) (list (parse-exp (cadr b))))) (caddr datum)) (map parse-exp (cdddr datum)))]
          [(eqv? (car datum) 'let) (let-basic (map (lambda (b) (append (list (parse-exp (car b))) (list (parse-exp (cadr b))))) (cadr datum)) (map parse-exp (cddr datum)))]
          [(eqv? (car datum) 'let*) (let-star (map (lambda (b) (cons (parse-exp (car b)) (list (parse-exp (cadr b))))) (cadr datum)) (map parse-exp (cddr datum)))]
          [(eqv? (car datum) 'letrec) (let-rec (map parse-exp (map car (cadr datum)))
                                               (map (lambda (a) (map parse-exp a)) (map cadr (map cadr (cadr datum))))
                                               (map list (map parse-exp (map caddr (map cadr (cadr datum)))))
                                               (map parse-exp (cddr datum)))])))

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
      [if-else (a b c) (unparse-if exp)]
      [if-no-else (exp1 exp2) (unparse-if exp)]
      [app-exp (rator rand) (cons (unparse-exp rator) (map unparse-exp rand))]
      [while-exp (test body) (cons 'while (cons (unparse-exp test) (map unparse-exp body)))]
      [define-exp (var val) (cons 'define (list var (unparse-exp val)))]
      [set-exp (var val) (cons 'set! (list var (unparse-exp val)))]
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

(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms (_box-vals vals) env)))

(define _box-vals
  (lambda (vals)
    (let hlp ([vals vals]
              [ret '()])
      (cond [(null? vals) (reverse ret)]
            [(not (list? vals)) (cons (box vals) ret)]
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
                        ;(raise 'missing-var)]
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
                                           (apply-env old-env sym)))])))


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
                 (s/e-let-named name ids body)]
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
      [define-exp (id body)
                  (define-exp id (syntax-expand body))]
      [else exp])))


; some procedures to syntax expand specific expression types
(define s/e-letstar
  (lambda (ids body cur-var cur-val)
    (cond [(null? ids) (app-exp (lc-mult (list cur-var) (map syntax-expand body)) (list (syntax-expand cur-val)))]
          [else (app-exp (lc-mult (list cur-var) (list (syntax-expand (s/e-letstar (cdr ids) (map syntax-expand body) (1st (1st ids)) (2nd (1st ids)))))) (list (syntax-expand cur-val)))])))

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
    (cond [(null? body) (lit-exp #t)]
          [else (syntax-expand (let-basic (list (list (parse-exp 'a) (syntax-expand (1st body)))) (list (if-else (parse-exp 'a) (s/e-and (cdr body)) (lit-exp #f)))))])))

(define s/e-or
  (lambda (body)
    (cond [(null? body) (lit-exp #f)]
          [else (syntax-expand (let-basic (list (list (parse-exp 'a) (syntax-expand (1st body)))) (list (if-else (parse-exp 'a) (parse-exp 'a) (s/e-or (cdr body))))))])))

(define s/e-begin
  (lambda (body)
    (app-exp (lc-mult '() (map syntax-expand body)) '())))

(define lasttolist
  (lambda (values argslength)
    (cond [(= 1 argslength) (list values)]
          [else (cons (car values) (lasttolist (cdr values) (- argslength 1)))])))

(define s/e-let-named
  (lambda (name ids body)
    (syntax-expand (let-basic (list (list name (lit-exp #f))) (list (let-basic ids (cons (set-exp (unparse-exp name) (lc-mult (map 1st ids) (map syntax-expand body))) (map syntax-expand body))))))))

(define s/e-letrec
  (lambda (proc-names idss bodiess letrec-bodies)
    (syntax-expand (let-basic (map (lambda (proc-name) (list proc-name (lit-exp #f))) proc-names) (append (map (lambda (proc-name ids body) (set-exp (unparse-exp proc-name) (lc-mult ids (map syntax-expand body)))) proc-names idss bodiess) letrec-bodies)))))

;---------------------------------------+
;                                       |
; sec:CONTINUATION DATATYPE and APPLY-K |
;                                       |
;---------------------------------------+

; To be added in assignment 18a.
(define-datatype continuation continuation?
  (init-k)
  [if-else-k (then-exp expression?)
             (else-exp expression?)
             (env environment?)
             (k continuation?)]
  [if-no-else-k (then-exp expression?)
                (env environment?)
                (k continuation?)]
  [rator-k (rands (list-of expression?))
           (env environment?)
           (k continuation?)]
  [rands-k (proc-value scheme-value?)
           (k continuation?)]
  [rands-recur (rands (list-of expression?))
               (eval-d list?)
               (env environment?)
               (k continuation?)]
  [set-k (id scheme-value?)
         (body expression?)
         (env environment?)
         (k continuation?)]
  [define-k (id symbol?)
    (body continuation?)]
    ;(env environment?)
    ;(k continuation?)]
  [box-k (var box?)
         (k continuation?)]
  [map-k (proc procedure?)]
  [eval-k (exp expression?)
          (env environment?)
          (k continuation?)]
  [closure-k (code (list-of expression?))
             (env environment?)
             (k continuation?)];args vals code env k
  )


(define apply-k
  (lambda (k val)
    (cases continuation k
      [init-k () val]
      [if-else-k (then-exp else-exp env k)
                 (if val
                     (eval-exp then-exp env k)
                     (eval-exp else-exp env k))]
      [if-no-else-k (then-exp env k)
                    (if val
                        (eval-exp then-exp env k)
                        (apply-k k (void)))]
      [rator-k (rands env k)
               (eval-rands rands env (rands-k val k))]
      [rands-k (proc k)
               (apply-proc proc val k)]
      [rands-recur (rands eval-d env k)
                   (cond [(null? rands) (apply-k k (reverse (cons val eval-d)))]
                         [else (eval-exp (car rands) env (rands-recur (cdr rands) (cons val eval-d) env k))])]
      [set-k (var set-to env k)
             (eval-exp set-to env (box-k val k))]
      
             ;(set! env (extend-env var set-to env))]
             ;(apply-k k (extend-env var set-to env))]
             ;(set-box! (apply-env env var) (eval-exp set-to env k))]
             ;(with-handlers ([exn:fail? (lambda (exn) (let ([env-box (apply-env global-env var)])
             ;                                           (set-box! env-box (eval-exp set-to env k))))])
             ;  (let ([env-box (apply-env env var)])
             ;    (set-box! env-box (eval-exp set-to env k))))]
      [box-k (b k) (apply-k k (set-box! b val))]
      
      [define-k (id body) (apply-k k (extend-global-env id val))];(set! global-env (cons (list id (box val)) global-env)))]
      [map-k (proc) (proc val)]
      [eval-k (exp env k) (eval-exp exp env k)]
      [closure-k (code env k) (apply-closure code env k)]
      [else val])))

;-------------------+
;                   |
;  sec:INTERPRETER  |
;                   |
;-------------------+

; top-level-eval evaluates a form in the global environment

(define top-level-eval
  (lambda (form)
    ; later we may add things that are not expressions.
    (eval-exp form global-env (init-k))))


; eval-exp is the main component of the interpreter

(define eval-exp
  (lambda (exp env k)
    (cases expression exp
      [lc-mult (args body) (apply-k k (closure args body env))]
      [lc-indef (arg body) (apply-k k (closure (list arg) body env))]
      [if-else (cond if-true else)
               (eval-exp cond env (if-else-k if-true else env k))]
      [if-no-else (cond if-true)
                  (eval-exp cond env (if-no-else-k if-true env k))]
      [set-exp (var set-to)
               (with-handlers ([exn:fail? (lambda (exn) (apply-k (set-k var set-to global-env k) (apply-env global-env var)))])
                                                          (apply-k (set-k var set-to env k) (apply-env env var)))]
               
               ;(with-handlers ([exn:fail? (lambda (exn) (let ([env-box (apply-env global-env var)])
               ;    (set-box! env-box (eval-exp set-to env (init-k)))))])
               ;  (let ([env-box (apply-env env var)])
               ;    (set-box! env-box (eval-exp set-to env (init-k)))))]
               
               ;(with-handlers ([exn:fail? (lambda (exn) (set-box! (apply-env global-env var) (eval-exp set-to global-env (init-k))))])
               ;  (set-box! (apply-env env var) (eval-exp set-to env (init-k))))]
               
               ;(extend-global-env var set-to)]
      
      [lit-exp (datum) (apply-k k datum)]
      [var-exp (id)
               ;(apply-k k (unbox (apply-env env id)))]
               (with-handlers ([exn:fail? (lambda (exn) (apply-k k (unbox (apply-env global-env id))))]) (apply-k k (unbox (apply-env env id))))]
      [app-exp (rator rands)
               (eval-exp rator env (rator-k rands env k))]
      [while-exp (test body)
                 (apply-while test body env k)]
      [define-exp (id body)
        ;(apply-k k (define-k id body))]
        (extend-global-env id (eval-exp body env k))]
        ;(eval-exp body env (define-k id k))]
      [else (error 'eval-exp "Bad abstract syntax: ~a" exp)])))


(define apply-while
  (lambda (test body env k)
    (if (eval-exp test env k)
        (apply-while-hlp test body env (void) k);(map-cps (lambda (a) (eval-exp a env k)) body k) k)
        (apply-k k (void)))))

(define apply-while-hlp
  (lambda (test body env prev k)
    (if (eval-exp test env k)
        (apply-while-hlp test body env (map-cps (lambda (a) (eval-exp a env k)) body k) k);(apply-while-hlp test body env (map (lambda (a) (eval-exp a env k)) body) k)
        (apply-k k prev))))

; evaluate the list of operands, putting results into a list

(define eval-rands
  (lambda (rands env k)
    (if (null? rands)
        (apply-k k rands)
        (eval-exp (car rands) env (rands-recur (cdr rands) '() env k)))))

;  Apply a procedure to its arguments.
;  At this point, we only have primitive procedures.  
;  User-defined procedures will be added later.

(define apply-proc
  (lambda (proc-value arg-vals k)
    (cases proc-val proc-value
      [prim-proc (op) (apply-prim-proc op arg-vals k)]
      [closure (args code env) (apply-closure code (extend-env (map unparse-exp args) arg-vals env) k)]
      [k-proc (k) (apply-k k (car arg-vals))]
      [else (error 'apply-proc
                   "Attempt to apply bad procedure: ~s" 
                   proc-value)])))

(define *prim-proc-names* '(map apply procedure? + - * / quotient positive? negative? add1 sub1 cons not eqv? = >= <= > < cons car cdr caar cadr cdar cddr caaar caadr cadar caddr cdaar cdadr cddar cdddr list null? eq? equal? atom? assq length list->vector make-vector vector-ref vector-set! list? pair? vector->list vector? number? zero? symbol? display newline vector append list-tail print void call/cc exit-list))

(define init-env         ; for now, our initial global environment only contains 
  (extend-env            ; procedure names.  Recall that an environment associates
   *prim-proc-names*   ;  a value (not an expression) with an identifier.
   (map prim-proc      
        *prim-proc-names*)
   (empty-env)))


(define global-env init-env)
(define extend-global-env
  (lambda (var val)
    (set! global-env (extend-env (list var) (list val) global-env))))
(define reset-global-env
  (lambda () (set! global-env init-env)))

; Usually an interpreter must define each 
; built-in procedure individually.  We are "cheating" a little bit.

(define apply-prim-proc
  (lambda (prim-proc args k)
    (case prim-proc
      [(+) (apply-k k (apply + args))]
      [(-) (apply-k k (apply - args))]
      [(*) (apply-k k (apply * args))]
      [(/) (apply-k k (apply / args))]
      [(quotient) (apply-k k (quotient (1st args) (2nd args)))]
      [(positive?) (apply-k k (positive? (1st args)))]
      [(negative?) (apply-k k (negative? (1st args)))]
      [(add1) (apply-k k (+ (1st args) 1))]
      [(sub1) (apply-k k (- (1st args) 1))]
      [(cons) (apply-k k (cons (1st args) (2nd args)))]
      [(not) (apply-k k (not (1st args)))]
      [(=) (apply-k k (= (1st args) (2nd args)))]
      [(>=) (apply-k k (>= (1st args) (2nd args)))]
      [(<=) (apply-k k (<= (1st args) (2nd args)))]
      [(>) (apply-k k (> (1st args) (2nd args)))]
      [(<) (apply-k k (< (1st args) (2nd args)))]
      [(car) (apply-k k (car (1st args)))]
      [(cdr) (apply-k k (cdr (1st args)))]
      [(caar) (apply-k k (caar (1st args)))]
      [(cadr) (apply-k k (cadr (1st args)))]
      [(cdar) (apply-k k (cdar (1st args)))]
      [(cddr) (apply-k k (cddr (1st args)))]
      [(caaar) (apply-k k (caaar (1st args)))]
      [(caadr) (apply-k k (caadr (1st args)))]
      [(cadar) (apply-k k (cadar (1st args)))]
      [(caddr) (apply-k k (caddr (1st args)))]
      [(cdaar) (apply-k k (cdaar (1st args)))]
      [(cdadr) (apply-k k (cdadr (1st args)))]
      [(cddar) (apply-k k (cddar (1st args)))]
      [(cdddr) (apply-k k (cdddr (1st args)))]
      [(list) (apply-k k (apply list args))]
      [(null?) (apply-k k (null? (1st args)))]
      [(eq?) (apply-k k (eq? (1st args) (2nd args)))]
      [(eqv?) (apply-k k (eqv? (1st args) (2nd args)))]
      [(equal?) (apply-k k (equal? (1st args) (2nd args)))]
      [(length) (apply-k k (length (1st args)))]
      [(list?) (apply-k k (list? (1st args)))]
      [(pair?) (apply-k k (pair? (1st args)))]
      [(vector?) (apply-k k (vector? (1st args)))]
      [(number?) (apply-k k (number? (1st args)))]
      [(symbol?) (apply-k k (symbol? (1st args)))]
      [(zero?) (apply-k k (zero? (1st args)))]
      ;[(atom?) (apply-k k (atom? (1st args)))]
      [(assq) (apply-k k (assq (1st args) (2nd args)))]
      [(procedure?) (apply-k k (proc-val? (1st args)))]
      [(list->vector) (apply-k k (list->vector (1st args)))]
      [(vector->list) (apply-k k (vector->list (1st args)))]
      [(make-vector) (apply-k k (make-vector (1st args)))]
      [(vector-ref) (apply-k k (vector-ref (1st args) (2nd args)))]
      [(vector-set!) (apply-k k (vector-set! (1st args) (2nd args) (3rd args)))]
      [(display) (apply-k k (apply display args))]
      [(newline) (apply-k k (apply newline args))]
      [(vector) (apply-k k (apply vector args))]
      [(map) (map-cps (1st args) (2nd args) k)];(apply map (lambda (x) (apply-proc (1st args) (list x) k)) (cdr args))];(map-cps (1st args) (map list (2nd args)) k)];
      [(apply) (apply-k k (apply apply-proc (append args (list (init-k)))))]
      [(append) (apply-k k (append (1st args) (2nd args)))]
      [(list-tail) (apply-k k (list-tail (1st args) (2nd args)))]
      [(print) (apply-k k (print (2nd args)))]
      [(void) (apply-k k (void))]
      [(call/cc) (apply-proc (1st args) (list (k-proc k)) k)]
      [(exit-list) args]
      [else (error 'apply-prim-proc 
                   "Bad primitive procedure name: ~s" 
                   prim-proc)])))


(define map-cps
  (lambda (proc args k)
    (if (null? args)
        (apply-k k '())
         (map-cps proc (cdr args) (map-k (lambda (b) (apply-proc proc (list (car args)) (map-k (lambda (a) (apply-k k (cons a b)))))))))))


(define apply-closure
  (lambda (code env k)
    ;(with-handlers ([exn:fail? (lambda (exn) (let ([env-box (apply-env global-env var)])
             ;                                           (set-box! env-box (eval-exp set-to env k))))])
    (with-handlers ((exn:fail? (lambda (exn) (car (reverse (map (lambda (a) (eval-exp a env k)) code))))))
    (if (null? (cdr code))
    ;    (eval-exp (car code) env k)
    ;    (apply-closure (cdr code) env (eval-k (car code) env k)))))
        
    ;    (eval-exp (car code) env k)
    ;    (begin (eval-exp (car code) env k) (apply-closure (cdr code) env k)))))
        (eval-exp (car code) env k)
        (eval-exp (car code) env (closure-k (cdr code) env k))))))
        
    ;(car (reverse (map (lambda (a) (eval-exp a env k)) code)))))
    ;(let ([new-env (extend-env-helper (map unparse-exp args) vals env)])
    ;  (car (reverse (map (lambda (a) (eval-exp a new-env k)) code))))))

(define extend-env-helper
  (lambda (args vals env)
    (if (pair? args)
        (if (and (not (null? (cdr vals))) (null? (cdr args)))
            (extend-env args (list vals) env)
            (extend-env-helper (cdr args) (cdr vals) (extend-env (list (car args)) (list (car vals)) env)))
        (extend-env args vals env))))

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
(define nested-set
  (parse-exp '(let ([a 1]) (set! a 3) a)))

; Test cases copied for debugging
(define some-letrec
  '(letrec ((fact (lambda (x) (if (zero? x) 1 (* x (fact (- x 1))))))) (map fact '(0 1 2 3 4 5))))
(define some-letrec-parsed
  (parse-exp some-letrec))


; Eval tests
(define eval-test
  (lambda (parsed-test)
    (eval-exp (syntax-expand parsed-test) init-env)))




; Simple CPS Tests
(define cps-lc-mult
  (lambda ()
    (eval-one-exp '(lambda (a b) (+ a b) (- a b))))) ;-1

(define cps-apply-closure
  (lambda ()
    (eval-one-exp '((lambda (a b) (+ a ((lambda (c) c) b))) 1 2)))) ;3

(define cps-if
  (lambda ()
    (eval-one-exp '(if (= 1 0) #t #f)))) ;#f

(define cps-lit
  (lambda ()
    (eval-one-exp '(1 2 3)))) ;'(1 2 3)

(define cps-define
  (lambda ()
    (eval-one-exp '(define a 1))
    (eval-one-exp 'a)
    (reset-global-env))) ;1

(define cps-set
  (lambda ()
    (eval-one-exp '(define a 1))
    (eval-one-exp 'a)
    (eval-one-exp '(set! a 3))
    (eval-one-exp 'a)
    (reset-global-env))) ;13

(define s/e
  (lambda (a)
    (syntax-expand (parse-exp a))))



(define e eval-one-exp)
(define u
  (lambda (a)
    (unparse-exp (syntax-expand (parse-exp a)))))







