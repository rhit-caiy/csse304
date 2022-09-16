#lang racket
(define  (count-occurrences slist sym)
  (let count ([slist slist])
    (cond [(null? slist) 0];null
          [(equal? (car slist) sym) (add1 (count-occurrences (cdr slist) sym))];first is equal
          [(list? (car slist)) (+ (count-occurrences (car slist) sym) (count-occurrences (cdr slist) sym))];first is list
          [else (count-occurrences (cdr slist) sym)])));next
           
	  


(count-occurrences '() 'a)                      ; 0
(count-occurrences '(b a b () a b) 'a)          ; 2
(count-occurrences '(b a b () a b) 'a)          ; 2
(count-occurrences '(b ((a) a) b () a b) 'a)    ; 3
(count-occurrences '((b ((a) a) b () a b)) 'a)  ; 3

(define  (flatten slist)
  (cond [(null? slist) '()]
        [(symbol? (car slist)) (cons (car slist) (flatten (cdr slist)))]
        [else (append (flatten (car slist)) (flatten (cdr slist)))]))



(flatten '( () (a ((b) c () ((d e ((f))) g) h)) ()))  ; (a b c d e f g h)

(define (notate-depth slist) 
  (let notate ([slist slist]
	       [depth 1])
    (cond [(null? slist) '()]
          [(symbol? (car slist)) (cons (list (car slist) depth) (notate (cdr slist) depth))]
          [else (cons (notate (car slist) (add1 depth)) (notate (cdr slist) depth))])))



(notate-depth '())                          ; ()
(notate-depth '(a))                         ; ((a 1)) 
(notate-depth '((a b) c))                   ; (((a 2) (b 2)) (c 1))
(notate-depth '( () (a (b)) c ((d) () e)))  ; (() ((a 2) ((b 3))) (c 1) (((d 3)) () (e 2)))
(notate-depth '((() (a (b)) c ((d) () e)))) ; ((() ((a 3) ((b 4))) (c 2) (((d 4)) () (e 3))))