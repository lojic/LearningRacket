#lang racket

(require debug/repl)

;; Inspired by a puzzle using match sticks as segments in a 7-segment
;; display, where you're supposed to move 2 match sticks to form a
;; valid equation, beginning with 8 - 4 = 61
;;
;; The goal of this program is to compute all valid equations that can
;; be made with the original 22 match sticks.
;;
;; I've limited the possible characters to 0-9, +, -, = as a simplification

;; --------------------------------------------------------------------------------------------
;; Expression parser
;; --------------------------------------------------------------------------------------------

(require parser-tools/lex
         parser-tools/yacc
         (prefix-in : parser-tools/lex-sre))

(define-tokens value-tokens (NUMBER))
(define-empty-tokens op-tokens (EOF ADD SUBTRACT PRODUCT NEG NEWLINE))

(define next-token
  (lexer-src-pos
   [ (eof) (token-EOF) ]
   [ (:+ (:& (:~ #\newline) whitespace)) (return-without-pos (next-token input-port)) ]
   [ #\+ (token-ADD) ]
   [ #\- (token-SUBTRACT) ]
   [ #\* (token-PRODUCT) ]
   [ (:: (:+ numeric) (:* (:: #\. (:+ numeric) ))) (token-NUMBER (string->number lexeme))]))

(define myparser
  (parser
   ;; (start input)
   (start exp)
   (end EOF)
   (tokens value-tokens op-tokens)
   (src-pos)
   ;; (error (λ (a b c d e)
   ;;          (begin
   ;;            (printf "a = ~a\nb = ~a\nc = ~a\nd = ~a\ne = ~a\n" a b c d e) (void))))
   (error (λ (a b c d e) (void)))

   (precs
    (left ADD SUBTRACT)
    (left PRODUCT)
    (nonassoc NEG))

   (grammar
    (exp [ (NUMBER)           $1        ]
         [ (exp ADD exp)      (+ $1 $3) ]
         [ (exp SUBTRACT exp) (- $1 $3) ]
         [ (exp PRODUCT exp)  (* $1 $3) ]))))
;         [ (SUBTRACT exp)     (prec NEG) (- $2) ]))))

(define (parse str)
  (let ([ ip (open-input-string str) ])
    (port-count-lines! ip)
    (with-handlers ([ exn:fail:read?
                      (λ (e) #f) ])
      (myparser (λ () (next-token ip))))))

;; --------------------------------------------------------------------------------------------

(define N 22)
(define chars '((#\0 . 6)
                (#\1 . 2)
                (#\2 . 5)
                (#\3 . 5)
                (#\4 . 4)
                (#\5 . 5)
                (#\6 . 6)
                (#\7 . 3)
                (#\8 . 7)
                (#\9 . 6)
                (#\= . 2)
                (#\* . 4)
                (#\+ . 2)
                (#\- . 1)))

;; Return the count of sticks from all the characters in the stack
(define (count-sticks stack)
  (let loop ([ stack stack ][ sum 0 ])
    (if (null? stack)
        sum
        (loop (cdr stack) (+ sum (stick-len (car stack)))))))

(define (get-char n)
  (let loop ([ chars chars ])
    (if (null? chars)
        #f
        (match-let ([ (cons c len) (car chars) ])
          (if (<= len n)
              c
              (loop (cdr chars)))))))

(define (is-valid? stack)
  (let-values ([(lhs rhs) (split-equation stack)])
    (and lhs
         rhs
         (= lhs rhs))))

;; Return the successor of ch
(define (next-char ch)
  (let loop ([ chars chars ][ prev #\Z ])
    (cond [ (null? chars)    #f          ]
          [ (char=? ch prev) (caar chars) ]
          [ else
            (loop (cdr chars) (caar chars)) ])))

;; Backtracking
(define (next-stack stack)
  (if (null? stack)
      #f
      (let* ([ top (car stack)     ]
             [ c   (next-char top) ])
        (if c
            (cons c (cdr stack))
            (next-stack (cdr stack))))))

(define (split-equation stack)
  (let ([ indices (indexes-of stack #\=) ])
    (if (= 1 (length indices))
        (let-values ([ (lhs rhs) (split-at stack (car indices)) ])
          (values (parse (list->string lhs)) (parse (list->string (cdr rhs)))))
        (values #f #f))))

;; Return the number of sticks required to represent the character
(define (stick-len c)
  (let loop ([ chars chars ])
    (match-let ([ (cons ch len) (car chars) ])
      (if (char=? c ch)
          len
          (loop (cdr chars))))))

(define (equation stack)
  (let loop ([ stack stack ])
    (let ([ available-sticks (- N (count-sticks stack))])
      (cond [ (zero? available-sticks)
              (let ([ reversed (reverse stack) ])
                (if (is-valid? reversed)
                    stack
                    ;; Backtrack
                    (let ([ next (next-stack stack) ])
                      (if next
                          (loop next)
                          #f)))) ]
            [ else
              (let ([ c (get-char available-sticks) ])
                (if c
                    (loop (cons c stack))
                    (let ([ next (next-stack stack) ])
                      (if next
                          (loop next)
                          #f)))) ]))))

(module+ main
  (let loop ([ stack '(#\= #\1 #\6) ])
    (let ([ solution (equation stack) ])
      (cond [ solution
              (displayln (list->string (reverse solution)))
              (let ([ next (next-stack solution) ])
                (if next
                    (loop next)
                    (void))) ]
            [ else (void) ]))))

(module+ test
  (require rackunit)

  ;; count-sticks -----------------------------------------------------------------------------
  (check-equal? (count-sticks '(#\8 #\- #\4 #\= #\6 #\1)) 22)
  
  ;; get-char ---------------------------------------------------------------------------------
  (check-equal? (get-char 8) (caar chars)) ; Should always return first
  (check-equal? (get-char 4) #\1)          ; Depends on order of chars
  (check-equal? (get-char 1) #\-)
  (check-equal? (get-char 0) #f)
  
  ;; is-valid? --------------------------------------------------------------------------------
  (check-not-false (is-valid? '(#\8 #\- #\4 #\= #\4)))
  (check-false (is-valid? '(#\8 #\- #\4 #\= #\5)))
  
  ;; parse ------------------------------------------------------------------------------------
  (for ([ pair (in-list '(("1 + 1" 2)
                          ("3 + 2 * 4" 11)
                          ("-3 * -4" 12)
                          ("* * 4" #f)
                          )) ])
    (match-let ([ (list expr val) pair ])
      (check-equal? (parse expr) val)))

  ;; split-equation ---------------------------------------------------------------------------
  (let-values ([ (lhs rhs) (split-equation '(#\6 #\+ #\3 #\= #\5 #\- #\7)) ])
    (check-equal? lhs 9)
    (check-equal? rhs -2))

  (let-values ([ (lhs rhs) (split-equation '(#\6 #\+ #\3 #\7)) ])
    (check-false lhs)
    (check-false rhs))

  ;; stick-len --------------------------------------------------------------------------------
  (check-equal? (stick-len #\0) 6)
  (check-equal? (stick-len #\=) 2)
  
  )
