#lang racket

;; Arithmetic expression evaluator inspired by Haskal's solution to AoC 2020 Day 18.

;; See other examples here:
;; https://github.com/racket/parser-tools/tree/master/parser-tools-lib/parser-tools/examples

(require parser-tools/lex
         parser-tools/yacc
         (prefix-in : parser-tools/lex-sre))

(define-tokens       nums [ number ])
(define-empty-tokens syms [ + - * / lp rp eof ])
(define-lex-abbrev   digit (:/ #\0 #\9))

(define arith-lexer
  (lexer
    [ "+" (token-+) ]
    [ "-" (token--) ]
    [ "*" (token-*) ]
    [ "/" (token-/) ]
    [ "(" (token-lp) ]
    [ ")" (token-rp) ]
    [ whitespace (arith-lexer input-port) ]
    [ (:+ digit) (token-number (string->number lexeme)) ]
    [ (:: (:+ digit) #\. (:* digit)) (token-number (string->number lexeme)) ]
    [ (eof) (token-eof) ]))

(define arith-parser
  (parser
   [ start   exp ]
   [ end     eof ]
   [ error   (lambda args (error "error:" args)) ]
   [ tokens  nums syms ]
   [ precs   (left + -) (left * /) (left lp rp) ]
   [ grammar (exp [ (lp exp rp) $2        ]
                  [ (exp * exp) (* $1 $3) ]
                  [ (exp / exp) (/ $1 $3) ]
                  [ (exp + exp) (+ $1 $3) ]
                  [ (exp - exp) (- $1 $3) ]
                  [ (number)    $1        ]) ]))

(define (parse s)
  (let ([ input (open-input-string s) ])
    (arith-parser (thunk (arith-lexer input)))))

(module+ test
  (require rackunit)

  (for ([ pair (in-list `(("0" 0)
                          ("0 + 0" ,(+ 0 0))
                          ("0 + 1" ,(+ 0 1))
                          ("1 + 2 - 3 * 4 / 5" ,(- (+ 1 2) (/ (* 3 4) 5)))
                          ("7 - 3 * 4" ,(- 7 (* 3 4)))
                          ("5 / 3 * 2 - 10" ,(- (* (/ 5 3) 2) 10))
                          ("3.141592653589793 / 3" ,(/ 3.141592653589793 3))
                          ("3 * (5 + 6)" ,(* 3 (+ 5 6)))
                          ("(3 + 4) - 2" ,(- (+ 3 4) 2))
                          )) ])
    (check-equal? (parse (first pair)) (second pair))))
