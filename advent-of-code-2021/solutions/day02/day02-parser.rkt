#lang racket

(require parser-tools/lex
         parser-tools/yacc
         (prefix-in : parser-tools/lex-sre))

(define-tokens       value-tokens [ number ])
(define-empty-tokens op-tokens    [ forward up down eof ])
(define-lex-abbrev   digit (:/ #\0 #\9))

(define aim 0)
(define x   0)
(define y   0)

(define sub-lexer
  (lexer
   [ "down"      (token-down)                           ]
   [ "forward"   (token-forward)                        ]
   [ "up"        (token-up)                             ]
   [ (:+ digit)  (token-number (string->number lexeme)) ]
   [ (eof)       (token-eof)                            ]
   [ whitespace  (sub-lexer input-port)                 ]))

(define sub-parser
  (parser
   [ start plan ]
   [ end   eof  ]
   [ error (λ args (error "error:" args)) ]
   [ tokens value-tokens op-tokens ]
   [ grammar (plan [ (phrase)      (void) ]
                   [ (plan phrase) (void) ])
             (phrase [ (forward number) (begin (set! x (+ x $2))
                                               (set! y (+ y (* aim $2)))) ]
                     [ (up number)      (set! aim (- aim $2)) ]
                     [ (down number)    (set! aim (+ aim $2)) ]) ]))

(define (parse s)
  (let ([ input (open-input-string s) ])
    (sub-parser (thunk (sub-lexer input)))))

(parse (file->string "day02.txt"))
(printf "Part 1 = ~a, Part 2 = ~a\n" (* x aim) (* x y))
