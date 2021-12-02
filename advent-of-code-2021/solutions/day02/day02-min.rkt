#lang racket

(require "syntax.rkt")
(define input (file->list "day02.txt"))
(struct s (aim pos depth))

(define (solve part input)
  (let ([ obj (foldl (λ (pair obj) (part obj (car pair) (cdr pair)))
                     (s 0 0 0)
                     (parse input)) ])
    (* (s-pos obj) (s-depth obj))))

(define (parse input) (if (null? input) '() (cons (cons (car input) (cadr input)) (parse (cddr input)))))

(define (part1 obj command n)
  (match command
    [ 'forward (state [ pos   (+ (s-pos obj) n) ])   ]
    [ 'down    (state [ depth (+ (s-depth obj) n) ]) ]
    [ 'up      (state [ depth (- (s-depth obj) n) ]) ]))

(define (part2 obj command n)
  (match command
    [ 'forward (state [ pos   (+ (s-pos obj) n)                      ]
                      [ depth (+ (s-depth obj) (* (s-aim obj) n)) ]) ]
    [ 'down    (state [ aim   (+ (s-aim obj) n) ])                   ] 
    [ 'up      (state [ aim   (- (s-aim obj) n) ])                   ]))

(solve part1 input)
(solve part2 input)
