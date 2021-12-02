#lang racket

(require "syntax.rkt")
(define input (file->list "day02.txt"))
(define example '(forward 5 down 5 forward 8 up 3 down 8 forward 2))
(struct s (aim pos depth))

;; Solve the puzzle. Use a fold to update the state struct with each
;; command, then return the product of pos and depth.
(define (solve part input)
  (let ([ obj (foldl (λ (pair obj) (part obj (car pair) (cdr pair)))
                     (s 0 0 0)
                     (parse input)) ])
    (* (s-pos obj) (s-depth obj))))

;; Convert the input to a list of the form:
;; ((<command1> . <n1>) (<command2> . <n2>) ...)
(define (parse input)
  (if (null? input)
      '()
      (cons (cons (car input)
                  (cadr input))
            (parse (cddr input)))))

;; Part 1 command processor
(define (part1 obj command n)
  (match command
    [ 'forward (state [ pos   (+ (s-pos obj) n) ])   ]
    [ 'down    (state [ depth (+ (s-depth obj) n) ]) ]
    [ 'up      (state [ depth (- (s-depth obj) n) ]) ]))

;; Part 2 command processor
(define (part2 obj command n)
  (match command
    [ 'forward (state [ pos   (+ (s-pos obj) n)                      ]
                      [ depth (+ (s-depth obj) (* (s-aim obj) n)) ]) ]
    [ 'down    (state [ aim   (+ (s-aim obj) n) ])                   ] 
    [ 'up      (state [ aim   (- (s-aim obj) n) ])                   ]))

;; --------------------------------------------------------------------------------------------
;; Tests
;; --------------------------------------------------------------------------------------------

(module+ test
  (require rackunit)

  (check-equal? (solve part1 example) 150)
  (check-equal? (solve part2 example) 900)
  (check-equal? (solve part1 input) 1936494)
  (check-equal? (solve part2 input) 1997106066))
