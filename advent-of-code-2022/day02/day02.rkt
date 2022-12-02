#lang racket

(require "../advent.rkt")

(define in (parse-aoc 2 chars))

;; Part 1 -------------------------------------------------------------------------------------

(define (char->shape c)
  (match c
    [ #\A 'rock     ]
    [ #\X 'rock     ]
    [ #\B 'paper    ]
    [ #\Y 'paper    ]
    [ #\C 'scissors ]
    [ #\Z 'scissors ]))

(define (shape-score shape)
  (match shape
    [ 'rock     1 ]
    [ 'paper    2 ]
    [ 'scissors 3 ]))

(define (outcome us them)
  (match (cons us them)
   [ '(rock     . scissors) 'win  ]
   [ '(rock     . rock)     'draw ]
   [ '(rock     . paper)    'loss ]
   [ '(paper    . rock)     'win  ]
   [ '(paper    . paper)    'draw ]
   [ '(paper    . scissors) 'loss ]
   [ '(scissors . paper)    'win  ]
   [ '(scissors . scissors) 'draw ]
   [ '(scissors . rock)     'loss ]))

(define (outcome-score outcome)
  (match outcome
    [ 'loss 0 ]
    [ 'draw 3 ]
    [ 'win  6 ]))

(define (part1 pair)
  (let ([ them (char->shape (first pair))  ]
        [ us   (char->shape (second pair)) ])
    (+ (shape-score us)
       (outcome-score (outcome us them)))))

(foldl (λ (pair sum) (+ (part1 pair) sum))
       0
       in)

;; Part 2 -------------------------------------------------------------------------------------

(define (expected-outcome c)
  (match c
    [ #\X 'loss ]
    [ #\Y 'draw ]
    [ #\Z 'win  ]))

(define (expected-shape pair)
  (let ([ shapes  (map char->shape '(#\A #\B #\C)) ]
        [ them    (char->shape (first pair))       ]
        [ desired (expected-outcome (second pair)) ])
    (findf (λ (shape)
              (eq? desired (outcome shape them)))
           shapes)))

(define (part2 pair)
  (let ([ outcome (expected-outcome (second pair)) ]
        [ them    (char->shape (first pair))       ]
        [ us      (expected-shape pair)            ])
    (+ (shape-score us)
       (outcome-score outcome))))

(foldl (λ (pair sum) (+ (part2 pair) sum))
       0
       in)
