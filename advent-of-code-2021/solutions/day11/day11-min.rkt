#lang racket

(struct octopus (coord energy flashed?))

(define (part1 octopi n [ sum 0 ])
  (cond [ (< n 1) sum ]
        [ else (let ([ octopi (step octopi) ])
                 (part1 octopi (sub1 n) (+ sum (count octopus-flashed? octopi)))) ]))

(define (part2 octopi [ n 0 ])
  (cond [ (all? octopus-flashed? octopi) n ]
        [ else (part2 (step octopi) (add1 n)) ]))

(define (reset octopi)
  (for/list ([ o octopi ])
    (if (octopus-flashed? o) (reset-octo o) o)))

(define (increment-energy octopi) (map increment-octo octopi))

(define (flash octopi)
  (define (flash-needed? octo)
    (and (> (octopus-energy octo) 9) (not (octopus-flashed? octo))))
  (define (flash-one-octopus octo octopi)
    (define (mark-flashed o)
      (struct-copy octopus o [ flashed? #t ]))
    (let ([ adjacent (map (curry + (octopus-coord octo)) '(-i 1 +i -1 1-i -1-i 1+i -1+i)) ])
      (for/list ([ o octopi ])
        (cond [ (= (octopus-coord o) (octopus-coord octo)) (mark-flashed o)   ]
              [ (member (octopus-coord o) adjacent)        (increment-octo o) ]
              [ else                                       o                  ]))))
  (let ([ octo (findf flash-needed? octopi) ])
    (if octo (flash (flash-one-octopus octo octopi)) octopi)))

(define (parse file-name)
  (for/list ([ line (file->lines file-name) ][ y (range 10) ]
             #:when #t
             [ character (string->list line) ][ x (range 10) ])
    (octopus (+ x (* y +i)) (- (char->integer character) 48) #f)))

(define all?               andmap)
(define step               (compose flash increment-energy reset))
(define (increment-octo o) (struct-copy octopus o [ energy (add1 (octopus-energy o)) ]))
(define (reset-octo o)     (struct-copy octopus o [ flashed? #f ][ energy 0 ]))

(module+ test
  (require rackunit)
  (let ([ input (parse "day11.txt") ])
    (check-equal? (part1 input 100) 1647)
    (check-equal? (part2 input) 348)))
