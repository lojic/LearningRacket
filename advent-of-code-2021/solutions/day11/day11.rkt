#lang racket

;; This version uses a list of octopus structs as the data structure,
;; and is completely functional with no mutation.

(struct octopus (coord
                 energy
                 flashed?))

(define (part1 octopi n [ sum 0 ])
  (if (< n 1)
      sum
      (let ([ octopi (step octopi) ])
        (part1 octopi
               (sub1 n)
               (+ sum (count octopus-flashed? octopi))))))

(define (part2 octopi [ n 0 ])
  (if (all? octopus-flashed? octopi)
      n
      (part2 (step octopi) (add1 n))))

(define (reset octopi)
  (for/list ([ o octopi ])
    (if (octopus-flashed? o)
        (struct-copy octopus o [ flashed? #f ][ energy 0 ])
        o)))

(define (increment-energy octopi)
  (for/list ([ o octopi ])
    (increment-octo o)))

(define (flash octopi)
  ;; Helper functions -------------------------------------------------------------------------
  (define (flash-needed? octo)
    (and (> (octopus-energy octo) 9)
         (not (octopus-flashed? octo))))

  (define (flash-one-octopus octo octopi)
    (define (neighbors-of coord)
      (for/list ([ direction '(-i 1 +i -1 1-i -1-i 1+i -1+i) ])
        (+ coord direction)))

    (define (mark-flashed o)
      (struct-copy octopus o [ flashed? #t ]))

    (let ([ adjacent (neighbors-of (octopus-coord octo)) ])
      (for/list ([ o octopi ])
        (let ([ coord (octopus-coord o) ])
          (cond [ (= coord (octopus-coord octo))  (mark-flashed o)   ]
                [ (member coord adjacent)         (increment-octo o) ]
                [ else                            o                  ])))))
  ;; ------------------------------------------------------------------------------------------

  (let ([ octo (findf flash-needed? octopi) ])
    (if octo
        (flash (flash-one-octopus octo octopi))
        octopi)))

(define step (compose flash increment-energy reset))

(define (increment-octo o)
  (struct-copy octopus o [ energy (add1 (octopus-energy o)) ]))

(define (parse file-name)
  (define N 10)
  (define (->coord x y) (+ x (* y +i)))

  (for/list ([ line (file->lines file-name) ]
             [ y    (range N)               ]
             #:when #t
             [ character (string->list line) ]
             [ x         (range N)           ])
    (octopus (->coord x y)
             (- (char->integer character) 48)
             #f)))

;; Aliases ------------------------------------------------------------------------------------

(define all? andmap)

;; Tests --------------------------------------------------------------------------------------

(module+ test
  (require rackunit)
  (let ([ input (parse "day11.txt") ])
    (check-equal? (part1 input 100) 1647)
    (check-equal? (part2 input) 348)))
