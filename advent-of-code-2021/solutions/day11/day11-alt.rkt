#lang racket

;; This versions threads both the number of flashes and the list of
;; octopi through the pipeline.

(require "../../advent/advent.rkt")

(struct octopus (coord energy flashed?))
(struct state (flashes octopi))

(define (part1 s)
  (flashes (iterate step s 100)))

(define (part2 s)
  (define action     (λ (s) (step (clear s))))
  (define predicate? (λ (s) (= (flashes s) 100)))

  (repeat-until action predicate? s))

(define (reset s)
  (octopi= s (for/list ([ o (state-octopi s) ])
                          (if (octopus-flashed? o)
                              (reset-octo o)
                              o))))

(define (increment-energy s)
  (octopi= s (map increment-octo (state-octopi s))))

(define (flash s)
  ;; Helper functions -------------------------------------------------------------------------
  (define (flash-needed? octo)
    (and (> (octopus-energy octo) 9)
         (not (octopus-flashed? octo))))

  (define (flash-one-octopus octo s)
    (define (neighbors-of coord)
      (map (curry + coord) '(-i 1 +i -1 1-i -1-i 1+i -1+i)))

    (define (mark-flashed o)
      (struct-copy octopus o [ flashed? #t ]))

    (let ([ adjacent (neighbors-of (octopus-coord octo)) ])
      (for/list ([ o (state-octopi s) ])
        (let ([ coord (octopus-coord o) ])
          (cond [ (= coord (octopus-coord octo))  (mark-flashed o)   ]
                [ (member coord adjacent)         (increment-octo o) ]
                [ else                            o                  ])))))
  ;; ------------------------------------------------------------------------------------------

  (let ([ octo (findf flash-needed? (state-octopi s)) ])
    (if octo
        (flash (octopi= s (flash-one-octopus octo s)))
        s)))

(define (parse file-name)
  (define N 10)
  (define (->coord x y) (+ x (* y +i)))

  (let ([ octopi (for/list ([ line (file->lines file-name) ]
                            [ y    (range N)               ]
                            #:when #t
                            [ c (string->list line) ]
                            [ x (range N)           ])
                   (octopus (->coord x y)
                            (- (char->integer c) 48)
                            #f)) ])
    (state 0 octopi)))

(define (count-flashes s)
  (let ([ octopi  (state-octopi s)  ]
        [ flashes (state-flashes s) ])
    (flashes= s (+ flashes (count octopus-flashed? octopi)))))

(define (repeat-until fun pred? arg)
  (let loop ([ n 1 ][ arg arg ])
    (let ([ new-arg (fun arg) ])
      (if (pred? new-arg)
          n
          (loop (add1 n) new-arg)))))

(define (clear s)          (struct-copy state s [ flashes 0 ]))
(define (flashes s)        (state-flashes s))
(define (increment-octo o) (struct-copy octopus o [ energy (add1 (octopus-energy o)) ]))
(define (reset-octo o)     (struct-copy octopus o [ flashed? #f ][ energy 0 ]))
(define (flashes= s n)     (struct-copy state s [ flashes n ]))
(define (octopi= s octopi) (struct-copy state s [ octopi octopi ]))
(define step               (compose count-flashes flash increment-energy reset))

;; Tests --------------------------------------------------------------------------------------

(module+ test
  (require rackunit)
  (let ([ input (parse "day11.txt") ])
    (check-equal? (part1 input) 1647)
    (check-equal? (part2 input) 348)))
