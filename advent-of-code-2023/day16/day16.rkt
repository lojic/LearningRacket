#lang racket
(require "../advent.rkt")

(define-values (grid width height)
  (let ([ lines (parse-aoc 16 string->list) ])
    (values (grid->hash lines) (length (car lines)) (length lines))))

(define (beam pos dir seen)
  (define (ray c seen key pos dir)
    (let ([ seen (set-add seen key) ]
          [ x    (real-part dir)    ]
          [ y    (imag-part dir)    ]
          [ next (λ (pos dir seen) (beam (+ pos dir) dir seen)) ])
      (cond [ (or (char=? c #\.)
                  (and (char=? c #\-) (= 0 y))
                  (and (char=? c #\|) (= 0 x))) (next pos dir seen) ]
            [ (char=? c #\\) (next pos (+ y (* x +i))         seen) ]
            [ (char=? c #\/) (next pos (+ (- y) (* x -i)) seen) ]
            [ (char=? c #\-) (next pos -1 (next pos 1  seen)) ]
            [ (char=? c #\|) (next pos -i (next pos +i seen)) ])))

  (let ([ key (cons pos dir) ])
    (cond [ (set-member? seen key) seen ]
          [ else (let ([ c (hash-ref grid pos #f) ])
                   (cond [ (not c) seen ]
                         [ else (ray c seen key pos dir)]))])))

(define (part1 pos dir)
  (~> (beam pos dir (set))
      (set->list _)
      (map car _)
      (remove-duplicates _)
      length))

(define (part2)
  (list-max
   (map (λ (config) (part1 (car config) (cdr config)))
        (append (map (λ (col) (cons col                          +i))  (range width))
                (map (λ (col) (cons (+ col (* (sub1 height) +i)) -i))  (range width))
                (map (λ (row) (cons (* row +i)                   1))   (range height))
                (map (λ (row) (cons (+ (sub1 width) (* row +i)) -1))   (range height))))))

;; Tests --------------------------------------------------------------------------------------

(check-equal? (part1 0 1) 7242)
(check-equal? (part2) 7572)
