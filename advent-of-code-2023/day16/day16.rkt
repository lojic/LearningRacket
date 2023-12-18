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
                  (and (char=? c #\|) (= 0 x))) (next pos dir seen) ] ; Pass through
            [ (char=? c #\\) (next pos (+ y (* x +i))         seen) ] ; Reflect
            [ (char=? c #\/) (next pos (+ (- y) (* x -i))     seen) ] ; Reflect
            [ (char=? c #\-) (next pos -1 (next pos 1  seen)) ]       ; Split
            [ (char=? c #\|) (next pos -i (next pos +i seen)) ])))    ; Split

  (let ([ key (cons pos dir) ])
    (cond [ (set-member? seen key) seen           ]
          [ else (let ([ c (hash-ref grid pos #f) ])
                   (cond [ (not c) seen                     ]
                         [ else    (ray c seen key pos dir) ])) ])))

(define (part1 pos dir)
  (length (remove-duplicates (map car (set->list (beam pos dir (set)))))))

(define (part2)
  (list-max
   (map (curry apply part1)
        (append (map (λ (col) (list col                          +i)) (range width))      ; Top edge
                (map (λ (col) (list (+ col (* (sub1 height) +i)) -i)) (range width))      ; Bottom edge
                (map (λ (row) (list (* row +i)                    1)) (range height))     ; Left edge
                (map (λ (row) (list (+ (sub1 width) (* row +i))  -1)) (range height)))))) ; Right edge

;; Tests --------------------------------------------------------------------------------------

(check-equal? (part1 0 1) 7242)
(check-equal? (part2) 7572)