#lang racket
(require "../advent.rkt")

(define in  (parse-aoc 8 digits))              ; List of rows
(define N   (length (car in)))                 ; Dimension of grid
(define vec (apply vector (apply append in)))  ; N x N grid

(define (get x y)    (vector-ref vec (+ (* y N) x))) ; Get tree at (x, y)
(define (valid? x y) (and (< -1 x N) (< -1 y N)))    ; Valid (x, y) coordinates?

;; Return a list of viewing distances in order of: up, right, down, left
(define (viewing-distances x* y* [ return add1 ])
  (define t*  (get x* y*))
  (define pos (make-rectangular x* y*)) ; Use a complex number for easier up/right/down/left nav

  (for/list ([ dir '(0-i 1+0i 0+i -1+0i) ])
    (let loop ([ pos (+ pos dir) ][ dist 0 ])
      (let ([ x (real-part pos) ]
            [ y (imag-part pos) ])
        (if (valid? x y)
            (if (<= t* (get x y))
                (return dist)
                (loop (+ pos dir) (add1 dist)))
            dist)))))

;; Indicate whether the Tree at (x, y) is visible outside the grid
(define (visible? x y)
  (match-let ([ (list up right down left) (viewing-distances x y identity) ])
    (or (= left  x)
        (= up    y)
        (= right (- N x 1))
        (= down  (- N y 1)))))

;; Compute the scenic score for Tree at (x, y)
(define scenic-score (compose (curry apply *) viewing-distances))

;; Main solve loop that accepts a <part> high order function
(define (solve part)
  (for*/fold ([ result 0 ])
             ([ x (in-range N) ]
              [ y (in-range N) ])
    ((part x y) result)))

(define (part1 x y)
  (if (visible? x y) add1 identity))

(define (part2 x y)
  (curry max (scenic-score x y)))

(solve part1)
(solve part2)
