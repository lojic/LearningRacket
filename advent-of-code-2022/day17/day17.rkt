#lang racket
(require "../advent.rkt")

(struct hole (height num-rocks coords jets shape-idx) #:transparent)

(define shapes     #((0 1 2 3) (1 +i 1+i 2+i 1+2i) (0 1 2 2+i 2+2i) (0 +i +2i +3i) (0 1 +i 1+i)))
(define num-shapes (vector-length shapes))
(define jets       (map (λ (c)
                          (match c
                            [ #\< -1 ]
                            [ #\>  1 ]))
                        (string->list (car (parse-aoc 17)))))
(define init-state (hole 0 0 '() jets 0))
(define get-shape  (λ (n) (vector-ref shapes n)))
(define next-jet   (λ (state) (car (hole-jets state))))

(define (shape-height shape)
  (* +i (add1 (for/fold ([ height 0 ])
                        ([ c shape ])
                (max height (imag-part c))))))

(define (shape-push state shape jet)
  (let* ([ shape* (map (λ (c) (+ c jet)) shape) ]
         [ valid? (ormap (λ (c)
                           (or (not (<= 0 (real-part c) 6))
                               (memv c (hole-coords state))))
                         shape*) ])
    (if valid? shape shape*)))

(define (shape-fall state shape)
  (let* ([ shape* (map (λ (c) (- c +i)) shape) ]
         [ valid? (ormap (λ (c)
                           (or (< (imag-part c) 0)
                               (memv c (hole-coords state))))
                         shape*) ])
    (if valid? shape shape*)))

(define (increment-jet state)
  (let ([ jets* (cdr (hole-jets state)) ])
    (struct-copy hole state [ jets (if (null? jets*) jets jets*) ])))

(define (shape-create state)
  (map (λ (c)
         (+ c 2 +3i (hole-height state)))
       (get-shape (hole-shape-idx state))))

(define (drop-shape state)
  (let loop ([ state state ][ shape (shape-create state) ])
    (let* ([ jet    (next-jet state)             ]
           [ shape  (shape-push state shape jet) ]
           [ shape* (shape-fall state shape)     ]
           [ state* (increment-jet state)        ])
      (if (equal? (shape-height shape) (shape-height shape*))
          (add-rock state* shape*)
          (loop state* shape*)))))

(define (add-rock state shape)
  (let* ([ coords* (prune-coords (append shape (hole-coords state))) ]
         [ height* (* +i (max (imag-part (shape-height shape))
                              (imag-part (hole-height state)))) ]
         [ num-rocks* (add1 (hole-num-rocks state)) ])
    (struct-copy hole state
                 [ height    height*    ]
                 [ coords    coords*    ]
                 [ num-rocks num-rocks* ]
                 [ shape-idx (modulo (add1 (hole-shape-idx state)) num-shapes) ])))

(define (prune-coords coords)
  (let ([ y (lowest-column-height coords) ])
    (filter (λ (c)
              (>= (imag-part c) (- y 5)))
            coords)))

(define (lowest-column-height coords) (list-min (column-heights coords)))

(define (column-heights coords)
  (let loop ([ coords coords ][ c0 0 ][ c1 0 ] [c2 0 ][ c3 0 ][ c4 0 ][ c5 0 ][ c6 0 ])
    (if (null? coords)
        (list c0 c1 c2 c3 c4 c5 c6)
        (let* ([ x  (real-part (car coords))   ]
               [ y  (imag-part (car coords))   ]
               [ c0 (if (= x 0) (max c0 y) c0) ]
               [ c1 (if (= x 1) (max c1 y) c1) ]
               [ c2 (if (= x 2) (max c2 y) c2) ]
               [ c3 (if (= x 3) (max c3 y) c3) ]
               [ c4 (if (= x 4) (max c4 y) c4) ]
               [ c5 (if (= x 5) (max c5 y) c5) ]
               [ c6 (if (= x 6) (max c6 y) c6) ])
          (loop (cdr coords) c0 c1 c2 c3 c4 c5 c6)))))

(define (part1 state N)
  (let loop ([ state state ][ heights '(0) ])
    (if (>= (hole-num-rocks state) N)
        state
        (let ([ state (drop-shape state) ])
          (loop state (cons (hole-height state) heights))))))

(time (check-equal? (hole-height (part1 init-state 2022)) +3157i))
