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

(define (part1 N [ state init-state ])
  (if (>= (hole-num-rocks state) N)
      state
      (part1 N (drop-shape state))))

(define (part2 N)
  (let loop ([ state init-state ][ seen (hash) ])
    (let* ([ state   (drop-shape state) ]
           [ l-state (loop-state state) ])
      (if (hash-has-key? seen l-state)
          (match-let* ([ (cons beg-rocks beg-height) (hash-ref seen l-state)              ]
                       [ loop-rocks                  (- (hole-num-rocks state) beg-rocks) ])
            (let-values ([ (num-loops rem-rocks) (quotient/remainder (- N beg-rocks) loop-rocks) ])
              (let ([ height-gained (- (hole-height state) beg-height)   ]
                    [ state         (iterate drop-shape state rem-rocks) ])
                (+ (hole-height state) (* height-gained (sub1 num-loops))))))
          (loop state (hash-set seen
                                l-state
                                (cons (hole-num-rocks state)
                                      (hole-height state))))))))

(define (drop-shape state)
  (let loop ([ state state ][ shape (shape-create state) ])
    (let* ([ jet    (next-jet state)             ]
           [ shape  (shape-push state shape jet) ]
           [ shape* (shape-fall state shape)     ]
           [ state* (increment-jet state)        ])
      (if (equal? (shape-height shape) (shape-height shape*))
          (add-rock state* shape*)
          (loop state* shape*)))))

(define (shape-create state)
  (map (λ (c)
         (+ c 2 +3i (hole-height state)))
       (get-shape (hole-shape-idx state))))

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

(define (shape-height shape)
  (* +i (add1 (for/fold ([ height 0 ])
                        ([ c shape ])
                (max height (imag-part c))))))

(define (prune-coords coords)
  (let ([ y (lowest-column-height coords) ])
    (filter (λ (c)
              (>= (imag-part c) (- y 5)))
            coords)))

(define (lowest-column-height coords) (list-min (column-heights coords)))

(define (normalize-column-heights heights)
  (let ([ m (list-min heights) ])
    (map (λ (y) (- y m)) heights)))

(define (column-heights coords)
  (let loop ([ coords coords ][ c0 0 ][ c1 0 ] [c2 0 ][ c3 0 ][ c4 0 ][ c5 0 ][ c6 0 ])
    (if (null? coords)
        (list c0 c1 c2 c3 c4 c5 c6)
        (let ([ x (real-part (car coords)) ]
              [ y (imag-part (car coords)) ])
          (loop (cdr coords)
                (if (= x 0) (max c0 y) c0)
                (if (= x 1) (max c1 y) c1)
                (if (= x 2) (max c2 y) c2)
                (if (= x 3) (max c3 y) c3)
                (if (= x 4) (max c4 y) c4)
                (if (= x 5) (max c5 y) c5)
                (if (= x 6) (max c6 y) c6))))))

(define (loop-state state)
  (list (normalize-column-heights (column-heights (hole-coords state)))
        (hole-shape-idx state)
        (length (hole-jets state))))

(time (check-equal? (hole-height (part1 2022)) +3157i))
(time (check-equal? (part2 1000000000000) +1581449275319i))
