#lang racket
(require "../advent.rkt")
(require data/queue)

(define-values (in end h-vec v-vec width height)
  (let* ([ in     (~> (parse-aoc 24 string->list #:print-sample #f)
                      (map (λ (line)
                             (let ([ len (- (length line) 2) ])
                               (take (drop line 1) len)))
                           _))                                  ]
         [ width  (length (car in))                             ]
         [ height (length in)                                   ]
         [ end    (make-rectangular (sub1 width) (sub1 height)) ]
         [ h-vec  (make-vector (* width height) #f)             ]
         [ v-vec  (make-vector (* width height) #f)             ])
    (values in end h-vec v-vec width height)))

(define (c->i c)          (+ (real-part c) (* width (imag-part c))))
(define (vget vec c)      (vector-ref vec (c->i c)))
(define (vset! vec c val) (vector-set! vec (c->i c) val))
(define (wrap m val)      (if (< val 0) m (if (> val m) 0 val)))

(define (part1)
  (init-blizzards)
  (solve 0 end 0))

(define (part2)
  (init-blizzards)
  (~> (solve 0 end 0)
      (solve end 0 _)
      (solve 0 end _)))

(define (solve start end step)
  (let ([ seen  (mutable-set) ]
        [ queue (make-queue)  ])
    (enqueue! queue (cons start step))

    (let bfs ()
      (match-let ([ (cons pos step) (dequeue! queue) ])
        (let* ([ hmod  (modulo step width)        ]
               [ vmod  (modulo step (- height 2)) ]
               [ state (list pos hmod vmod)       ])
          (cond [ (set-member? seen state) (bfs) ]
                [ else (set-add! seen state)
                       (let* ([ step*      (add1 step)                ]
                              [ candidates (get-candidates step* pos) ])
                         (cond [ (ormap (λ (p) (= p end)) candidates) step* ]
                               [ else (for ([ pos* (in-list candidates) ])
                                        (enqueue! queue (cons pos* step*)))
                                      (bfs) ]))]))))))

(define (get-candidates step pos)
  (let ([ hmod (modulo step width)        ]
        [ vmod (modulo step (- height 2)) ])
    (~> (map (λ (dir)
               (+ pos dir))
             '(-i 1 +i -1 0))
        (filter (λ (pos*)
                  (and (in-bounds? pos*)
                       (let ([ hbliz (vget h-vec pos*) ]
                             [ vbliz (vget v-vec pos*) ])
                         (and (not (vector-ref hbliz hmod))
                              (not (vector-ref vbliz vmod))))))
                  _))))

(define (in-bounds? pos)
  (let ([ x (real-part pos) ]
        [ y (imag-part pos) ])
    (and (<= 0 x (sub1 width))
         (<= 0 y (sub1 height)))))

(define (init-blizzards)
  (for* ([ x (in-range width)  ]
         [ y (in-range height) ])
    (vset! h-vec (make-rectangular x y) (make-vector width #f))
    (vset! v-vec (make-rectangular x y) (make-vector height #f)))

  (for ([ row (in-list in)  ]
        [ y   (in-naturals) ])
    (for ([ col (in-list row) ]
          [ x   (in-naturals) ])
      (match col
        [ #\# (update-wall!  x y)    ]
        [ #\< (update-h-vec! x y -1) ]
        [ #\> (update-h-vec! x y 1)  ]
        [ #\^ (update-v-vec! x y -1) ]
        [ #\v (update-v-vec! x y 1)  ]
        [ _   (void)                 ]))))

(define (update-wall! x y)
  (let ([ blizzards (vget h-vec (make-rectangular x y)) ])
    (for ([ m (in-range width) ])
      (vector-set! blizzards m #t))))

(define (update-h-vec! x y step)
  (let loop ([ i 0 ][ m (if (negative? step) x (modulo (- width x) width)) ])
    (when (< i width)
      (let ([ blizzards (vget h-vec (make-rectangular i y)) ])
        (vector-set! blizzards m #t)
        (loop (add1 i) (wrap (sub1 width) (+ m step)))))))

(define (update-v-vec! x y step)
  (let loop ([ i 1 ][ m (if (negative? step) (sub1 y) (modulo (- height (add1 y)) (- height 2))) ])
    (when (< i (sub1 height))
      (let ([ blizzards (vget v-vec (make-rectangular x i)) ])
        (vector-set! blizzards m #t)
        (loop (add1 i) (wrap (- height 3) (+ m step)))))))

(time (check-equal? (part1) 308))
(time (check-equal? (part2) 908))
