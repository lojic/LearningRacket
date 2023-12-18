#lang racket
(require "../advent.rkt" data/heap)

(define-values (grid goal width height)
  (let* ([ lines  (parse-aoc 17 string->list)      ]
         [ width  (length (car lines))             ]
         [ height (length lines)                   ]
         [ grid   (make-vector (* width height) 0) ])
    (for ([ line (in-list lines) ]
          [ y  (in-naturals)   ])
      (for ([ loss (in-list line) ]
            [ x  (in-naturals)  ])
        (vector-set! grid (+ (* y width) x) (- (char->integer loss) 48))))
    (values grid (make-rectangular (sub1 width) (sub1 height)) width height)))

(struct state (pos loss prev history) #:transparent)

(define (goal? n obj)
  (and (= goal (state-pos obj))
       (>= (length (same-dir obj)) n)))

(define (same-dir obj [ dir #f ])
  (let ([ history (state-history obj) ])
    (cond [ (null? history) '() ]
          [ else (let ([ dir (or dir (car history)) ])
                   (takef history (λ (n) (= n dir)))) ])))

(define ((get-candidates min-dir max-dir) obj)
  (match-let ([ (state pos loss prev history) obj ])
    (map (λ (n)
           (let* ([ pos*  (+ pos n)                     ]
                  [ loss* (+ loss (vector-ref grid (+ (* (imag-part pos*) width) (real-part pos*)))) ])
             (state pos* loss* pos (cons n history))))
         (filter (λ (n)
                   (let ([ next (+ pos n) ])
                     (cond [ (= next prev) #f ] ; Can't be previous position
                           [ (not (and (<= 0 (real-part next) (sub1 width))
                                       (<= 0 (imag-part next) (sub1 height)))) #f ] ; Must be in grid
                           [ else
                             (let* ([ same (same-dir obj) ]
                                    [ len  (length same)  ])
                               (cond [ (= max-dir len)   (not (= n (car same))) ] ; At most max-dir moves in same dir
                                     [ (< 0 len min-dir) (= n (car same))       ]  ; < min-dir, must be same dir
                                     [ else              #t                     ])) ])))
                 '(1 +i -1 -i)))))

(define (solve get-candidates goal?)
  (let ([ seen  (mutable-set) ]
        [ queue (make-heap (λ (a b)
                             (<= (state-loss a) (state-loss b)))) ])
    (heap-add! queue (state 0 0 -7 '()))

    (let bfs ()
      (let ([ obj (heap-min queue) ])
        (heap-remove-min! queue)
        (let ([ key (apply string-append (number->string (state-pos obj)) (map number->string (same-dir obj))) ])
          (cond [ (set-member? seen key) (bfs) ]
                [ else (set-add! seen key)
                       (let* ([ candidates (get-candidates obj)     ]
                              [ final      (findf goal? candidates) ])
                         (cond [ final (state-loss final) ]
                               [ else (for ([ obj (in-list candidates) ])
                                        (heap-add! queue obj))
                                      (bfs) ])) ]))))))

(define (part1)
  (solve (get-candidates 0 3) (curry goal? 1)))

(define (part2)
  (solve (get-candidates 4 10) (curry goal? 4)))

;; Tests --------------------------------------------------------------------------------------

(check-equal? (time (part1)) 1246)
(check-equal? (time (part2)) 1389)
