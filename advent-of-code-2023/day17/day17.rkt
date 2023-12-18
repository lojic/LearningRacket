#lang racket
(require "../advent.rkt" data/heap)

(define-values (grid goal)
  (let ([ lines (parse-aoc 17 string->list) ])
    (values (grid->hash lines #:col-transform (λ (c) (- (char->integer c) 48)))
            (make-rectangular (sub1 (length (car lines))) (sub1 (length lines))))))

(struct state (pos loss prev history) #:transparent)

(define (goal? n obj)
  (and (= goal (state-pos obj))
       (>= (length (same-dir obj)) n)))

(define (cache-key obj)
  (string-join (map number->string (cons (state-pos obj) (same-dir obj))) ":"))

(define (same-dir obj [ dir #f ])
  (let ([ history (state-history obj) ])
    (cond [ (null? history) '() ]
          [ else (let ([ dir (or dir (car history)) ])
                   (takef history (λ (n) (= n dir)))) ])))

(define ((get-candidates min-dir max-dir) obj)
  (match-let ([ (state pos loss prev history) obj ])
    (map (λ (n)
           (let* ([ pos*  (+ pos n)                     ]
                  [ loss* (+ loss (hash-ref grid pos*)) ])
             (state pos* loss* pos (cons n history))))
         (filter (λ (n)
                   (let ([ next (+ pos n) ])
                     (cond [ (= next prev)                   #f ] ; Can't be previous position
                           [ (not (hash-has-key? grid next)) #f ] ; Must be in grid
                           [ else
                             (let* ([ same (same-dir obj) ]
                                    [ len  (length same)  ])
                               (cond [ (= max-dir len) (not (= n (car same))) ] ; At most max-dir moves in same dir
                                     [ (= 0 len)  #t ]                          ; No history, anything is ok
                                     [ (< len min-dir)  (= n (car same)) ]      ; < min-dir, must be same dir
                                     [ else  #t ])) ])))
                 '(1 +i -1 -i)))))

(define (solve get-candidates goal?)
  (let ([ seen  (mutable-set) ]
        [ queue (make-heap (λ (a b)
                             (<= (state-loss a) (state-loss b)))) ])
    (heap-add! queue (state 0 0 -7 '()))

    (let bfs ()
      (let ([ obj (heap-min queue) ])
        (heap-remove-min! queue)
        (let ([ key (cache-key obj) ])
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
