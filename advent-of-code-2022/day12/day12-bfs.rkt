#lang racket
(require "../advent.rkt")
(require data/queue)

;; After noticing other people wisely using BFS, instead of DFS, I
;; created this BFS version.
;;
;; DFS run time: 29,000 ms
;; BFS run time:     51 ms :)

(struct part (valid? goal?))

(define-values (vec width height S E dirs)
  (let* ([ in      (parse-aoc 12)           ]
         [ width   (string-length (car in)) ]
         [ height  (length in)              ]
         [ i->c    (λ (i)
                     (let-values ([ (q r) (quotient/remainder i width) ])
                       (make-rectangular r q))) ]
         [ letters (string->list (apply string-append in)) ]
         [ S*      (index-of letters #\S char=?)           ]
         [ E*      (index-of letters #\E char=?)           ]
         [ vec     (list->vector letters)                  ])
    (vector-set! vec S* #\a)
    (vector-set! vec E* #\z)
    (vector-map! char->integer vec)
    (values vec width height (i->c S*) (i->c E*) '(-i 1 +i -1))))

(define (c->i c)       (+ (* width (imag-part c)) (real-part c)))
(define (vget c)       (vector-ref vec (c->i c)))
(define (in-bounds? c) (and (< -1 (real-part c) width)
                            (< -1 (imag-part c) height)))

(define (get-candidates part pos)
  (let ([ height (vget pos) ])
    (~> (map (curry + pos) dirs)
        (filter (λ (pos*)
                  (and (in-bounds? pos*)                    ; In bounds
                       ((part-valid? part) pos* height)))   ; Part specific validation
                _))))

(define (solve part pos*)
  (let ([ seen  (mutable-set) ]
        [ queue (make-queue)  ])
    (enqueue! queue (cons pos* 0))

    (let bfs ()
      (match-let ([ (cons pos len) (dequeue! queue) ])
        (cond [ (set-member? seen pos) (bfs) ]
              [ else (set-add! seen pos)
                     (let ([ candidates (get-candidates part pos) ]
                           [ len*       (add1 len)                ])
                       (cond [ (ormap (λ (pos*)
                                        ((part-goal? part) pos*)) candidates) len* ]
                             [ else (for ([ pos* (in-list candidates) ])
                                      (enqueue! queue (cons pos* len*)))
                                    (bfs) ])) ])))))

(define part1 (part (λ (pos height)
                      (<= (- (vget pos) height) 1))
                    (curry = E)))

(define part2 (part (λ (pos height)
                      (<= (- height (vget pos)) 1))
                    (compose (curry = (vget S)) vget)))

(time (check-equal? (solve part1 S) 490))
(time (check-equal? (solve part2 E) 488))
