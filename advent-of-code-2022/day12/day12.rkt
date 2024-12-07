#lang racket
(require "../advent.rkt")

(struct part (valid? goal? visited))

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

(define (get-candidates part pos len)
  (let ([ height (vget pos) ])
    (~> (map (curry + pos) dirs)
        (filter (λ (pos*)
                  (and (in-bounds? pos*)                    ; In bounds
                       ((part-valid? part) pos* height)     ; Part specific validation
                       (< len (hash-ref (part-visited part) ; Not already seen with <= len
                                        pos*
                                        MAX-INTEGER))))
                _))))

(define (solve part pos len)
  (hash-set! (part-visited part) pos len)
  (if ((part-goal? part) pos)
      len
      (let* ([ len*       (add1 len)                     ]
             [ candidates (get-candidates part pos len*) ])
        (and (not (null? candidates))
             (let ([ lengths (filter identity
                                     (map (λ (pos*)
                                            (solve part pos* len*))
                                          candidates)) ])
               (and (not (null? lengths))
                    (car (sort lengths <))))))))

(define part1 (part (λ (pos height)
                      (<= (- (vget pos) height) 1))
                    (curry = E)
                    (make-hash)))

(define part2 (part (λ (pos height)
                      (<= (- height (vget pos)) 1))
                    (compose (curry = (vget S)) vget)
                    (make-hash)))

(time (check-equal? (solve part1 S 0) 490))
(time (check-equal? (solve part2 E 0) 488))
