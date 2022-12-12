#lang racket
(require "../advent.rkt")
(require rackunit)

(struct part (valid? goal?))

(define-values (vec width height S E)
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
    (values vec width height (i->c S*) (i->c E*))))

(define (c->i c)       (+ (* width (imag-part c)) (real-part c)))
(define (vget c)       (vector-ref vec (c->i c)))
(define (in-bounds? c) (and (< -1 (real-part c) width)
                            (< -1 (imag-part c) height)))

(define (part1-valid? pos height)
  (<= (- (vget pos) height) 1))

(define dirs    '(-i 1 +i -1))
(define visited (make-hash))

(define (get-candidates part pos len)
  (let ([ height (vget pos) ])
    (~> (map (λ (dir)
               (+ pos dir)) dirs)
        (filter (λ (pos*)
                  (and (in-bounds? pos*)                         ; In bounds
                       ((part-valid? part) pos* height)          ; Not too high
                       (< len (hash-ref visited pos* 1000000)))) ; Not already seen with <= len
                _))))

(define (solve part pos len)
  (hash-set! visited pos len)
  (if ((part-goal? part) pos)
      len
      (let* ([ len*       (add1 len)                ]
             [ candidates (get-candidates part pos len*) ])
        (and (not (null? candidates))
             (let ([ lengths (filter identity
                                     (map (λ (pos*)
                                            (solve part pos* len*))
                                          candidates)) ])
               (and (not (null? lengths))
                    (car (sort lengths <))))))))

(define part1-goal? (curry = E))

(define part1 (part part1-valid? part1-goal?))

(time (check-equal? (solve part1 S 0) 490))
