#lang racket

;; The goals today were:
;; 1) To write Part 1 in such a way as to maximize reuse for Part 2
;; 2) To minimize allocation
;; 3) To be reasonably readable & testable

(provide (struct-out plan)
         floor-loc
         is-empty?
         is-occupied?
         run
         valid-row-col?
         vget)

(struct plan (v rows cols) #:transparent)

(define (pos obj row col)        (+ (* row (plan-cols obj)) col))
(define (valid-row-col? obj r c) (and (< -1 r (plan-rows obj)) (< -1 c (plan-cols obj))))
(define (vget obj row col)       (vector-ref (plan-v obj) (pos obj row col)))
(define (vset! obj row col val)  (vector-set! (plan-v obj) (pos obj row col) val))
(define empty-loc                #\L)
(define floor-loc                #\.)
(define occupied-loc             #\#)
(define (is-empty? seat)         (char=? empty-loc seat))
(define (is-floor? seat)         (char=? floor-loc seat))
(define (is-occupied? seat)      (char=? occupied-loc seat))
(define surrounding-deltas '((-1 . -1) (-1 . 0) (-1 . 1) (0 . -1) (0 . 1) (1 . -1) (1 . 0) (1 . 1)))

(define (run fname get-seat min-occupied)
  (let* ([ src (get-plan fname) ]
         [ dst (struct-copy plan src [ v (vector-copy (plan-v src)) ]) ])
    (let loop ()
      (set-seats! src dst get-seat min-occupied)
      (if (equal? (plan-v src) (plan-v dst))
          (vector-count (curry char=? occupied-loc) (plan-v dst))
          (begin
            (vector-copy! (plan-v src) 0 (plan-v dst))
            (loop))))))

(define (get-plan fname)
  (let* ([ lines (file->lines fname)         ]
         [ cols  (string-length (car lines)) ]
         [ rows  (length lines)              ]
         [ v     (make-vector (* rows cols)) ]
         [ obj   (plan v rows cols)          ])
    (for ([ row (in-range rows) ]
          [ str (in-list lines) ])
      (for ([ col (in-range cols) ])
        (vset! obj row col (string-ref str col))))
    obj))

(define (set-seats! src dst get-seat min-occupied)
  (for* ([ row (in-range (plan-rows src)) ]
         [ col (in-range (plan-cols src)) ])
    (let ([ seat (vget src row col) ])
      (when (not (is-floor? seat))
        (let ([ occupied (count-surrounding src get-seat row col) ])
          (cond [ (and (is-empty? seat) (= occupied 0))
                  (vset! dst row col occupied-loc) ]
                [ (and (is-occupied? seat) (>= occupied min-occupied))
                  (vset! dst row col empty-loc) ]))))))

(define (count-surrounding obj get-seat row col)
  (let loop ([ lst surrounding-deltas ][ occupied 0 ])
    (if (null? lst)
        occupied
        (match-let* ([ (cons dr dc) (car lst) ])
          (loop (cdr lst) (if (is-occupied? (get-seat obj row col dr dc))
                              (add1 occupied)
                              occupied))))))

(define (get-seat obj row col dr dc)
  (let ([ r (+ row dr) ][ c (+ col dc) ])
    (if (valid-row-col? obj r c) (vget obj r c) floor-loc)))

(module+ test
  (require rackunit)

  (check-equal? (run "day11-test.txt" get-seat 4) 37)
  (check-equal? (run "day11.txt" get-seat 4) 2406)

  (let ([ obj (get-plan "day11-test.txt") ])
    (check-equal? (plan-rows obj) 10)
    (check-equal? (plan-cols obj) 10)

    (check-equal? (vector-length (plan-v obj)) (* (plan-rows obj) (plan-cols obj)))

    (check-equal? (vget obj 0 0) empty-loc)
    (check-equal? (vget obj 0 1) floor-loc)
    (check-equal? (vget obj 0 2) empty-loc)
    (check-equal? (vget obj 0 9) empty-loc)
    (check-equal? (vget obj 1 0) empty-loc)
    (check-equal? (vget obj 1 1) empty-loc)
    (check-equal? (vget obj 1 9) empty-loc)
    (check-equal? (vget obj 9 0) empty-loc)
    (check-equal? (vget obj 9 1) floor-loc)
    (check-equal? (vget obj 9 9) empty-loc)

    (check-equal? (count-surrounding obj get-seat 0 0) 0)
    (check-equal? (count-surrounding obj get-seat 1 1) 0)
    (check-equal? (count-surrounding obj get-seat 5 5) 0)

    (check-equal? (vget obj 0 0) empty-loc)
    (vset! obj 0 0 occupied-loc)
    (check-equal? (vget obj 0 0) occupied-loc)

    (check-equal? (vget obj 0 2) empty-loc)
    (vset! obj 0 2 occupied-loc)
    (check-equal? (vget obj 0 2) occupied-loc)

    (check-equal? (count-surrounding obj get-seat 1 1) 2)))
