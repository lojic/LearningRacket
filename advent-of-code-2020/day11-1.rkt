#lang racket

;; The goals today were:
;; 1) To write Part 1 in such a way as to maximize reuse for Part 2
;; 2) To minimize allocation
;; 3) To be reasonably readable & testable

(provide (struct-out plan)
         run
         valid-row?
         valid-col?
         vget
         is-empty?
         is-occupied?)

(struct plan (v rows cols) #:transparent)

(define (pos obj row col)       (+ (* row (plan-cols obj)) col))
(define (valid-col? obj row)    (< -1 row (plan-cols obj)))
(define (valid-row? obj row)    (< -1 row (plan-rows obj)))
(define (vget obj row col)      (vector-ref (plan-v obj) (pos obj row col)))
(define (vset! obj row col val) (vector-set! (plan-v obj) (pos obj row col) val))
(define is-empty?               (curry char=? #\L))
(define is-occupied?            (curry char=? #\#))
(define surrounding-deltas '((-1 . -1) (-1 . 0) (-1 . 1) (0 . -1) (0 . 1) (1 . -1) (1 . 0) (1 . 1)))

(define (run fname get-seat min-occupied)
  (let* ([ src (get-plan fname) ]
         [ dst (struct-copy plan src [ v (vector-copy (plan-v src)) ]) ])
    (let loop ()
      (set-seats! src dst get-seat min-occupied)
      (if (equal? (plan-v src) (plan-v dst))
          (vector-count (curry char=? #\#) (plan-v dst))
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
    (let-values ([ (seat)           (vget src row col)                     ]
                 [ (occupied empty) (count-surrounding src get-seat row col) ])
      (cond [ (and (is-empty? seat) (= occupied 0))
              (vset! dst row col #\#) ]
            [ (and (is-occupied? seat) (>= occupied min-occupied))
              (vset! dst row col #\L) ]))))

(define (count-surrounding obj get-seat row col)
  (let loop ([ lst surrounding-deltas ][ occupied 0 ][ empty 0 ])
    (if (null? lst)
        (values occupied empty)
        (match-let* ([ (cons dr dc) (car lst)  ])
          (match (get-seat obj row col dr dc)
            [ #\L (loop (cdr lst) occupied (add1 empty)) ]
            [ #\# (loop (cdr lst) (add1 occupied) empty) ]
            [ _   (loop (cdr lst) occupied empty)        ])))))

(define (get-seat obj row col dr dc)
  (let ([ r (+ row dr) ]
        [ c (+ col dc) ])
    (if (and (valid-row? obj r) (valid-col? obj c))
        (vget obj r c)
        #\.)))

(module+ test
  (require rackunit)

  (check-equal? (run "day11-test.txt" get-seat 4) 37)
  (check-equal? (run "day11.txt" get-seat 4) 2406)

  (let ([ obj (get-plan "day11-test.txt") ])
    (check-equal? (plan-rows obj) 10)
    (check-equal? (plan-cols obj) 10)

    (check-equal? (vector-length (plan-v obj)) (* (plan-rows obj) (plan-cols obj)))

    (check-equal? (vget obj 0 0) #\L)
    (check-equal? (vget obj 0 1) #\.)
    (check-equal? (vget obj 0 2) #\L)
    (check-equal? (vget obj 0 9) #\L)
    (check-equal? (vget obj 1 0) #\L)
    (check-equal? (vget obj 1 1) #\L)
    (check-equal? (vget obj 1 9) #\L)
    (check-equal? (vget obj 9 0) #\L)
    (check-equal? (vget obj 9 1) #\.)
    (check-equal? (vget obj 9 9) #\L)

    (let-values ([ (occupied empty) (count-surrounding obj get-seat 0 0) ])
      (check-equal? occupied 0)
      (check-equal? empty 2))

    (let-values ([ (occupied empty) (count-surrounding obj get-seat 1 1) ])
      (check-equal? occupied 0)
      (check-equal? empty 6))

    (let-values ([ (occupied empty) (count-surrounding obj get-seat 5 5) ])
      (check-equal? occupied 0)
      (check-equal? empty 5))

    (check-equal? (vget obj 0 0) #\L)
    (vset! obj 0 0 #\#)
    (check-equal? (vget obj 0 0) #\#)

    (check-equal? (vget obj 0 2) #\L)
    (vset! obj 0 2 #\#)
    (check-equal? (vget obj 0 2) #\#)

    (let-values ([ (occupied empty) (count-surrounding obj get-seat 1 1) ])
      (check-equal? occupied 2)
      (check-equal? empty 4))))
