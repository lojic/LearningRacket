#lang racket

;; Version precomputing the first seats. I got the idea from:
;; https://github.com/tckmn/polyaoc-2020/blob/master/11/rb/11.rb

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

(define (get-sight obj)
  (let ([ vec (struct-copy plan obj [ v (make-vector (vector-length (plan-v obj)) #f) ]) ])
    (for* ([ row (in-range (plan-rows obj)) ]
           [ col (in-range (plan-cols obj)) ])
      (let ([ seat (vget obj row col) ])
        (when (not (is-floor? seat))
          (vset! vec row col (sight-coords obj row col)))))
    vec))

(define (sight-coords obj row col)
  (let loop ([ lst surrounding-deltas ][ result '() ])
    (if (null? lst)
        result
        (match-let* ([ (cons dr dc) (car lst) ])
          (let ([ pair (get-first-seat obj row col dr dc) ])
            (loop (cdr lst) (if pair
                                (cons pair result)
                                result)))))))

(define (get-first-seat obj row col dr dc)
  (let loop ([ x (+ row dr) ][ y (+ col dc) ])
    (if (valid-row-col? obj x y)
        (let ([ seat (vget obj x y) ])
          (if (is-floor? seat)
              (loop (+ x dr) (+ y dc))
              (cons (- x row) (- y col))))
        #f)))

(define (run fname)
  (let* ([ src (get-plan fname) ]
         [ dst (struct-copy plan src [ v (vector-copy (plan-v src)) ]) ]
         [ sight (get-sight src) ])
    (let loop ()
      (set-seats! src dst sight)
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

(define (set-seats! src dst sight)
  (for* ([ row (in-range (plan-rows src)) ]
         [ col (in-range (plan-cols src)) ])
    (let ([ seat (vget src row col) ])
      (when (not (is-floor? seat))
        (let ([ occupied (count-surrounding src (vget sight row col) row col) ])
          (cond [ (and (is-empty? seat) (= occupied 0))
                  (vset! dst row col occupied-loc) ]
                [ (and (is-occupied? seat) (>= occupied 5))
                  (vset! dst row col empty-loc) ]))))))

(define (count-surrounding obj coords row col)
  (let loop ([ lst coords ][ occupied 0 ])
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

  (check-equal? (run "day11-test.txt") 26)
  (check-equal? (run "day11.txt") 2149)

  ;; get-first-seat ---------------------------------------------------------------------------

  (let ([ obj (get-plan "day11-test.txt") ])
    (check-false (get-first-seat obj 0 0 -1 -1))
    (check-false (get-first-seat obj 0 0 -1 0))
    (check-false (get-first-seat obj 0 0 -1 1))
    (check-false (get-first-seat obj 0 0 0 -1))
    (check-false (get-first-seat obj 0 0 1 -1))

    (check-equal? (get-first-seat obj 0 0 0 1) '(0 . 2))
    (check-equal? (get-first-seat obj 0 0 1 1) '(1 . 1))
    (check-equal? (get-first-seat obj 0 0 1 0) '(1 . 0))
    )

  (let ([ obj (get-plan "day11-test2.txt") ])
    (check-equal? (get-first-seat obj 4 3 -1 -1) '(-2 . -2))
    (check-equal? (get-first-seat obj 4 3 -1 0)  '(-3 . 0))
    (check-equal? (get-first-seat obj 4 3 -1 1)  '(-4 . 4))
    (check-equal? (get-first-seat obj 4 3 0 -1)  '(0 . -1))
    (check-equal? (get-first-seat obj 4 3 0 1)   '(0 . 5))
    (check-equal? (get-first-seat obj 4 3 1 -1)  '(3 . -3))
    (check-equal? (get-first-seat obj 4 3 1 0)   '(4 . 0))
    (check-equal? (get-first-seat obj 4 3 1 1)   '(1 . 1)))

  ;; sight-coords -----------------------------------------------------------------------------
  (let* ([ obj (get-plan "day11-test2.txt") ])
    (check-equal? (sight-coords obj 4 3) '((1 . 1)(4 . 0)(3 . -3)(0 . 5)(0 . -1)(-4 . 4)
                                           (-3 . 0)(-2 . -2)))
    (check-equal? (sight-coords obj 4 4) '((1 . 0)(0 . 4)(0 . -1)))

    )

  (let* ([ obj (get-plan "day11-test.txt") ])
    (check-equal? (sight-coords obj 0 0) '((1 . 1)(1 . 0)(0 . 2)))
    )

  ;; get-sight --------------------------------------------------------------------------------
  (let* ([ obj   (get-plan "day11-test.txt") ]
         [ sight (get-sight obj)             ])
    (check-equal? (vget sight 0 0) '((1 . 1)(1 . 0)(0 . 2)))
    )

  )
