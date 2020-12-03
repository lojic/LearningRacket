#lang racket

(define test-file "day03-test.txt")
(define data-file "day03.txt")

(define (mod-ref str i) (string-ref str (modulo i (string-length str))))

;; loop version
(define (count-loop lines x delta)
  (let loop ([ lines lines ][ x x ][ count 0 ])
    (if (null? lines)
        count
        (loop (cdr lines) (+ x delta) (if (and (exact-integer? x)
                                               (char=? #\# (mod-ref (car lines) x)))
                                          (add1 count)
                                          count)))))

;; for version
(define (count-for lines x delta)
  (for/sum ([ line lines ]
            [ x (in-range 0 +inf.0 delta) ])
    (if (and (exact-integer? x) (char=? #\# (mod-ref line x))) 1 0)))

(define (count-product f slopes lines)
  (apply * (map (λ (s) (f lines 0 s)) slopes)))

(module+ main
  (printf "Solution using loop is ~a\n"
          (count-product count-loop '(1 3 5 7 1/2) (file->lines data-file)))
  (printf "Solution using for is ~a\n"
          (count-product count-for '(1 3 5 7 1/2) (file->lines data-file))))

(module+ test
  (require rackunit)

  ;; ------------------------------------------------------------------------------------------
  ;; Sample data
  ;; ------------------------------------------------------------------------------------------
  (let ([ lines (file->lines test-file) ])
    (check-equal? (count-loop lines 0 1) 2)
    (check-equal? (count-for lines 0 1) 2)
    (check-equal? (count-loop lines 0 3) 7)
    (check-equal? (count-for lines 0 3) 7)
    (check-equal? (count-loop lines 0 5) 3)
    (check-equal? (count-for lines 0 5) 3)
    (check-equal? (count-loop lines 0 7) 4)
    (check-equal? (count-for lines 0 7) 4)
    (check-equal? (count-loop lines 0 1/2) 2)
    (check-equal? (count-for lines 0 1/2) 2)

    (check-equal? (count-product count-loop '(1 3 5 7 1/2) lines) 336)
    (check-equal? (count-product count-for '(1 3 5 7 1/2) lines) 336))

  ;; ------------------------------------------------------------------------------------------
  ;; Solution
  ;; ------------------------------------------------------------------------------------------
  (let ([ lines (file->lines data-file) ])
    (check-equal? (count-loop lines 0 3) 169)
    (check-equal? (count-for lines 0 3) 169)

    (check-equal? (count-product count-loop '(1 3 5 7 1/2) lines) 7560370818)
    (check-equal? (count-product count-for '(1 3 5 7 1/2) lines) 7560370818))

  )
