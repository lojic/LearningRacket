#lang racket

(define test-file "day03-test.txt")
(define data-file "day03.txt")

(define (mod-ref str i) (string-ref str (modulo i (string-length str))))

;; loop version
(define (count-loop lines x delta)
  (let loop ([ lines lines ][ x x ][ count 0 ])
    (if (null? lines)
        count
        (loop (cdr lines) (+ x delta) (if (char=? #\# (mod-ref (car lines) x))
                                          (add1 count)
                                          count)))))

;; for version
(define (count-for lines x delta)
  (for/sum ([ line lines ]
            [ x (in-range 0 +inf.0 delta) ])
    (if (char=? #\# (mod-ref line x)) 1 0)))

(module+ main
  (printf "Count using loop is ~a\n"
          (count-loop (file->lines data-file) 0 3))
  (printf "Count using for is ~a\n"
          (count-for (file->lines data-file) 0 3)))

(module+ test
  (require rackunit)

  ;; ------------------------------------------------------------------------------------------
  ;; Sample data
  ;; ------------------------------------------------------------------------------------------
  (check-equal? (count-loop (file->lines test-file) 0 3) 7)
  (check-equal? (count-for (file->lines test-file) 0 3) 7)

  ;; ------------------------------------------------------------------------------------------
  ;; Solution
  ;; ------------------------------------------------------------------------------------------
  (check-equal? (count-loop (file->lines data-file) 0 3) 169)
  (check-equal? (count-for (file->lines data-file) 0 3) 169)

  )
