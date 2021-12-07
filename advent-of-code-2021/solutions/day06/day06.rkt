#lang racket

(require "../../advent/advent.rkt" threading)

(define fish (~>> (file->string "day06.txt")
                  string-trim
                  (string-split _ ",")
                  (map string->number)
                  (foldl (λ (n v) (vector-update! v n add1) v)
                         (make-vector 9 0))))

(define (solve fish n)
  (define (spawn v)
    (define vr (curry vector-ref v))
    (vector (vr 1) (vr 2) (vr 3) (vr 4) (vr 5) (vr 6) (+ (vr 0) (vr 7)) (vr 8) (vr 0)))

  (vector-sum (iterate spawn fish n)))

;; Tests --------------------------------------------------------------------------------------

(module+ test
  (require rackunit)
  (check-equal? (solve fish 80) 371379)
  (check-equal? (solve fish 256) 1674303997472))
