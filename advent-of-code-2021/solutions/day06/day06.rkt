#lang racket

(require "../../advent/advent.rkt")

(define (part n)
  (let* ([ fish (map string->number
                     (string-split (string-trim (file->string "day06.txt"))
                                   ",")) ]
         [ vr (λ (v i) (vector-ref v i)) ])
    (vector-sum (iterate (λ (v)
                           (vector (vr v 1) (vr v 2) (vr v 3) (vr v 4) (vr v 5) (vr v 6)
                                   (+ (vr v 0) (vr v 7)) (vr v 8) (vr v 0)))
                         (foldl (λ (n v)
                                  (vector-set! v n (add1 (vr v n)))
                                  v)
                                (make-vector 9 0)
                                fish)
                         n))))

;; Tests --------------------------------------------------------------------------------------

(module+ test
  (require rackunit)
  (check-equal? (part 80) 371379)
  (check-equal? (part 256) 1674303997472))
