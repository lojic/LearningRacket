#lang racket

(require "../../advent/advent.rkt" threading)

(define (part n)
  ;; Sum the vector result of iterating the simulation
  (vector-sum

   ;; Iterate a simulation step function n times
   (iterate

    ;; The simulation step function.
    (λ (v)
      (define vr (curry vector-ref v))
      ;; Rotate the vector e.g. 1's become 0's, 2's become 1's,
      ;; etc. The exception is for the new 6's - these will come from
      ;; *both* 0's and 7's
      (vector (vr 1) (vr 2) (vr 3) (vr 4) (vr 5) (vr 6) (+ (vr 0) (vr 7)) (vr 8) (vr 0)))

    ;; The initial argument to the iteration. Obtain a list of
    ;; integers from the input and create an initial vector where each
    ;; slot contains the count of fishes.
    (foldl

     ;; Fold function - increment the vector slot corresponding to n
     (λ (n v)
       (vector-set! v n (add1 (vector-ref v n))) v)

     ;; Initial value - a vector of zeros
     (make-vector 9 0)

     ;; List of numbers to fold over
     (~> (file->string "day06.txt")
         string-trim
         (string-split ",")
         (map string->number _)))

    ;; The number of times to iterate
    n)))

;; Tests --------------------------------------------------------------------------------------

(module+ test
  (require rackunit)
  (check-equal? (part 80) 371379)
  (check-equal? (part 256) 1674303997472))
