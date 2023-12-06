#lang racket

(define (solve races)
  (define (records t d)
    (length (filter (λ (d1) (> d1 d)) (map (λ (b) (* b (- t b))) (inclusive-range 0 t)))))

  (foldl * 1 (map (λ (pair)
                    (records (car pair) (cdr pair))) races)))

;; Tests --------------------------------------------------------------------------------------

(module+ test
  (require rackunit)
  (check-equal? (solve '((47 . 207)(84 . 1394)(74 . 1209)(67 . 1014))) 741000)
  (check-equal? (solve '((47847467 . 207139412091014))) 38220708))
