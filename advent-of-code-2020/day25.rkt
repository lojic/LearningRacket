#lang racket

(define input   '(3248366 4738476))
(define divisor 20201227)
(define subject 7)

(define (transform1 subject val)
  (remainder (* val subject) divisor))

(define (transform subject n [ val 1 ])
  (cond [ (< n 1) val ]
        [ else (transform subject
                          (sub1 n)
                          (transform1 subject val)) ]))

(define (find-loop-size subject key)
  (let loop ([ val 1 ][ n 0 ])
    (cond [ (= val key) n ]
          [ else (loop (transform1 subject val) (add1 n)) ])))

(define (encryption-key key1 key2)
  (transform key2 (find-loop-size subject key1)))

(module+ test
  (require rackunit)

  (check-equal? (find-loop-size subject 5764801) 8)
  (check-equal? (find-loop-size subject 17807724) 11)
  (check-equal? (transform subject 8) 5764801)
  (check-equal? (transform subject 11) 17807724)
  (check-equal? (encryption-key 5764801 17807724) 14897079)
  (check-equal? (encryption-key 17807724 5764801) 14897079)

  (check-equal? (apply encryption-key input) 18293391)) ; Part 1
