#lang racket

(provide phone-number bad-number)

(define bad-number "0000000000")

(define (phone-number s)
  (let* ([ number (string-replace s #px"[-+.() ]" "") ]
         [ len (string-length number) ]
         [ 1st-is-1? (λ () (char=? #\1 (string-ref number 0))) ])
    (cond [ (= len 10)                   number               ]
          [ (and (= len 11) (1st-is-1?)) (substring number 1) ]
          [ else                         bad-number           ])))

