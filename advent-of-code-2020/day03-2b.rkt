#lang racket

;; Concise solution
(foldl (λ (delta result)
         (* result
            (for/sum ([ line (file->lines "day03.txt") ]
                      [ x (in-range 0 +inf.0 delta) ])
              (if (and (exact-integer? x)
                       (char=? #\# (string-ref line (modulo x (string-length line)))))
                  1 0))))
       1
       '(1 3 5 7 1/2))
