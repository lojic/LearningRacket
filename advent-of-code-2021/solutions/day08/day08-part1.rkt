#lang racket

(require "../../advent/advent.rkt" threading)

(~>> (file->lines "day08.txt")
     (map (compose (curry map string-split)
                   (curryr string-split " | ")))
     (map (compose length
                   (curry filter (λ (s)
                                  (member (string-length s) '(2 3 4 7))))
                   second))
     sum)
