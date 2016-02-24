#lang racket

(provide anagrams)

(define (anagrams word candidates)
  (define base (string-downcase word))
  (filter (λ (candidate) (anagram? base (string-downcase candidate)))
          candidates))

(define (anagram? a b)
  (if (string=? a b)
      #f
      (apply equal?
             (map (λ (e) (sort (string->list e) char<?))
                  (list a b)))))
