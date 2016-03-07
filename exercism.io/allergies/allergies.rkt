#lang racket
(provide allergen-list)

(define allergens #hash(("cats" . 128)
                        ("pollen" . 64)
                        ("chocolate" . 32)
                        ("tomatoes" . 16)
                        ("strawberries" . 8)
                        ("shellfish" . 4)
                        ("peanuts" . 2)
                        ("eggs" . 1)))

(define (allergen-list flags)
  (for/list ([(allergen _) allergens]
             #:when (allergic-to? flags allergen))
            allergen))

(define (allergic-to? flags item)
  (> (bitwise-and (hash-ref allergens item)
                  flags)
     0))