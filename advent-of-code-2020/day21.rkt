#lang racket
(require graph threading rackunit)

(struct food (ingredients allergens ing-set inert) #:transparent)

(define (parse-input fname)
  (let* ([ obj           (foldl parse-line (food '() (make-hash) #f #f) (file->lines fname)) ]
         [ ing-set       (apply set-union (food-ingredients obj))                            ]
         [ non-allergens (set-subtract ing-set (apply set-union (hash-values (food-allergens obj)))) ])
    (struct-copy food obj [ ing-set ing-set ][ inert non-allergens ])))

(define (parse-line line obj)
  (let* ([ m (regexp-match #px"^([a-z ]+) \\(contains ([a-z, ]+)\\)$" line) ]
         [ ingredients (list->set (string-split (second m) " ")) ]
         [ allergens   (string-split (third m) ", ")             ]
         [ hsh         (food-allergens obj)                      ])
    (for ([ allergen (in-list allergens) ])
      (hash-set! hsh allergen (let ([ st (hash-ref hsh allergen #f) ])
                                (if st
                                    (set-intersect st ingredients)
                                    ingredients))))
    (struct-copy food obj [ ingredients (cons ingredients (food-ingredients obj)) ])))

(define (part1 obj)
  (for/sum ([ non (in-set (food-inert obj)) ])
    (for/sum ([ ing (in-list (food-ingredients obj)) ])
      (if (set-member? ing non) 1 0))))

(define (part2 obj)
  (let ([ hsh (food-allergens obj) ])
    (~> (sort (maximum-bipartite-matching
               (undirected-graph (for*/list ([ allergen (in-list (hash-keys hsh)) ]
                                             [ ing (in-set (hash-ref hsh allergen)) ])
                                   (list allergen ing)))) string<? #:key second)
        (map first _)
        (string-join _ ","))))

(check-equal? (part1 (parse-input "day21.txt")) 2428)
(check-equal? (part2 (parse-input "day21.txt")) "bjq,jznhvh,klplr,dtvhzt,sbzd,tlgjzx,ctmbr,kqms")
