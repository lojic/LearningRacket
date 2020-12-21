#lang racket
(require graph threading)

(struct food (ingredients allergens ing-set inert) #:mutable #:transparent)

(define (parse-input fname)
  (let* ([ obj           (foldl parse-line (food '() (make-hash) #f #f) (file->lines fname)) ]
         [ ing-set       (apply set-union (food-ingredients obj))                            ]
         [ non-allergens (set-subtract ing-set (apply set-union (hash-values (food-allergens obj)))) ])
    (set-food-ing-set! obj ing-set)
    (set-food-inert!   obj non-allergens)
    obj))

(define (parse-line line obj)
  (let* ([ m (regexp-match #px"^([a-z ]+) \\(contains ([a-z, ]+)\\)$" line) ]
         [ ingredients (list->set (string-split (second m) " ")) ]
         [ allergens   (string-split (third m) ", ")             ]
         [ hsh         (food-allergens obj)                      ])
    (set-food-ingredients! obj (cons ingredients (food-ingredients obj)))
    (for ([ allergen (in-list allergens) ])
      (hash-set! hsh allergen (let ([ st (hash-ref hsh allergen #f) ])
                                (if st
                                    (set-intersect st ingredients)
                                    ingredients))))
    obj))

(define (part1 obj)
  (for/sum ([ non (in-set (food-inert obj)) ])
    (for/sum ([ ing (in-list (food-ingredients obj)) ])
      (if (set-member? ing non) 1 0))))

(define (part2 obj)
  (let* ([ hsh (food-allergens obj) ]
         [ lst (maximum-bipartite-matching
                (undirected-graph (for*/list ([ allergen (in-list (hash-keys hsh)) ]
                                              [ ing (in-set (hash-ref hsh allergen)) ])
                                    (list allergen ing)))) ])
    ;; maximum-bipartite-matching may have swapped the tuples!
    (let-values ([ (allergen ingredient) (if (set-member? (food-ing-set obj) (caar lst))
                                             (values second first)
                                             (values first second)) ])
      (~> (sort lst string<? #:key allergen)
          (map ingredient _)
          (string-join _ ",")))))

(module+ test (require rackunit)
  (check-equal? (part1 (parse-input "day21.txt")) 2428)
  (check-equal? (part2 (parse-input "day21.txt")) "bjq,jznhvh,klplr,dtvhzt,sbzd,tlgjzx,ctmbr,kqms"))
