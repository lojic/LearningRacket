#lang racket

;; Tests for advent.rkt

(require "./advent.rkt")

(module+ test
  (require rackunit)

  ;; ascending-permutations-generator ---------------------------------------------------------

  (let ([ g (ascending-permutations-generator 3 '(1 2 3 4 5)) ])
    (for ([ lst (in-list '((1 2 3) (1 2 4) (1 2 5) (1 3 4) (1 3 5)
                           (1 4 5) (2 3 4) (2 3 5) (2 4 5) (3 4 5))) ])
      (check-equal? (g) lst)))

  ;; bool-list->decimal ----------------------------------------------------------------

  (for ([ pair (in-list '(((1 0 1 1) 11)
                          ((0 0 0) 0)
                          ((0 0 1) 1)
                          ((0 1 0) 2)
                          ((0 1 1) 3)
                          ((1 0 0) 4)
                          ((1 1 1) 7))) ])
    (check-equal? (bool-list->decimal (first pair)) (second pair)))

  ;; bool-string-list->decimal ----------------------------------------------------------------

  (check-equal? (bool-string-list->decimal '("1" "0" "1" "1")) 11)

  ;; chunk ------------------------------------------------------------------------------------

  (check-equal? (chunk (range 15) 5)
                '((0 1 2 3 4)
                  (5 6 7 8 9)
                  (10 11 12 13 14)))

  ;; csv-file->numbers ------------------------------------------------------------------------

  (let ([ path (path->string (make-temporary-file)) ])
    (dynamic-wind (λ ()
                    ;; Create the cvs line
                    (with-output-to-file path
                      (λ ()
                        (printf "1,6,43,8,0,21,50\n"))
                      #:exists 'replace))
                  (λ ()
                    (check-equal? (csv-file->numbers path)
                                  '(1 6 43 8 0 21 50)))
                  (λ ()
                    (delete-file path))))

  ;; filter-ascending-permutations ------------------------------------------------------------

  (let ([ sum-is-even? (λ (lst) (even? (foldl + 0 lst))) ])
    (check-equal? (filter-ascending-permutations sum-is-even? 3 '(1 2 3 4 5))
                  '((1 2 3) (1 2 5) (1 3 4) (1 4 5) (2 3 5) (3 4 5))))

  ;; iterate
  
  (let ([ fun (λ (n) (+ n 3)) ])
    (check-equal? (iterate fun 7 4) 19))

  (let ([ fun (λ (s)
                (cond [ (symbol? s) (symbol->string s) ]
                      [ (string? s) (string->symbol s) ]
                      [ else        (error "Invalid")  ])) ])
    (check-equal? (iterate fun 'foo 0) 'foo)
    (check-equal? (iterate fun 'foo 1) "foo")
    (check-equal? (iterate fun 'foo 2) 'foo)
    (check-equal? (iterate fun 'foo 3) "foo"))

  ;; list-max

  (check-equal? (list-max '(3 8 4 9 0 -3)) 9)
  (check-equal? (list-max '(-3 -2 -9)) -2)

  ;; list-min
  
  (check-equal? (list-min '(3 8 4 9 0)) 0)
  (check-equal? (list-min '(3 8 4 9 0 -3)) -3)
  (check-equal? (list-min '(-3 -2 -9)) -9)

  ;; list-prod

  (check-equal? (list-prod '(2 7 4 13)) 728)

  ;; list-sum
  
  (check-equal? (list-sum '(2 7 4 13)) 26)

  ;; point-add

  (check-equal? (point-add (point 1 2 3)
                           (point 2 3 4))
                (point 3 5 7))
  
  ;; point-sub

  (check-equal? (point-sub (point 1 2 3)
                           (point 2 3 4))
                (point -1 -1 -1))

  ;; vector-sum

  (check-equal? (vector-sum #(2 7 4 13)) 26)

  ;; vector-update!

  (let ([ vec (vector 1 2 3 4 5) ])
    (vector-update! vec 2 add1)
    (check-equal? vec #(1 2 4 4 5)))

  ;; zipn

  (check-equal? (zipn '(1 2 3 4 5) '(2 3 4 5) '(3 4 5))
                '((1 2 3) (2 3 4) (3 4 5)))

  ;; pair-stream ------------------------------------------------------------------------------

  (check-equal?
   (for/sum ([ (a b) (in-stream (pair-stream '((1 . 2) (2 . 3) (3 . 4) (4 . 5)))) ])
     (* a b))
   40)
  
  )
