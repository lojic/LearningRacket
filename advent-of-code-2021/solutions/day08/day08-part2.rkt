#lang racket

(require threading)

(define input (~>> (file->lines "day08.txt")
                  (map (compose (curry map string-split)
                                (curryr string-split " | ")))))

(define (solve input)
  (for/sum ([ lst (in-list input) ])
    (entry (first lst) (second lst))))

(define (entry lst digits)
  (let* ([ sets (make-sets lst) ]
         [ d1 (find sets (by-count 2)) ]
         [ d7 (find sets (by-count 3)) ]
         [ d4 (find sets (by-count 4)) ]
         [ d8 (find sets (by-count 7)) ]
         [ d6 (find sets (by-subtraction d1 6 5)) ]
         [ d2 (find sets (by-subtraction d4 5 3)) ]
         [ d9 (find sets (by-subtraction (set-union d4 d7) 6 1)) ]
         [ d0 (find sets (by-exclusion (list d6 d9) 6)) ]
         [ d3 (find (remove d2 sets) (by-subtraction d7 5 2)) ]
         [ d5 (find (remove d2 sets) (by-subtraction d7 5 3)) ]
         [ hsh (hash d0 0 d1 1 d2 2 d3 3 d4 4 d5 5 d6 6 d7 7 d8 8 d9 9) ]
         [ digit-sets (make-sets digits) ])
    (display-value hsh digit-sets)))

(define make-sets (curry map (compose list->set string->list)))

(define (find lst pred?)
  (findf pred? lst))

(define (by-count n)
  (λ (s)
    (= n (set-count s))))

(define (by-exclusion exclude len)
  (λ (s)
    (and (= len (set-count s))
         (not (member s exclude)))))

(define (by-subtraction sub len cnt)
  (λ (s)
    (and (= len (set-count s))
         (= cnt (set-count (set-subtract s sub))))))

(define (display-value hsh digits)
  (let ([ get (λ (f) (hash-ref hsh (f digits))) ])
    (+ (* 1000 (get first))
       (* 100  (get second))
       (* 10   (get third))
               (get fourth))))

(module+ test
  (require rackunit)
  (check-equal? (solve input) 1027483))
