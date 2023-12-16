#lang racket
(require "../advent.rkt")

(define input (string-split (car (parse-aoc 15)) ","))

(define (hash-code s)
  (foldl (λ (c v) (remainder (* (+ v (char->integer c)) 17) 256))
         0
         (string->list s)))

(define (part1)
  (list-sum (map hash-code input)))

(define (part2)
  (define (sum-box xs)
    (for/sum ([ pair xs ][ slot (in-naturals 1) ])
      (* (add1 (hash-code (car pair))) slot (string->number (cdr pair)))))

  (define (simplify lst)
    (cond [ (null? lst) '() ]
          [ else (match (car lst)
                   [ (list _)  (simplify (cdr lst)) ] ; A '-' with nothing to delete, ignore
                   [ (list label focal)
                     (let* ([ next (findf (λ (l) (string=? label (car l))) (cdr lst)) ]
                            [ tail (remove next (cdr lst))                            ])
                       (match next
                         [ #f         (cons (cons label focal) (simplify tail)) ] ; No next label, continue
                         [ (list _)   (simplify tail)                           ] ; Next is '-', consume both elements
                         [ (list _ _) (simplify (cons next tail))               ])) ]) ])) ; Next is '=', replace focal length

  (~> (map (curry (flip string-split) #px"[=-]") input)
      (group-by (compose1 hash-code car) _)
      (map (compose1 sum-box simplify) _)
      list-sum))

;; Tests --------------------------------------------------------------------------------------

(check-equal? (part1) 507291)
(check-equal? (part2) 296921)
