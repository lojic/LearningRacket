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
  (define (add-power xs)
    (for/sum ([ pair xs ][ slot (in-naturals 1) ])
      (* (add1 (hash-code (car pair))) slot (string->number (cdr pair)))))

  (define (collapse lst)
    (define (next-label label lst)
      (let ([ val (findf (λ (l) (string=? label (car l))) lst) ])
        (values val (remove val lst))))

    (if (null? lst)
        '()
        (match (car lst)
          [ (list _) (collapse (cdr lst)) ] ; A - with nothing to delete, ignore
          [ (list label focal)
            (let-values ([ (next tail) (next-label label (cdr lst)) ])
              (match next
                [ #f         (cons (cons label focal) (collapse tail)) ] ; No next label, continue
                [ (list _)   (collapse tail)                           ] ; Next is -, consume both elements
                [ (list _ _) (collapse (cons next tail))               ])) ]))) ; Next is =, replace focal length

  (~> (map (curry (flip string-split) #px"[=-]") input)
      (group-by (compose1 hash-code car) _)
      (map (compose1 add-power collapse) _)
      list-sum))

;; Tests --------------------------------------------------------------------------------------

(check-equal? (part1) 507291)
(check-equal? (part2) 296921)
