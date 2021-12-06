#lang racket

(require threading)
(define pattern #px"^(\\d+),(\\d+)\\s+->\\s+(\\d+),(\\d+)$")

(define (make-step p) (/ p (max (abs (real-part p)) (abs (imag-part p)))))

(define input (map (λ (s)
                     (let* ([ lst (map string->number (drop (regexp-match pattern s) 1)) ]
                            [ p1 (+ (first lst) (* (second lst) +i)) ]
                            [ p2 (+ (third lst) (* (fourth lst) +i)) ])
                       (list p1 p2 (make-step (- p2 p1)))))
                   (file->lines "day05.txt")))

(define (points line)
  (match-let ([ (list p1 p2 step) line ])
    (define (helper p1 p2) (if (= p1 p2) (list p2) (cons p1 (helper (+ p1 step) p2))))
    (helper p1 p2)))

(define (part2 lines)
  (~>> (foldl (λ (line hsh) (foldl (λ (point hsh) (hash-update hsh point add1 0)) hsh (points line))) (hash) lines)
       hash-values (filter (curryr >= 2)) length))

(define (part1 input)
  (part2 (filter (match-lambda
                   [ (list _ _ step) (or (zero? (real-part step))
                                         (zero? (imag-part step))) ]) input)))
