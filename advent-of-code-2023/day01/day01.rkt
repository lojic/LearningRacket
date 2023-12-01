#lang racket

(require "../advent.rkt" threading)

(define in (parse-aoc 1))

(define digits '(("1" . 1) ("2" . 2) ("3" . 3) ("4" . 4) ("5" . 5)
                           ("6" . 6) ("7" . 7) ("8" . 8) ("9" . 9)))

(define words '(("one" . 1) ("two" . 2) ("three" . 3) ("four" . 4) ("five" . 5)
                            ("six" . 6) ("seven" . 7) ("eight" . 8) ("nine" . 9)))

;; Scan through a string, forward or backward, looking for the first
;; match of a prefix/suffix, and return its translation.
(define (find-digit s pairs fix? beg end)
  (let loop ([ p pairs ][ s s ])
    (if (null? p)
        (loop pairs (substring s (beg 0) (end (string-length s))))
        (let ([ pair (car p) ])
          (if (fix? s (car pair))
              (cdr pair)
              (loop (cdr p) s))))))

(define (find-first-digit s pairs)
  (find-digit s pairs string-prefix? add1 identity))

(define (find-last-digit s pairs)
  (find-digit s pairs string-suffix? identity sub1))

(define (solve pairs)
  (~> in
      (map (λ (s)
             (+ (* 10 (find-first-digit s pairs))
                (find-last-digit s pairs))) _)
      list-sum))

;; Parts --------------------------------------------------------------------------------------

(define (part1) (solve digits))

(define (part2) (solve (append digits words)))

;; Tests --------------------------------------------------------------------------------------

(check-equal? (part1) 52974)
(check-equal? (part2) 53340)

      
