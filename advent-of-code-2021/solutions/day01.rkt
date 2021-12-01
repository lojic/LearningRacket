#lang racket

(require "../advent/advent.rkt")

(define example '(199 200 208 210 200 207 240 269 260 263))
(define input (file->numbers "day01.txt"))

;; Return a count of the number of times a number in the list is
;; greater than the preceding number.
(define (count-increases lst)
  (car (foldl (match-lambda* [ (list n (cons count last)) (cons (+ count (if (> n last) 1 0)) n) ])
              (cons 0 (car lst))
              (cdr lst))))

;; Return a list of 3-element sliding windows from a list.
(define (windows n lst)
  (let ([ window (with-handlers ([ exn:fail:contract? (λ (_) #f) ])
                   (take lst n)) ])
    (if window
        (cons window (windows n (cdr lst)))
        '())))

(define (part1) (count-increases input))

(define (part2) (count-increases (map sum (windows 3 input))))

;; --------------------------------------------------------------------------------------------
;; Tests
;; --------------------------------------------------------------------------------------------

(module+ test
  (require rackunit)

  ;; Part 1
  (check-equal? (part1) 1616)

  ;; Part 2
  (check-equal? (part2) 1645)

  ;; count-increases
  (check-equal? (count-increases example) 7)

  ;; windows-3
  (check-equal? (windows 3 '(1 2 3 4 5 6))
                '((1 2 3) (2 3 4) (3 4 5) (4 5 6)))

  )
