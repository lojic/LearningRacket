#lang racket

(require "./advent.rkt")

;; Advent of Code 2020 Day 1:
;; Find two numbers in the input that sum to 2020, then multiply them
;; to obtain the puzzle answer.

(define expense-sum       2020)
(define expenses-filename "day1.txt")

;; (get-expenses fname) -> (listof number?)
;; fname : string?
;;
;; Returns a list of numbers obtained from reading a file containing
;; one number per line.
(define (get-expenses fname)
  (with-input-from-file fname
    (thunk
     (for/list ([ l (in-lines) ])
       (string->number l)))))

;; (sums-to n) -> boolean?
;; n : number?
;;
;; Returns a procedure that accepts two numbers and indicates whether
;; they sum to the specified value.
(define (sums-to n) (λ (n1 n2) (= n (+ n1 n2))))

(module+ main
  (let ([ pair (find-2 (sums-to expense-sum)
                       (get-expenses expenses-filename)) ])
    (if pair
        (let ([ n1 (car pair)  ]
              [ n2 (cdr pair) ])
          (printf "Answer is ~a * ~a = ~a\n" n1 n2 (* n1 n2)))
        (printf "Unable to find 2 numbers that sum to ~a\n" expense-sum))))

(module+ test
  (require rackunit)
  
  ;; ------------------------------------------------------------------------------------------
  ;; get-expenses
  ;; ------------------------------------------------------------------------------------------
  (let ([ l (get-expenses expenses-filename) ])
    (check-equal? (first l)  1652)
    (check-equal? (second l) 1998)
    (check-equal? (last l)   1861))
  
  ;; ------------------------------------------------------------------------------------------
  ;; sums-to
  ;; ------------------------------------------------------------------------------------------
  (check-not-false ((sums-to 7) 3 4))
  (check-false ((sums-to 7) 1 2))
  
  ;; ------------------------------------------------------------------------------------------
  ;; find-2
  ;; ------------------------------------------------------------------------------------------
  (check-false  (find-2 (sums-to 0) '()))
  (check-false  (find-2 (sums-to 0) '(0)))
  (check-false  (find-2 (sums-to 0) '(1 2 3)))
  
  (check-equal? (find-2 (sums-to 3) '(1 2 3)) (cons 1 2))
  ;; Use a list with the first element is half the sum to ensure we
  ;; don't erroneously sum the same element.
  (check-equal? (find-2 (sums-to 4) '(2 1 7 4 0)) (cons 4 0))
  (check-equal? (find-2 (sums-to 4) '(3 -1 2 5)) (cons -1 5))
  
  )
