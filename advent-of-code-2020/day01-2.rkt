#lang racket

(require "./advent.rkt")

;; Advent of Code 2020 Day 1 part 2:
;; Find three numbers in the input that sum to 2020, then multiply
;; them to obtain the puzzle answer.
;;
;; This solution is quite a bit more general than what is required. I
;; had fun coming up with the filter-ascending-permutations function,
;; and I think I may use it in the future.

(define expense-sum       2020)
(define expenses-filename "day01.txt")

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
;; Returns a procedure that accepts a list of numbers and indicates
;; whether they sum to the specified value.
(define (sums-to n) (λ (lst) (= n (apply + lst))))

(define (find-3 pred? lst)
  (let ([ tuple (filter-ascending-permutations pred? 3 lst) ])
    (if (null? tuple)
        #f
        (first tuple))))

(module+ main
  (let ([ numbers (find-3 (sums-to expense-sum)
                          (get-expenses expenses-filename)) ])
    (if numbers
        (let ([ n1 (first numbers)  ]
              [ n2 (second numbers) ]
              [ n3 (third numbers)  ])
          (printf "Answer is (* ~a ~a ~a) = ~a\n" n1 n2 n3 (* n1 n2 n3)))
        (printf "Unable to find 3 numbers that sum to ~a\n" expense-sum))))

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
  (check-not-false ((sums-to 9) '(2 3 4)))
  (check-false     ((sums-to 7) '(1 2 3)))
  
  ;; ------------------------------------------------------------------------------------------
  ;; find-3
  ;; ------------------------------------------------------------------------------------------
  (check-false  (find-3 (sums-to 0) '()))
  (check-false  (find-3 (sums-to 0) '(0)))
  (check-false  (find-3 (sums-to 0) '(1 2 3)))
  
  (check-equal? (find-3 (sums-to 9) '(2 1 3 4)) '(2 3 4))
  (check-equal? (find-3 (sums-to 4) '(2 1 7 4 0 1)) '(2 1 1))
  
  ;; ------------------------------------------------------------------------------------------
  ;; filter-ascending-permutations
  ;; ------------------------------------------------------------------------------------------
  (check-equal? (filter-ascending-permutations (const #t) 2 '(foo bar foo baz))
                '((foo bar) (foo foo) (foo baz) (bar foo) (bar baz) (foo baz)))
  
  (check-equal? (filter-ascending-permutations (sums-to 7) 2 '(1 2 3 4))
                '((3 4)))
  
  ;; ------------------------------------------------------------------------------------------
  ;; Solution
  ;; ------------------------------------------------------------------------------------------
  (check-equal? (apply * (find-3 (sums-to expense-sum)
                                 (get-expenses expenses-filename)))
                178724430)
  
  )
