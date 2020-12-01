#lang racket

;; Advent of Code 2020 Day 1 part 2:
;; Find three numbers in the input that sum to 2020, then multiply
;; them to obtain the puzzle answer.
;;
;; This solution is quite a bit more general than what is required. I
;; had fun coming up with the filter-ascending-permutations function,
;; and I think I may use it in the future.

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
;; Returns a procedure that accepts a list of numbers and indicates
;; whether they sum to the specified value.
(define (sums-to n) (λ (lst) (= n (apply + lst))))

;; (filter-ascending-permutations pred? n lst) -> list?
;; pred?  : procedure?
;; n      : exact-nonnegative-integer?
;; lst    : (listof number?)
;;
;; Return a list of permutations that satisfy the specified
;; predicate. For example, the list '(foo bar foo baz) produces the
;; following list of 2-tuple ascending permutations:
;; '((foo bar) (foo foo) (foo baz) (bar foo) (bar baz) (foo baz))
(define (filter-ascending-permutations pred? n lst)
  (reverse
   (let loop ([ lst lst ][ n n ][ stack '() ][ result '() ])
     (if (= n 0)
         (let ([ s (reverse stack) ])
           (if (pred? s) (cons s result) result))
         (if (null? lst)
             result
             (loop (cdr lst)
                   n
                   stack
                   (loop (cdr lst) (sub1 n) (cons (car lst) stack) result)))))))

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
  (check-equal? (filter-ascending-permutations identity 2 '(foo bar foo baz))
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
