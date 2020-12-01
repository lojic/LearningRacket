#lang racket

;; Advent of Code 2020 Day 1 part 2:
;; Find three numbers in the input that sum to 2020, then multiply
;; them to obtain the puzzle answer.

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

;; (find-n pred? n lst result) -> (or/c list? #f)
;; pred?  : procedure?
;; n      : exact-nonnegative-integer?
;; lst    : (listof number?)
;; result : (listof number?)
;;
;; Searches for n distinct numbers in a list that satisfy the
;; specified predicate. If found, returns (list n1 n2 ... nn);
;; otherwise, returns #f
(define (find-n pred? n lst [ result '() ])
  (if (= n 0)
      (if (pred? result)
          (reverse result)
          #f)
      (if (null? lst)
          #f
          (let* ([ num (car lst) ]
                 [ ans (find-n pred? (sub1 n) (cdr lst) (cons num result)) ])
            (if ans
                ans
                (find-n pred? n (cdr lst) result))))))

(define (find-3 pred? lst)
  (find-n pred? 3 lst))

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
  ;; find-n
  ;; ------------------------------------------------------------------------------------------
  (check-equal? (find-n (λ (lst) (andmap even? lst)) 3 '(1 2 3 4 5 6)) '(2 4 6))
  (check-equal? (find-n (sums-to 17) 2 '(1 2 3 4 5 6 7 8 9 10)) '(7 10))
  
  )
