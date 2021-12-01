#lang racket

(require "../advent/advent.rkt")

(define example '(199 200 208 210 200 207 240 269 260 263))
(define input (file->numbers "day01.txt"))

;; Return a count of the number of times a number in the list is
;; greater than the preceding number.
(define (count-increases lst)
  (let loop ([ lst (cdr lst) ][ last (car lst) ][ count 0 ])
    (if (null? lst)
        count
        (let ([ n (car lst) ])
          (loop (cdr lst)
                n
                (if (> n last)
                    (add1 count)
                    count))))))

;; Return the sum of the first n numbers in a list. If there are less
;; than n numbers in the list, return #f
(define (sum-n n lst)
  (let loop ([ lst lst ][ n n ][ sum 0 ])
    (cond [ (= n 0) sum ]
          [ (null? lst) #f ]
          [ else
            (loop (cdr lst) (sub1 n) (+ sum (car lst))) ])))

;; Return a list of the n-element sliding window sums of the input
;; list.
(define (window-sums n lst)
  (let ([ sum (sum-n n lst) ])
    (if sum
        (cons sum (window-sums n (cdr lst)))
        '())))

(module+ test
  (require rackunit)

  ;; Part 1
  (check-equal? (count-increases input) 1616)
  
  ;; Part 2
  (check-equal? (count-increases (window-sums 3 input)) 1645)


  ;; count-increases
  (check-equal? (count-increases example) 7)

  ;; sum-n
  (check-false (sum-n 4 '(1 2 3) ))
  (check-equal? (sum-n 3 '(1 2 3 4 5 6)) 6)
  
  ;; window-sums
  (check-equal? (window-sums 3 example)
                '(607 618 618 617 647 716 769 792))


  )
