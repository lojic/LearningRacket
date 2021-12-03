#lang racket

(require "../../advent/advent.rkt" threading)
(define input (file->lines "day03.txt"))
(define example '("00100" "11110" "10110" "10111" "10101" "01111"
                  "00111" "11100" "10000" "11001" "00010" "01010"))

;; Parse the input of '("001" "101" "010") into the form:
;; '((0 0 1) (1 0 1) (0 1 0))
(define (parse lst)
  (map (λ (s)
         (~> (string-split s "")
             (filter non-empty-string? _)
             (map string->number _)))
       lst))

;; Compute the column sums. For example:
;; '((0 0 1)
;;   (1 0 1)
;;   (0 1 0)) results in:
;;  '(1 1 2)
(define (compute-sums lst)
  (define (add2 lst1 lst2)
    (if (null? lst1)
        '()
        (cons (+ (car lst1) (car lst2))
              (add2 (cdr lst1) (cdr lst2)))))

  (let loop ([ lst (cdr lst) ][ sum (car lst) ])
    (if (null? lst)
        sum
        (loop (cdr lst) (add2 (car lst) sum)))))

;; Flip 1 to 0 or 0 to 1 in a list of boolean numbers
(define (flip lst)
  (if (null? lst)
      '()
      (cons (if (= (car lst) 1)
                0
                1)
            (flip (cdr lst)))))

;; Return a list of boolean numbers representing the most common
;; boolean number in each position of a list of boolean numbers. In
;; the case of a tie, use a default value which defaults to 1.
;;
;; For example:
;; '((0 0 1 0)
;;   (1 0 1 1)
;;   (0 1 0 0
;;   (0 1 0 0)) results in:
;;
;;  '(0 1 1 0)
(define (compute-common lst [ default 1 ])
  (define half (/ (length lst) 2))

  (map (λ (n)
         (cond [ (= n half) default ]
               [ (> n half) 1       ]
               [ else       0       ]))
       (compute-sums lst)))

;; Part 1 solution
(define (part1 input)
  (let* ([ common   (compute-common (parse input)) ]
         [ uncommon (flip common)                  ]
         [ gamma    (bool-list->decimal common)         ]
         [ epsilon  (bool-list->decimal uncommon)       ])
    (* gamma epsilon)))

;; Part 2 solution
(define (part2 input)
  (define (life input oxy)
    (define num-bits (length (car input)))

    (define (scrub lst oxy bit)
      (define common (compute-common lst))
      (filter (λ (l)
                (= (list-ref l bit)
                   (list-ref (if oxy common (flip common)) bit)))
              lst))

    (car (let loop ([ lst input ][ bit 0 ])
           (if (or (>= bit num-bits)
                   (< (length lst) 2))
               lst
               (let ([ result (scrub lst oxy bit) ])
                 (loop result (add1 bit)))))))

  (let ([ lst (parse input) ])
    (* (bool-list->decimal (life lst #t))
       (bool-list->decimal (life lst #f)))))

;; --------------------------------------------------------------------------------------------
;; Tests
;; --------------------------------------------------------------------------------------------

(module+ test
  (require rackunit)

  (check-equal? (part1 input) 852500)
  (check-equal? (part1 example) 198)
  (check-equal? (part2 example) 230)
  (check-equal? (part2 input) 1007985)

  ;; compute-sums -----------------------------------------------------------------------------

  (check-equal? (compute-sums '((0 0 1) (1 0 1) (0 1 0))) '(1 1 2))

  ;; flip -------------------------------------------------------------------------------------

  (check-equal? (flip '(1 0 1 1 0)) '(0 1 0 0 1))

  ;; parse ------------------------------------------------------------------------------------

  (check-equal? (parse '("001" "101" "010"))
                '((0 0 1) (1 0 1) (0 1 0)))

  )
