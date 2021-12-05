#lang racket

;; A functional version emphasizing readability with small functions.

(require "../../advent/advent.rkt" threading)

(define input   (file->lines "day04.txt"))
(define dim     5)

(define (part1 lines) (solve first lines))
(define (part2 lines) (solve last  lines))

(define (cols board)
  (for/list ([ col (in-range dim) ])
    (for/list ([ row (in-range dim) ])
      (get board row col))))

(define (get board row col)    (vector-ref board (+ (* row dim) col)))
(define (is-winning? board)    (ormap (curry andmap false?) (append (rows board) (cols board))))
(define (mark-board n board)   (vector-map (λ (i) (if (equal? i n) #f i)) board))
(define (mark-boards n boards) (map (curry mark-board n) boards))
(define (parse-numbers lines)  (map string->number (string-split (first lines) ",")))

(define (parse-boards lines)
  (define (parse-board lst)
    (~>> (string-join lst " ")
         string-split
         (map string->number)
         list->vector))

  (~>> (chunk (rest lines) (add1 dim))
       (map (curryr drop 1))
       (map parse-board)))

(define (rows board)
  (for/list ([ row (in-range dim) ])
    (for/list ([ col (in-range dim) ])
      (get board row col))))

(define (solve accessor lines)
  (match-let ([ (list n boards)
                (accessor (winning-numbers (parse-numbers lines)
                                           (parse-boards lines))) ])
    (* n (sum-unmarked (accessor boards)))))

(define (sum-unmarked board)
  (for/sum ([ i (in-range (* dim dim)) ])
    (or (vector-ref board i) 0)))

(define (winning-numbers numbers boards)
  (if (null? numbers)
      '()
      (let*-values ([ (n)      (first numbers)                ]
                    [ (boards) (mark-boards n boards)         ]
                    [ (yes no) (partition is-winning? boards) ])
        (cond [ (null? yes) (winning-numbers (rest numbers) no)                     ]
              [ else        (cons (list n yes) (winning-numbers (rest numbers) no)) ]))))

(printf "Part 1 = ~a, Part 2 = ~a\n" (part1 input) (part2 input))
