#lang racket

;; A functional version emphasizing readability with small, useful functions.

(require "../../advent/advent.rkt" threading)

(define input   (file->lines "day04.txt"))
(define dim     5)

;; Puzzle parts
(define (part1 lines) (solve first lines))
(define (part2 lines) (solve last  lines))

;; --------------------------------------------------------------------------------------------

;; Return a list of board columns
(define (cols board)
  (for/list ([ col (in-range dim) ])
    (for/list ([ row (in-range dim) ])
      (get board row col))))

;; Return the value at board[row, col]
(define (get board row col)
  (vector-ref board (+ (* row dim) col)))

;; Indicate whether the board is a winning board
(define (is-winning? board)
  (ormap (curry andmap false?) (append (rows board) (cols board))))

;; Functionally mark the board by replacing any values equal to n with #f
(define (mark-board n board)
  (vector-map (λ (i) (if (equal? i n) #f i)) board))

;; Return a new list of boards that have been marked
(define (mark-boards n boards)
  (map (curry mark-board n) boards))

;; Return a list of boards from the input
(define (parse-boards lines)
  (define (parse-board lst)
    (~>> (string-join lst " ")
         string-split
         (map string->number)
         list->vector))

  (~>> (chunk (rest lines) (add1 dim))
       (map (curryr drop 1))
       (map parse-board)))

;; Return a list of random numbers from the input
(define (parse-random-numbers lines)
  (~> (first lines)
      (string-split ",")
      (map string->number _)))

;; Return a list of rows from the board
(define (rows board)
  (for/list ([ row (in-range dim) ])
    (for/list ([ col (in-range dim) ])
      (get board row col))))

;; Associate numbers with a list of boards the number caused to
;; win. Then simply choose either the first board from the first
;; number, or the last board from the last number according to the
;; part via the accessor function which is either first or last.
(define (solve accessor lines)
  (match-let ([ (list n boards)
                (accessor (winning-numbers (parse-random-numbers lines)
                                           (parse-boards lines))) ])
    (* n (sum-unmarked (accessor boards)))))

;; Return the sum of all unmarked positions on the board
(define (sum-unmarked board)
  (for/sum ([ i (in-range (* dim dim)) ])
    (or (vector-ref board i) 0)))

;; Return an associaton list of the form:
;; ( (<m> (<winning-board-m-1> <winning-board-m-2> ...))
;;   (<n> (<winning-board-n-1> <winning-board-n-2> ...)) )
(define (winning-numbers numbers boards)
  (if (null? numbers)
      '()
      (let*-values ([ (n)      (first numbers)                ]
                    [ (boards) (mark-boards n boards)         ]
                    [ (yes no) (partition is-winning? boards) ])
        (cond [ (null? yes) (winning-numbers (rest numbers) no)                     ]
              [ else        (cons (list n yes) (winning-numbers (rest numbers) no)) ]))))

;; Tests
(module+ test
  (require rackunit)
  (check-equal? (part1 input) 8442)
  (check-equal? (part2 input) 4590))
