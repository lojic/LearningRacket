#lang racket
(require "../advent.rkt")

(define (translate part2? card)
  (match card
    [ #\A 14 ]
    [ #\K 13 ]
    [ #\Q 12 ]
    [ #\J (if part2? 1 11) ]
    [ #\T 10 ]
    [ _ (- (char->integer card) 48) ]))

(define (rank-hand hand)
  (match (map length hand)
    [ (list 5)         7 ]   ; Five of a kind
    [ (list 4 1)       6 ]   ; Four of a kind
    [ (list 3 2)       5 ]   ; Full house
    [ (list 3 1 1)     4 ]   ; Three of a kind
    [ (list 2 2 1)     3 ]   ; Two pair
    [ (list 2 1 1 1)   2 ]   ; One pair
    [ (list 1 1 1 1 1) 1 ])) ; High card

(define (parse-input part)
  (define (parse-round part pair)
    (let ([ lst (map (curry translate (eq? part part2)) (string->list (car pair))) ])
      (list (part lst) lst (string->number (cadr pair)))))

  (map (curry parse-round part) (parse-aoc 7 strings #:print-sample #f)))

(define (solve rounds)
  (~> (map (match-lambda [(list groups hand bid) (list (cons (rank-hand groups) hand) bid)]) rounds)
      (sort _ < #:key (match-lambda [(list (list a b c d e f) _)
                                     (+ (* 537824 a) (* 38416 b) (* 2744 c) (* 196 d) (* 14 e) f)] ))
      (map second _)
      (enumerate _ 1)
      (map (parallel-combine * car cdr) _)
      list-sum))

(define (part1 lst) (sort (group-by identity lst) > #:key length))

(define (part2 lst)
  (~> '(2 3 4 5 6 7 8 9 10 12 13 14)
      (map (compose1 part1 (curry list-replace lst 1)) _)
      (sort _ > #:key rank-hand)
      car))

(check-equal? (solve (parse-input part1)) 246409899)
(check-equal? (solve (parse-input part2)) 244848487)
