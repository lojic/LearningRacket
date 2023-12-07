#lang racket
(require "../advent.rkt")

(define (parse-input part)
  (define (translate part2? card) ; Translate card symbol to numeric value
    (index-of (string->list (if part2? "_J23456789T_QKA" "__23456789TJQKA")) card))

  (define (parse-round part pair) ; -> (rank <list of 5 cards> bid)
    (let ([ lst (map (curry translate (eq? part part2)) (string->list (car pair))) ])
      (list (part lst) lst (string->number (cadr pair)))))

  (map (curry parse-round part) (parse-aoc 7 strings #:print-sample #f)))

(define (solve rounds)
  (~> (map (match-lambda [(list rank hand bid) (list (cons rank hand) bid)]) rounds)
      (sort _ < #:key (match-lambda [(list (list a b c d e f) _)
                                     (+ (* 537824 a) (* 38416 b) (* 2744 c) (* 196 d) (* 14 e) f)] ))
      (map second _)                       ; grab bid
      (enumerate _ 1)                      ; add rank
      (map (parallel-combine * car cdr) _) ; multiply rank * bid
      list-sum))                           ; sum all

(define (part1 cards) ; Compute the rank of a hand
  (match (sort (map length (group-by identity cards)) >)
         [ '(5)         7 ]   ; Five of a kind
         [ '(4 1)       6 ]   ; Four of a kind
         [ '(3 2)       5 ]   ; Full house
         [ '(3 1 1)     4 ]   ; Three of a kind
         [ '(2 2 1)     3 ]   ; Two pair
         [ '(2 1 1 1)   2 ]   ; One pair
         [ '(1 1 1 1 1) 1 ])) ; High card

(define (part2 cards) ; Replace J with every card, and choose the best
  (~> '(2 3 4 5 6 7 8 9 10 12 13 14)
      (map (compose1 part1 (curry list-replace cards 1)) _)
      (sort _ >)
      car))

(check-equal? (solve (parse-input part1)) 246409899)
(check-equal? (solve (parse-input part2)) 244848487)
