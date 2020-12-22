#lang racket

(require threading)

(define (parse-input fname)
  (define (parse-player s) (map string->number (cdr (string-split s "\n"))))
  (map parse-player (string-split (file->string fname) "\n\n")))

(define (part1 players)
  (let loop ([ d1 (car players) ][ d2 (cadr players) ])
    (cond [ (null? d1) (score d2) ]
          [ (null? d2) (score d1) ]
          [ else (let ([ c1 (car d1) ][ c2 (car d2) ])
                   (if (> c1 c2)
                       (loop (append (cdr d1) (list c1 c2)) (cdr d2))
                       (loop (cdr d1) (append (cdr d2) (list c2 c1))))) ])))

(define (part2 players) (score (cdr (combat (car players) (cadr players)))))
(define (infinite? d1 rounds1 d2 rounds2) (or (member d1 rounds1) (member d2 rounds2)))
(define (score lst) (for/sum ([ i (in-naturals 1) ][ c (in-list (reverse lst)) ]) (* c i)))
  
(define (combat deck1 deck2)
  (let loop ([ d1 deck1 ][ r1 '() ][ d2 deck2 ][ r2 '() ])
    (cond [ (null? d1)                              (cons 'p2 d2) ]
          [ (or (null? d2) (infinite? d1 r1 d2 r2)) (cons 'p1 d1) ]
          [ else (let* ([ c1 (car d1) ]
                        [ c2 (car d2) ]
                        [ enough? (and (>= (length (cdr d1)) c1) (>= (length (cdr d2)) c2)) ])
                   (if (or (and enough? (eq? 'p1 (car (combat (take (cdr d1) c1) (take (cdr d2) c2)))))
                           (and (not enough?) (> c1 c2)))
                       (loop (append (cdr d1) (list c1 c2)) (cons d1 r1) (cdr d2) (cons d2 r2))
                       (loop (cdr d1) (cons d1 r1) (append (cdr d2) (list c2 c1)) (cons d2 r2)))) ])))

(module+ test (require rackunit)
  (check-equal? (part1 (parse-input "day22.txt")) 33010)
  (check-equal? (part2 (parse-input "day22.txt")) 32769))
