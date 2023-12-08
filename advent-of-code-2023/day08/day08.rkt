#lang racket
(require "../advent.rkt")

(define-values (dirs nodes)
  (let ([ lines (parse-aoc 8 atoms) ])
    (values (map (λ (c)
                   (if (char=? #\L c) car cdr))
                 (string->list (caar lines)))
            (make-immutable-hash (map (match-lambda [ (list key left right)
                                                      (cons key (cons left right)) ])
                                      (cddr lines))))))

(define (part1 suffix key)
  (let loop ([ dirs dirs ][ key key ][ steps 1 ])
    (let ([ val ((car dirs) (hash-ref nodes key)) ])
      (cond [ (string-ends-with? val suffix) steps ]
            [ else (loop (rotate-list dirs) val (add1 steps)) ]))))

(define (part2 suffix)
  (apply lcm (map (curry part1 suffix)
                  (filter (λ (key)
                            (string-ends-with? key "A"))
                          (hash-keys nodes)))))

;; Tests --------------------------------------------------------------------------------------

(check-equal? (part1 "ZZZ" "AAA") 20777)
(check-equal? (part2 "Z") 13289612809129)
