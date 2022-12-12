#lang racket
(require "../advent.rkt")

(struct monkey (div t f op items) #:transparent)

(define monkeys (vector (monkey  5 1 6 (curry * 13) '(52 78 79 63 51 94))
                        (monkey  7 5 3 (curry + 3)  '(77 94 70 83 53))
                        (monkey 13 0 6 sqr          '(98 50 76))
                        (monkey 11 5 7 (curry + 5)  '(92 91 61 75 99 63 84 69))
                        (monkey  3 2 0 (curry + 7)  '(51 53 83 52))
                        (monkey  2 4 7 (curry + 4)  '(76 76))
                        (monkey 17 1 3 (curry * 19) '(75 59 93 69 76 96 65))
                        (monkey 19 2 4 (curry + 2)  '(89))))

(define N   (vector-length monkeys))
(define vec (make-vector N 0))

(define (solve calm rounds)
  (vector-fill! vec 0)
  (for ([ m-idx (in-range N) ])
    (let ([ monk (vector-ref monkeys m-idx) ])
      (for ([ item (in-list (monkey-items monk)) ])
        (do-rounds rounds monk m-idx item calm))))

  (~> (vector->list vec)
      (sort _ >)
      (take _ 2)
      (apply * _)))

(define (do-rounds rounds monk idx item calm)
  (when (> rounds 0)
    (let-values ([ (item* idx*) (do-round monk idx item calm) ])
      (do-rounds (sub1 rounds) (vector-ref monkeys idx*) idx* item* calm))))

(define (do-round monk idx item calm)
  (vector-set! vec idx (add1 (vector-ref vec idx)))
  (match-let ([ (monkey div t f op items) monk ])
    (let* ([ item* (calm (op item))                   ]
           [ idx*  (if (divisible-by? item* div) t f) ])
      (if (<= idx* idx)
          (values item* idx*)
          (let ([ monk* (vector-ref monkeys idx*) ])
            (do-round monk* idx* item* calm))))))

(define (divisible-by? n d) (= 0 (remainder n d)))
(define (part1 n)           (floor (/ n 3)))

(define part2
  (let ([ div (apply lcm (map monkey-div (vector->list monkeys))) ])
    (λ (n)
      (modulo n div))))

(solve part1 20)
(solve part2 10000)
