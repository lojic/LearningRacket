#lang racket
(require "../advent.rkt")

(define-values (directions nodes)
  (let* ([ lines      (parse-aoc 8 atoms) ]
         [ directions (~> (first lines)     ; ("LRL")
                          first             ; "LRL"
                          (string->list _)  ; (#\L #\R #\L)
                          (map (λ (c)
                                 (match c
                                   [ #\L car ]
                                   [ #\R cdr ])) _)) ] ; (car cdr car)
         [ nodes      (for/hash ([ lst (in-list (drop lines 2)) ])
                        (match-let ([ (list key left right) lst ])
                          (values key (cons left right)))) ])
    (values directions nodes)))

(define (solve goal? key)
  (for/fold ([ key   key ]
             [ steps 0   ]
             #:result steps)
            ([ get (in-cycle directions) ])
    #:break (goal? key)
    (values (get (hash-ref nodes key)) (add1 steps))))

(define ends-with? (curry (flip string-suffix?)))

(define (part1)
  (solve (ends-with? "ZZZ") "AAA"))

(define (part2)
  (~> (hash-keys nodes)
      (filter (ends-with? "A") _)
      (map (curry solve (ends-with? "Z")) _)
      (apply lcm _)))

;; Tests --------------------------------------------------------------------------------------

(check-equal? (part1) 20777)
(check-equal? (part2) 13289612809129)
