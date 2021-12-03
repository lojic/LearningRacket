#lang racket

(require threading)
(define input (file->lines "day03.txt"))

(define (parse lst)
  (map (λ (s)
         (~> (string-split s "")
             (filter non-empty-string? _)
             (map string->number _))) lst))

(define (compute-sums lst)
  (define (add2 lst1 lst2)
    (if (null? lst1)
        '()
        (cons (+ (car lst1) (car lst2)) (add2 (cdr lst1) (cdr lst2)))))

  (let loop ([ lst (cdr lst) ][ sum (car lst) ])
    (if (null? lst)
        sum
        (loop (cdr lst) (add2 (car lst) sum)))))

(define (list->decimal lst)
  (let loop ([lst lst] [acc 0])
    (match lst [ '()        acc                              ]
               [ (cons 0 _) (loop (cdr lst) (* 2 acc))       ]
               [ (cons 1 _) (loop (cdr lst) (+ (* 2 acc) 1)) ]
               [ _          0                                ])))

(define (flip lst)
  (if (null? lst)
      '()
      (cons (if (= (car lst) 1) 0 1) (flip (cdr lst)))))

(define (compute-common lst [ default 1 ])
  (define half (/ (length lst) 2))

  (map (λ (n)
         (cond [ (= n half) default ]
               [ (> n half) 1       ]
               [ else       0       ]))
       (compute-sums lst)))

(define (part1 input)
  (let* ([ common   (compute-common (parse input)) ]
         [ uncommon (flip common)                  ]
         [ gamma    (list->decimal common)         ]
         [ epsilon  (list->decimal uncommon)       ])
    (* gamma epsilon)))

(define (part2 input)
  (define (life input oxy)
    (define num-bits (length (car input)))

    (define (scrub lst oxy bit)
      (define common (compute-common lst))
      (filter (λ (l)
                (= (list-ref l bit)
                   (list-ref (if oxy common (flip common)) bit))) lst))

    (car (let loop ([ lst input ][ bit 0 ])
           (if (or (>= bit num-bits) (< (length lst) 2))
               lst
               (loop (scrub lst oxy bit) (add1 bit))))))

  (let ([ lst (parse input) ])
    (* (list->decimal (life lst #t)) (list->decimal (life lst #f)))))
