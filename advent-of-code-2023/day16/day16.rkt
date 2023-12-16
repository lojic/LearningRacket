#lang racket
(require "../advent.rkt")

(define-values (grid width height)
  (let ([ lines (parse-aoc 16 string->list) ])
    (values (grid->hash lines) (length (car lines)) (length lines))))

(define seen  (mutable-set))

(define (part2)
  (let ([ configs
          (append
           ;; top edge
           (for/list ([ col (in-range width) ])
             (cons (make-rectangular col 0) +i))
           ;; bottom edge
           (for/list ([ col (in-range width) ])
             (cons (make-rectangular col (sub1 height)) -i))
           ;; left edge
           (for/list ([ row (in-range height) ])
             (cons (make-rectangular 0 row) 1))
           ;; right edge
           (for/list ([ row (in-range height) ])
             (cons (make-rectangular (sub1 width) row) -1))) ])
    (list-max (for/list ([ config (in-list configs) ])
                (set-clear! seen)
                (part1 (car config) (cdr config))))))
  
(define (beam pos dir)
  (define (next pos dir) (beam (+ pos dir) dir))
  
  (let ([ key (cons pos dir) ])
    (when (not (set-member? seen key))
      (let ([ c (hash-ref grid pos #f) ])
        (when c
          (set-add! seen key)
          (match (cons c dir)
            [ (cons #\. _) (next pos dir)  ]
            [ (cons #\- 1)  (next pos dir) ]
            [ (cons #\- -1) (next pos dir) ]
            [ (cons #\| +i) (next pos dir) ]
            [ (cons #\| -i) (next pos dir) ]
            
            [ (cons #\\ 1)  (next pos +i) ]
            [ (cons #\/ -1) (next pos +i) ]

            [ (cons #\\ +i) (next pos 1)  ]
            [ (cons #\/ -i) (next pos 1)  ]

            [ (cons #\\ -1) (next pos -i) ]
            [ (cons #\/ 1)  (next pos -i) ]

            [ (cons #\\ -i) (next pos -1) ]
            [ (cons #\/ +i) (next pos -1) ]
            
            [ (cons #\- +i) (begin (next pos -1) (next pos 1)) ]
            [ (cons #\- -i) (begin (next pos -1) (next pos 1)) ]
            
            [ (cons #\| -1) (begin (next pos -i) (next pos +i)) ]
            [ (cons #\| 1)  (begin (next pos -i) (next pos +i)) ]))))))

(define (part1 pos dir)
  (beam pos dir)

  (~> (set->list seen)
      (map car _)
      (remove-duplicates _)
      length))


(check-equal? (part1 0 1) 7242)
(check-equal? (part2) 7572)
