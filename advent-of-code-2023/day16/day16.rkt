#lang racket
(require "../advent.rkt")

(define-values (grid width height)
  (let ([ lines (parse-aoc 16 string->list) ])
    (values (grid->hash lines) (length (car lines)) (length lines))))

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
                (part1 (car config) (cdr config))))))

(define (beam pos dir seen)
  (define (next pos dir seen) (beam (+ pos dir) dir seen))

  (let ([ key (cons pos dir) ])
    (if (set-member? seen key)
        seen
        (let ([ c (hash-ref grid pos #f) ])
          (if (not c)
              seen
              (let ([ seen (set-add seen key) ])
                (match (cons c dir)
                  [ (cons #\. _) (next pos dir seen)  ]
                  [ (cons #\- 1)  (next pos dir seen) ]
                  [ (cons #\- -1) (next pos dir seen) ]
                  [ (cons #\| +i) (next pos dir seen) ]
                  [ (cons #\| -i) (next pos dir seen) ]

                  [ (cons #\\ 1)  (next pos +i seen) ]
                  [ (cons #\/ -1) (next pos +i seen) ]

                  [ (cons #\\ +i) (next pos 1 seen) ]
                  [ (cons #\/ -i) (next pos 1 seen) ]

                  [ (cons #\\ -1) (next pos -i seen) ]
                  [ (cons #\/ 1)  (next pos -i seen) ]

                  [ (cons #\\ -i) (next pos -1 seen) ]
                  [ (cons #\/ +i) (next pos -1 seen) ]

                  [ (cons #\- +i) (next pos -1 (next pos 1 seen)) ]
                  [ (cons #\- -i) (next pos -1 (next pos 1 seen)) ]

                  [ (cons #\| -1) (next pos -i (next pos +i seen)) ]
                  [ (cons #\| 1)  (next pos -i (next pos +i seen)) ])))))))

(define (part1 pos dir)


  (~> (beam pos dir (set))
      (set->list _)
      (map car _)
      (remove-duplicates _)
      length))

(check-equal? (part1 0 1) 7242)
(check-equal? (part2) 7572)
