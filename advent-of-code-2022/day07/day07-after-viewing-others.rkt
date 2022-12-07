#lang racket
(require "../advent.rkt")

(define in   (parse-aoc 7 atoms))
(define dirs (hash))
(define seen (hash))

(define (process dirs seen cwd in)
  (define (add-file dirs size cwd)
    (let ([ dirs (hash-set dirs cwd (+ size (hash-ref dirs cwd 0))) ])
      (if (null? cwd)
          dirs
          (add-file dirs size (cdr cwd)))))

  (if (null? in)
      dirs
      (let ([ rem (cdr in) ])
        (match (car in)
          [ (list "$" _)         (process dirs seen cwd rem) ]

          [ (list "dir" _)       (process dirs seen cwd rem) ]

          [ (list "$" "cd" "..") (process dirs seen (drop cwd 1) rem) ]

          [ (list "$" "cd" dir)  (if (string=? "/" dir)
                                     (process dirs seen '() rem)
                                     (process dirs seen (cons dir cwd) rem)) ]

          [ (list size name)     (let ([ key (cons name cwd) ])
                                   (if (hash-has-key? seen key)
                                       (process dirs seen cwd rem)
                                       (process (add-file dirs size cwd) (hash-set seen key #t) cwd rem))) ]))))

(let* ([ dirs  (process dirs seen #f in)          ]
       [ sizes (~> (hash->list dirs) (map cdr _)) ]
       [ root  (hash-ref dirs '() )               ])
  (values (~> sizes
              (filter (curry > 100001) _)
              list-sum) ; Part 1
          (~> (sort sizes <)
              (findf (curry < (+ -40000000 root)) _)))) ; Part 2
