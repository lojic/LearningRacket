#lang racket
(require "../advent.rkt")

(define in   (parse-aoc 7 atoms))
(define dirs (hash))
(define seen (hash))

(define (add-file dirs size cwd)
  (let ([ dirs (hash-set dirs cwd (+ size (hash-ref dirs cwd 0))) ])
    (if (null? cwd)
        dirs
        (add-file dirs size (cdr cwd)))))

(define (process dirs seen cwd in)
  (if (null? in)
      dirs
      (let ([ lst (car in) ]
            [ rem (cdr in) ])
        (match lst
          [ (list "$" _)         (process dirs seen cwd rem)          ]
          [ (list "dir" _)       (process dirs seen cwd rem)          ]
          [ (list "$" "cd" "..") (process dirs seen (drop cwd 1) rem) ]
          [ (list "$" "cd" dir)  (if (string=? "/" dir)
                                     (process dirs seen '() rem)
                                     (process dirs seen (cons dir cwd) rem)) ]
          [ (list size name)     (let ([ key (cons name cwd) ])
                                   (if (hash-has-key? seen key)
                                       (process dirs seen cwd rem)
                                       (process (add-file dirs size cwd) (hash-set seen key #t) cwd rem))) ]))))

(let* ([ dirs  (process dirs seen #f in)          ]
       [ sizes (~> (hash->list dirs) (map cdr _)) ])
  (values (list-sum (filter (curry > 100001) sizes))                            ; Part 1
          (findf (curry < (+ -40000000 (hash-ref dirs '() ))) (sort sizes <)))) ; Part 2
