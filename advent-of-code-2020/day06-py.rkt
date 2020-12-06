#lang racket

;; Alternate version inspired by a Python solution. I really like the
;; symmetry of using set-union and set-intersect for parts 1 & 2 used
;; in the Python solution.

;; In Python set('abc') produces { 'b', 'a', 'c' }
(define string->set (compose list->set string->list))

(module+ main
  (let ([ groups (for/list ([ line (string-split (file->string "day06.txt") "\n\n") ])
                   (for/list ([ g (string-split line "\n") ])
                     (string->set g))) ])
    (println (for/sum ([ ps groups ])              ;; Part 1
               (set-count (apply set-union ps))))
    (println (for/sum ([ ps groups ])              ;; Part 2
               (set-count (apply set-intersect ps))))))
