#lang racket

;; Version inspired by a Ruby version I read.

(require threading)

(define (part1 hsh bag)
  (~> (hash->list hsh)
      (filter (λ (l) (member bag (cdr l))) _)
      (map (λ (l) (part1 hsh (car l))) _)
      (foldl (λ (l result) (set-union result l)) (set bag) _)))

(~> (file->lines "day07.txt")
    (map (curry regexp-match* #px"([a-z]+ [a-z]+)(?= bag)") _)
    (make-immutable-hasheq _)
    (part1 _ "shiny gold")
    (set-count _)
    (sub1 _))
