#lang racket

(require threading)

;; For Day 6, the goal was concision :)

(module+ main
  (~> (file->string "day06.txt")
      (string-split _ "\n\n")
      (map (λ (s)
             (~> (string-split s "\n")
                 (map (compose list->set string->list) _)
                 (apply set-intersect _)
                 (set-count _)))
           _)
      (apply + _)))
