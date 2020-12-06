#lang racket

;; For Day 6, the goal was concision :)

(require threading)

(module+ main
  (~> (file->string "day06.txt")
      (string-split _ "\n\n")
      (map (λ (s) (string-replace s "\n" "")) _)
      (map string->list _)
      (map (compose length remove-duplicates) _)
      (apply + _)))
