#lang racket

;; Modified to use for/sum after seeing another participants use of it.

(require threading)

(module+ main
  (for/sum ([ group (in-list (string-split (file->string "day06.txt") "\n\n")) ])
    (~> (string-split group "\n")
        (map (compose list->set string->list) _)
        (apply set-intersect _)
        (set-count _))))
