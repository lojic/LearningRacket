#lang racket

;; Modified to use for/sum after seeing another participants use of it.

(require threading)

(module+ main
  (for/sum ([ group (in-list (string-split (file->string "day06.txt") "\n\n")) ])
    (~> (string-replace group "\n" "")
        (string->list _)
        ((compose length remove-duplicates) _))))
