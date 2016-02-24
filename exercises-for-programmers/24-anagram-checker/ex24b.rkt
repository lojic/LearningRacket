#lang racket
(require "../lojic.rkt")

(define (sort-string str)
  (list->string (sort (string->list str) char<?)))

; The two words are anagrams if, and only if, they are equal when sorted.
(define (is-anagram? word1 word2)
  (string=? (sort-string word1) (sort-string word2)))