#lang racket
(require "../lojic.rkt")

; The exercise constraint said not to use built-in functionality, so this code is an implementation
; of sort.
(define (merge x y)
  (cond [(null? x) y]
        [(null? y) x]
        [else (let [(hx (car x)) (tx (cdr x))
                    (hy (car y)) (ty (cdr y))]
                (if (char<? hx hy)
                  (cons hx (merge tx y))
                  (cons hy (merge ty x))))]))

(define (msort l)
  (if (or (null? l) (null? (cdr l)))
      l
      (let [(half (quotient (length l) 2))]
            (merge (msort (take l half)) 
                   (msort (drop l half))))))

(define (sort-string str)
  (list->string (msort (string->list str))))
; -------------------------------------------------------------------------------------------------

; The two words are anagrams if, and only if, they are equal when sorted.
(define (is-anagram? word1 word2)
  (string=? (sort-string word1) (sort-string word2)))