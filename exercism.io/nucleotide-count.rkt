#lang racket

(provide count histogram)

; List of valid nucleotides
(define nucleotides '(#\A #\C #\G #\T))

; Predicate to indicate if nucleotide is valid
(define (valid-nucleotide? n) (member n nucleotides))

; Predicate to indicate if strand is valid
(define (valid-nucleotides? strand)
  (andmap valid-nucleotide? (string->list strand)))

; Return the count of occurrences of nucleotide in strand
; Uses Racket's contract facility to raise exception on invalid args
(define/contract (count strand nucleotide)
  (-> valid-nucleotides? valid-nucleotide? any)
  (length (filter (λ (c) (char=? c nucleotide)) (string->list strand))))

; Return a hash of counts of occurrences of each nucleotide in strand
; Uses Racket's contract facility to raise exception on invalid args
(define/contract (histogram strand)
  (-> valid-nucleotides? any)
  (foldl (λ (n hsh) (hash-update hsh n add1 0))
         (make-immutable-hash (map (λ (x) (cons x 0)) nucleotides))
         (string->list strand)))
