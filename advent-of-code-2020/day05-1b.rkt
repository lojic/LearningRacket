#lang racket

;; Using a binary encoding after seeing that in someone else's solution!

(define input (file->lines "day05.txt"))

(define (encode-binary str)
  (for/sum ([ c str ][ i '(512 256 128 64 32 16 8 4 2 1) ])
    (if (or (char=? c #\B) (char=? c #\R)) i 0)))

(define (highest-seat-id seats)
  (foldl (λ (seat highest)
           (max highest (encode-binary seat)))
           0 seats))

(module+ main
  (printf "Highest seat id in input is ~a\n" (highest-seat-id input)))

(module+ test
  (require rackunit)

  (check-equal? (encode-binary "FBFBBFFRLR") 357)
  (check-equal? (encode-binary "BFFFBBFRRR") 567)
  (check-equal? (encode-binary "FFFBBBFRRR") 119)
  (check-equal? (encode-binary "BBFFBBFRLL") 820)

  (check-equal? (encode-binary "FFFFFFFLLL") 0)
  (check-equal? (encode-binary "FFFFFFFLLR") 1)
  (check-equal? (encode-binary "FFFFFFFLRL") 2)
  (check-equal? (encode-binary "BBBBBBBRRR") 1023)

  (check-equal? (highest-seat-id '("FBFBBFFRLR" "BFFFBBFRRR" "FFFBBBFRRR" "BBFFBBFRLL"))
                820)

  )
