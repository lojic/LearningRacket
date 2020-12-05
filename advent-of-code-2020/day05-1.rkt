#lang racket

(provide get-seat input num-rows num-cols)
(define input (file->lines "day05.txt"))
(define num-rows 128)
(define num-cols 8)

(define (get-seat rows cols str)
  (let loop ([rl 0][rh rows][cl 0][ch cols][letters (string->list str)])
    (if (null? letters)
        (+ (* rl num-cols) cl)
        (let ([ mid-r (/ (+ rl rh) 2) ]
              [ mid-c (/ (+ cl ch) 2) ])
          (match (car letters)
            [ #\F (loop rl mid-r cl ch (cdr letters)) ]
            [ #\B (loop mid-r rh cl ch (cdr letters)) ]
            [ #\L (loop rl rh cl mid-c (cdr letters)) ]
            [ #\R (loop rl rh mid-c ch (cdr letters)) ])))))

(define (highest-seat-id seats)
  (foldl (λ (seat highest)
           (max highest (get-seat num-rows num-cols seat)))
           0 seats))

(module+ main
  (printf "Highest seat id in input is ~a\n" (highest-seat-id input)))

(module+ test
  (require rackunit)

  (check-equal? (get-seat num-rows num-cols "FBFBBFFRLR") 357)
  (check-equal? (get-seat num-rows num-cols "BFFFBBFRRR") 567)
  (check-equal? (get-seat num-rows num-cols "FFFBBBFRRR") 119)
  (check-equal? (get-seat num-rows num-cols "BBFFBBFRLL") 820)

  (check-equal? (highest-seat-id '("FBFBBFFRLR" "BFFFBBFRRR" "FFFBBBFRRR" "BBFFBBFRLL"))
                820)

  )
