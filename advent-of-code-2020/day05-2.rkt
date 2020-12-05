#lang racket

(require "./day05-1.rkt")

(define (missing-seat ids)
  (let loop ([ last-id (car ids) ][ ids (cdr ids) ])
    (if (null? ids)
        #f
        (let ([ id (car ids) ][ next (add1 last-id) ])
          (if (= id next)
              (loop id (cdr ids))
              next)))))

(module+ main
  #;(printf "Missing seat id in input is ~a\n"
          (missing-seat (sort (map (λ (s) (get-seat num-rows num-cols s)) input) <)))
  (for ([ id (in-list (sort (map (λ (s) (get-seat num-rows num-cols s)) input) <)) ])
    (printf "ID = ~a\n" id))

  )


(module+ test
  (require rackunit)

  (check-equal? (get-seat num-rows num-cols "FBFBBFFRLR") 357)
  (check-equal? (get-seat num-rows num-cols "BFFFBBFRRR") 567)
  (check-equal? (get-seat num-rows num-cols "FFFBBBFRRR") 119)
  (check-equal? (get-seat num-rows num-cols "BBFFBBFRLL") 820)

  (check-false (missing-seat '(1 2 3 4)))

  (check-equal? (missing-seat '(1 2 4 5)) 3)

  )
