#lang racket

;; I had to cheat for Day 24! This is simply a port of a concise
;; Python version I found that was posted on reddit w/o a github
;; link. The solution takes 8 microseconds :)

(require threading)

(define instructions (~> (file->lines "day24.txt")
                         (list->vector)))

(define (solve)
  (define (helper i n)
    (~> (vector-ref instructions (+ (* 18 i) n))
        string-split
        third
        string->number))
    
  (let loop ([ i 0 ][ stack '() ][ p 99999999999999 ][ q 11111111111111 ])
    (cond [ (>= i 14) (values p q) ]
          [ else (let ([ a (helper i 5) ][ b (helper i 15) ])
                   (if (> a 0)
                       (loop (add1 i) (cons (cons i b) stack) p q)
                       (match-let ([ (cons j b) (car stack) ])
                         (loop (add1 i)
                               (cdr stack)
                               (- p (abs (* (+ a b) (expt 10 (- 13 (if (> a (- b)) j i))))))
                               (+ q (abs (* (+ a b) (expt 10 (- 13 (if (< a (- b)) j i)))))))))) ])))

;; Tests --------------------------------------------------------------------------------------

(module+ test
  (require rackunit)
  (let-values ([ (largest smallest) (time (solve)) ])
    (check-equal? largest 89959794919939)
    (check-equal? smallest 17115131916112)))
