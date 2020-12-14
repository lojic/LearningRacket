#lang racket

(require math/number-theory)
(require threading)

(define input (file->lines "day13.txt"))
(define ids   (map string->number (string-split (second input) ",")))

;; Part 1
(match-let* ([ etd (string->number (first input)) ]
             [ (cons id wait) (~> (filter identity ids)
                                  (map (λ (id) (cons id (- id (modulo etd id)))) _)
                                  (sort _ < #:key cdr)
                                  (first _)) ])
  (* id wait))

;; Part 2 (failed attempt)
;; This was a brute force attempt, but it's simply not fast enough -
;; even with sorting the list to use the largest id to increment
;; faster. It took 7 seconds to find a solution using the first seven
;; numbers, but adding the last two numbers was too much.
;;
;; I had no idea about the Chinese Remainder theorem, and my modular
;; arithmetic chops weren't up to the task of solving this with no
;; prior knowledge.
#|
(define (valid? t offset lst)
  (andmap (λ (pair)
            (match-let ([ (cons id off) pair ])
              (= 0 (modulo (+ t (- off offset)) id))))
          lst))

(match-let* ([ pairs (~> (for/list ([i  (in-naturals)][id (in-list ids)]) (cons id i))
                         (filter car _)
                         (sort _ >= #:key car)) ]
             [ (cons (cons id offset) rest) pairs ])
  (printf "id=~a, offset=~a, rest=~a\n" id offset rest)
  (let loop ([ t 0 ])
    (if (valid? t offset rest)
        (- t offset)
        (loop (+ t id)))))
|#

;; Part 2 - using Chinese Remainder theorem after reading a spoiler
(let ([ pairs (filter car (for/list ([ i (in-naturals) ]
                                     [ id (in-list ids)])
                            (cons id (and id (- id i))))) ])
  (solve-chinese (map cdr pairs) (map car pairs)))
