#lang racket

(define players  '((1 0) (3 0)))
(define-values (position score) (values first second))

(define (solve [ players players ])
  (cond [ (winner players)  (* (score (loser players)) (dice-rolls)) ]
        [ else              (solve (turn players))                   ]))

(define (turn players)
  (let* ([ the-player (first players) ]
         [ pos (add1 (modulo (+ (sub1 (position the-player)) (roll-dice)) 10)) ])
    (list (cadr players) (list pos (+ (score the-player) pos)))))

(define ((fun pred?) lst) (findf (λ (p) (pred? 1000 (score p))) lst))
(define winner (fun <=))
(define loser  (fun >))

(define-values (roll-dice dice-rolls)
  (let ([ n 0 ][ num-rolls 0 ])
    (values (λ ()
              (let ([ sum (+ (* 3 n) 6) ]
                    [ n* (+ n 3) ])
                (set! num-rolls (+ num-rolls 3))
                (set! n (if (> n* 100) (- n* 100) n*))
                sum))
            (λ () num-rolls))))

;; Tests --------------------------------------------------------------------------------------

(module+ test
  (require rackunit)
  (check-equal? (solve) 897798))
