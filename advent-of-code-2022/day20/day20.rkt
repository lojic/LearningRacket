#lang racket
(require "../advent.rkt")
(require "../circular-queue.rkt")

(define in (file->list "./day20.txt"))

(define (solve n in)
  (let* ([ q      (list->queue in)   ]
         [ len    (length in)        ]
         [ digits (get-references q) ])
    (for ([ _ (in-range n) ])
      (for ([ digit (in-list digits) ])
        (mix! digit len)))

    (for/sum ([ n '(1000 2000 3000) ])
      (queue-val (iterate queue-next (queue-memv 0 q) n)))))

(define (get-references q)
  (let loop ([ current (queue-next q) ][ result (list q) ])
    (if (queue-eq? current q)
        (reverse result)
        (loop (queue-next current) (cons current result)))))

(define (mix! digit len)
  (let ([ val (modulo (queue-val digit) (sub1 len)) ])
    (when (not (zero? val))
      (let ([ pos (cond [ (positive? val) (iterate queue-next digit val) ]
                        [ else (iterate queue-previous digit (add1 (abs val))) ]) ])
        (queue-remove-n! digit 1)
        (queue-insert-queue! pos digit)))))

(time (check-equal? (solve 1 in) 4151))
(time (check-equal? (solve 10 (map (λ (n) (* n 811589153)) in)) 7848878698663))
