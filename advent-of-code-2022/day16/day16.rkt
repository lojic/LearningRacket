#lang racket
(require "../advent.rkt")
(require graph)

(define-values (valves closed shortest g MAX)
  (let* ([ in       (~> (for/list ([ lst (in-list (parse-aoc 16 atoms #:print-sample #f)) ])
                          (match-let ([ (list _ name _ _ _ rate _ _ _ _ valves ...) lst ])
                            (list name rate valves)))) ]
         [ g        (~> (apply append (for/list ([ tuple in ])
                                        (for/list ([ dst (caddr tuple) ])
                                          (list (car tuple) dst))))
                        directed-graph) ]
         [ valves   (for/list ([ lst in ])
                      (cons (car lst) (cadr lst))) ]
         [ closed   (~> (filter (λ (pair) (> (cdr pair) 0)) valves)
                        make-immutable-hash) ]
         [ shortest (floyd-warshall g) ])
    (values (make-immutable-hash valves) closed shortest g 30)))

(define (get-destinations name closed minutes)
  (for/fold ([ result '() ])
            ([ dst (hash-keys closed) ])
    (let ([ dist (hash-ref shortest (list name dst) 0.0) ])
      (if (<= 1 dist minutes)
          (cons (cons dst dist) result)
          result))))

(define (depth-first-search name [ pressure 0 ][ closed closed ][ minutes MAX ])
  (if (< minutes 1)
      pressure
      (for/fold ([ best-pressure pressure ])
                ([ destination (get-destinations name closed minutes) ])
        (match-let* ([ (cons dst dist) destination             ]
                     [ minutes         (- minutes (add1 dist)) ])
          (let ([ pressure* (depth-first-search dst
                                                (+ pressure (* (hash-ref valves dst) minutes))
                                                (hash-remove closed dst)
                                                minutes) ])
            (if (> pressure* best-pressure)
                pressure*
                best-pressure))))))

(time (check-equal? (depth-first-search "AA") 1767.0)) ; Part 1
