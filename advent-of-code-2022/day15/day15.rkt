#lang racket
(require "../advent.rkt")

(define in (parse-aoc 15 numbers #:print-sample #f))

(define-syntax-rule (distance x1 y1 x2 y2) (+ (abs (- x1 x2)) (abs (- y1 y2))))

(define (part1 in row)
  (let* ([ bounds  (bounds-for-row in row) ]
         [ beacons (count (λ (beacon)
                            (ormap (λ (pair)
                                     (<= (car pair) beacon (cdr pair)))
                                   bounds))
                          (beacons-in-row in row)) ])

    (- (for/sum ([ pair bounds ])
         (- (cdr pair) (car pair) -1))
       beacons)))

(define (part2 in limit [ y 0 ])
  (let* ([ bounds (map (λ (pair)
                         (clip limit pair))
                       (bounds-for-row in y)) ]
         [ n (for/sum ([ pair bounds ])
               (- (cdr pair) (car pair) -1)) ])
    (if (<= n limit)
        (let* ([ pair  (car bounds) ]
               [ left  (car pair)   ]
               [ right (cdr pair)   ]
               [ x     (if (= left 0) (add1 right) (sub1 left)) ])
          (+ (* x limit) y))
        (part2 in limit (add1 y)))))

(define (bounds-for-row in row)
  (for/fold ([ bounds '() ])
            ([ lst (in-list in) ])
    (match-let* ([ (list sx sy bx by) lst ]
                 [ delta (abs (- sy row)) ]
                 [ r     (- (distance sx sy bx by)
                            delta) ]
                 [ left  (- sx r)  ]
                 [ right (+ sx r)  ])
      (if (<= left right)
          (extend-bounds bounds left right)
          bounds))))

(define (extend-bounds bounds left right)
  (if (null? bounds)
      (cons (cons left right) bounds)
      (match-let* ([ pair                (car bounds) ]
                   [ (cons left* right*) pair         ])
        (if (intersect? left right left* right*)
            (extend-bounds (cdr bounds)
                           (min left left*)
                           (max right right*))
            (cons pair
                  (extend-bounds (cdr bounds) left right))))))

(define (intersect? l r l* r*) (not (or (> l* r) (< r* l))))

(define (beacons-in-row in row)
  (for/fold ([ beacons '() ])
            ([ tuple (in-list in) ] )
    (match-let ([ (list _ _ bx by) tuple ])
      (if (and (= by row)
               (not (memv bx beacons)))
          (cons bx beacons)
          beacons))))

(define (clip limit pair)
  (let ([ left  (car pair) ]
        [ right (cdr pair) ])
    (if (< left 0)
        (if (> right limit)
            (cons 0 limit)
            (cons 0 right))
        (if (> right limit)
            (cons left limit)
            pair))))

(time (check-equal? (part1 in 2000000) 5181556))
(time (check-equal? (part2 in 4000000) 12817603219131))
