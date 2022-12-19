#lang racket
(require "../advent.rkt")
(require graph)

(define MIN        3)
(define ROOT       (cons "AA" 0))
(define valve-rate cdr)

(define-values (closed shortest g)
  (let* ([ in       (~> (for/list ([ lst (in-list (parse-aoc 16 atoms #:print-sample #f)) ])
                          (match-let ([ (list _ name _ _ _ rate _ _ _ _ valves ...) lst ])
                            (list name rate valves)))) ]
         [ valves   (~> (for/list ([ lst in ])
                          (cons (car lst) (cadr lst)))
                        make-immutable-hash) ]
         [ g        (~> (apply append (for/list ([ tuple in ])
                                        (for/list ([ dst (caddr tuple) ])
                                          (let* ([ src      (car tuple)           ]
                                                 [ src-rate (hash-ref valves src) ]
                                                 [ dst-rate (hash-ref valves dst) ])
                                            (list (cons src src-rate) (cons dst dst-rate))))))
                        directed-graph) ]
         [ nonzero  (~> (filter (λ (tuple)
                                  (or (string=? (car ROOT) (car tuple))
                                      (> (cadr tuple) 0)))
                                in)
                        (map (λ (tuple)
                               (cons (cons (car tuple) (cadr tuple)) #t))
                             _)
                        make-immutable-hash) ]
         [ closed   (hash-remove nonzero ROOT) ]
         [ shortest (~> (floyd-warshall g)
                        hash->list
                        (filter (λ (pair)
                                  (match-let ([ (cons (list src dst) dist) pair ])
                                    (and (hash-ref nonzero src #f)
                                         (hash-ref nonzero dst #f)
                                         (not (equal? src dst)))))
                                _)
                        make-immutable-hash) ])
    (values closed shortest g)))

(define (part1 v minutes pressure closed)
  (if (< minutes (add1 MIN))
      pressure
      (for/fold ([ best-pressure pressure ])
                ([ destination (get-destinations v closed minutes) ])
        (match-let* ([ (cons dst dist) destination             ]
                     [ minutes         (- minutes (add1 dist)) ]
                     [ pressure* (part1 dst
                                        minutes
                                        (+ pressure (* (valve-rate dst) minutes))
                                        (hash-remove closed dst)) ])
          (if (> pressure* best-pressure)
              pressure*
              best-pressure)))))

(define (part2 v minutes closed)
  (let* ([ all-pairs (~> (visited v closed minutes)
                         (map (λ (path)
                                (cons (sum-path path minutes) (list->set (map car path))))
                              _)
                         (sort _ > #:key car)) ]
         [ top-disjoint (match-let ([ (cons _ path) (car all-pairs) ])
                          (let loop ([ paths (cdr all-pairs) ][ result (list (car all-pairs)) ])
                            (if (null? paths)
                                result
                                (match-let ([ (cons _ path*) (car paths) ])
                                  (if (set-empty? (set-intersect path path*))
                                      (reverse (cons (car paths) result))
                                      (loop (cdr paths) (cons (car paths) result))))))) ])

    (let me ([ pairs top-disjoint ][ best 0 ])
      (if (null? pairs)
          best
          (match-let ([ (cons pressure path) (car pairs) ])
            (let elephant ([ paths* (cdr pairs) ])
              (if (null? paths*)
                  (me (cdr pairs) best)
                  (match-let ([ (cons pressure* path*) (car paths*) ])
                    (if (set-empty? (set-intersect path path*))
                        (let ([ sum (+ pressure pressure*) ])
                          (if (> sum best)
                              (me (cdr pairs) sum)
                              (elephant (cdr paths*))))
                        (elephant (cdr paths*)))))))))))

(define (get-destinations v closed minutes)
  (for/fold ([ result '() ])
            ([ dst (hash-keys closed) ])
    (let ([ dist (hash-ref shortest (list v dst) 0.0) ])
      (if (< 0 (+ dist MIN) minutes)
          (cons (cons dst dist) result)
          result))))

(define (visited v closed minutes)
  (if (or (< minutes 3)
          (hash-empty? closed))
      '()
      (for/fold ([ result '() ])
                ([ dest (in-list (get-destinations v closed minutes)) ])
        (match-let* ([ (cons dst dist) dest                         ]
                     [ closed (hash-remove closed dst)              ]
                     [ lst    (visited dst closed (- minutes dist)) ])
          (append (list (list dest))
                  (map (λ (path) (cons dest path))
                       lst)
                  result)))))

(define (sum-path path minutes [sum 0])
  (if (null? path)
      sum
      (match-let* ([ (cons v dist) (car path)              ]
                   [ *minutes      (- minutes (add1 dist)) ])
        (sum-path (cdr path)
                  *minutes
                  (+ sum (* (valve-rate v) *minutes))))))

(time (check-equal? (part1 ROOT 30 0 closed) 1767.0)) ; Part 1

(time (check-equal? (part2 ROOT 26 closed) 2528.0))   ; Part 2
