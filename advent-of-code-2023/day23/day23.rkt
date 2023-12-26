#lang racket
(require "../advent.rkt" graph)

(define-values (grid start goal)
  (let* ([ lines  (parse-aoc 23 string->list) ]
         [ width  (length (car lines))        ]
         [ height (length lines)              ])
    (values (grid->hash lines #:col-filter (λ (c) (memv c '(#\. #\> #\< #\^ #\v))))
            1
            (+ (- width 2) (* (sub1 height) +i)))))

(define (create-graph make-graph start weight valid-exits)
  (define (find-vertex v dir [ dist 1 ])
    (define (step v dir)
      (let ([ dir* (findf (λ (d)
                            (and (not (= d (- dir)))
                                 (hash-ref grid (+ v d) #f)))
                          '(1 +i -1 -i)) ])
        (values (+ v dir*) dir*)))

    (if (or (= v goal) (= v start))
        (values v dist '())
        (let ([ dirs (valid-exits v dir) ])
          (if (> (length dirs) 1)
              (values v dist dirs)
              (let-values ([ (v* dir*) (step v dir) ])
                (find-vertex v* dir* (add1 dist)))))))

  (let loop ([ queue (list (cons start +i)) ][ edges '() ][ weights '() ])
    (if (null? queue)
        (make-graph edges weights)
        (match-let ([ (cons v1 dir) (car queue) ])
          (let-values ([ (v2 dist dirs) (find-vertex (+ v1 dir) dir) ])
            (cond [ (findf (λ (lst)
                             (and (memv v1 lst) (memv v2 lst)))
                             edges)
                    (loop (cdr queue) edges weights) ]
                  [ (null? dirs)
                    (loop (cdr queue) (cons (list v1 v2) edges) (cons (weight dist) weights)) ]
                  [ else
                    (loop (append (cdr queue)
                                  (map (λ (dir) (cons v2 dir)) dirs))
                          (cons (list v1 v2) edges)
                          (cons (weight dist) weights)) ]))))))

(define (part1)
  (define (valid-exits pos _)
    (filter identity (list (if (char=? #\> (hash-ref grid (+ pos 1) #\#))   1 #f)
                           (if (char=? #\v (hash-ref grid (+ pos +i) #\#)) +i #f)
                           (if (char=? #\< (hash-ref grid (+ pos -1) #\#)) -1 #f)
                           (if (char=? #\^ (hash-ref grid (+ pos -i) #\#)) -i #f))))

  (let-values ([ (hsh _) (bellman-ford (create-graph directed-graph start - valid-exits) 1) ])
    (- (list-min (hash-values hsh)))))

(define (part2)
  (define (dfs g v [ path (set) ][ dist 0 ])
    (if (= v goal)
        dist
        (let ([ neighbors (filter (λ (v*)
                                    (not (set-member? path v*)))
                                  (get-neighbors g v)) ])
          (if (null? neighbors)
              0
              (list-max (for/list ([ neighbor (in-list neighbors) ])
                          (let ([ delta (edge-weight g v neighbor) ])
                            (dfs g neighbor (set-add path v) (+ dist delta)))))))))

  (define (valid-exits pos dir)
    (let ([ dirs (filter (λ (d)
                           (and (not (= d (- dir)))
                                (memv (hash-ref grid (+ pos d) #\#) '(#\. #\> #\< #\^ #\v))))
                         '(1 +i -1 -i)) ])
      dirs))

  (let ([ g (create-graph undirected-graph start identity valid-exits) ])
    (dfs g 1)))

;; Tests --------------------------------------------------------------------------------------

(check-equal? (time (part1)) 2238)
(check-equal? (time (part2)) 6398)
