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
      (define (dir-ok? d) (and (not (= d (- dir)))
                               (hash-ref grid (+ v d) #f)))
      (let ([ dir* (findf dir-ok? '(1 +i -1 -i)) ])
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
                  [ else (loop (append (cdr queue)
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
  (define (valid-exits pos dir)
    (filter (λ (d)
              (and (not (= d (- dir)))
                   (memv (hash-ref grid (+ pos d) #\#) '(#\> #\v #\< #\^))))
            '(1 +i -1 -i)))

  (define (create-edges g n v-index edges)
    (let ([ vec (make-vector (* n n) 0) ])
      (for ([ lst (in-list edges) ])
        (match-let ([ (list v1 v2) lst ])
          (let ([ i1 (hash-ref v-index v1) ]
                [ i2 (hash-ref v-index v2) ]
                [ wt (edge-weight g v1 v2) ])
            (vector-set! vec (+ i1 (* i2 n)) wt)
            (vector-set! vec (+ i2 (* i1 n)) wt))))
      vec))

  (define (dfs get-neighbors v goal get-weight visited [ dist 0 ])
    (if (= v goal)
        dist
        (let loop ([ neighbors (get-neighbors v) ][ dist* 0 ])
          (if (null? neighbors)
              dist*
              (let ([ neighbor (car neighbors) ])
                (if (vector-ref visited neighbor)
                    (loop (cdr neighbors) dist*)
                    (let ([ delta (get-weight v neighbor) ])
                      (vector-set! visited v #t)
                      (let ([ val (dfs get-neighbors neighbor goal get-weight visited (+ delta dist)) ])
                        (vector-set! visited v #f)
                        (loop (cdr neighbors) (max dist* val))))))))))

  (let* ([ g         (create-graph undirected-graph start identity valid-exits) ]
         [ vertices  (get-vertices g)                                           ]
         [ v-index   (make-immutable-hash (enumerate vertices))                 ]
         [ neighbors (list->vector (for/list ([ v (in-list vertices) ])
                                     (for/list ([ v* (in-neighbors g v) ])
                                       (hash-ref v-index v*)))) ]
         [ n             (hash-count v-index)                           ]
         [ edges         (create-edges g n v-index (get-edges g))       ]
         [ get-neighbors (λ (v) (vector-ref neighbors v))               ]
         [ get-weight    (λ (v1 v2) (vector-ref edges (+ v1 (* n v2)))) ])
    (dfs get-neighbors (hash-ref v-index start) (hash-ref v-index goal) get-weight (make-vector n #f))))

;; Tests --------------------------------------------------------------------------------------

(check-equal? (time (part1)) 2238)
(check-equal? (time (part2)) 6398)
