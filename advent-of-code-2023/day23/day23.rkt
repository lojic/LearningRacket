#lang racket
(require "../advent.rkt" graph)

(define-values (grid start goal)
  (let* ([ lines  (parse-aoc 23 string->list) ]
         [ width  (length (car lines))        ]
         [ height (length lines)              ])
    (values (grid->hash lines #:col-filter (λ (c) (memv c '(#\. #\> #\< #\^ #\v))))
            1
            (+ (- width 2) (* (sub1 height) +i)))))

(define (create-graph make-graph start weight)
  (define (find-vertex v dir [ dist 1 ])
    (define (step v dir)
      (let ([ dir* (findf (λ (d)
                            (and (not (= d (- dir)))
                                 (hash-ref grid (+ v d) #f)))
                          '(1 +i -1 -i)) ])
        (values (+ v dir*) dir*)))
    
    (define (valid-exits pos)
      (filter identity (list (if (char=? #\> (hash-ref grid (+ pos 1) #\#))   1 #f)
                             (if (char=? #\v (hash-ref grid (+ pos +i) #\#)) +i #f)
                             (if (char=? #\< (hash-ref grid (+ pos -1) #\#)) -1 #f)
                             (if (char=? #\^ (hash-ref grid (+ pos -i) #\#)) -i #f))))
    
    (if (= v goal)
        (values v dist '())
        (let ([ dirs (valid-exits v) ])
          (if (> (length dirs) 1) 
              (values v dist dirs)
              (let-values ([ (v* dir*) (step v dir) ])
                (find-vertex v* dir* (add1 dist)))))))

  (let loop ([ queue (list (cons start +i)) ][ edges '() ][ weights '() ])
    (if (null? queue)
        (make-graph edges weights)
        (match-let ([ (cons v1 dir) (car queue) ])
          (let-values ([ (v2 dist dirs) (find-vertex (+ v1 dir) dir) ])
            (if (null? dirs)
                (loop (cdr queue) (cons (list v1 v2) edges) (cons (weight dist) weights))
                (loop (append (cdr queue)
                              (map (λ (dir) (cons v2 dir)) dirs))
                      (cons (list v1 v2) edges)
                      (cons (weight dist) weights))))))))

(define (dfs g v [ path '() ][ dist 0 ])
  (if (= v goal)
      dist
      (let ([ neighbors (filter (λ (v*)
                                  (not (memv v* path)))
                                (get-neighbors g v)) ])
        (if (null? neighbors)
            0
            (let loop ([ dist* 0 ][ neighbors neighbors ])
              (if (null? neighbors)
                  dist*
                  (let* ([ v*    (car neighbors)      ]
                         [ delta (edge-weight g v v*) ])
                    (loop (max dist (dfs g v* (cons v path) (+ dist delta)))
                          (cdr neighbors)))))))))
     
(define (part1)
  (let-values ([ (hsh _) (bellman-ford (create-graph directed-graph 1 -) 1) ])
    (- (list-min (hash-values hsh)))))

;; Tests --------------------------------------------------------------------------------------

(check-equal? (part1) 2238)
  
        
  
            
                
