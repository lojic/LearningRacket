#lang racket
(require "../advent.rkt")

(struct brick (idx z-min z-max points) #:transparent)

(define (parse-brick idx lst)
  (match-let ([ (list x1 y1 z1 x2 y2 z2) lst ])
    (cond [ (not (= z1 z2)) ; Z-axis aligned
            (brick idx (min z1 z2) (max z1 z2) (list (make-rectangular x1 y1))) ] 
          [ (= x1 x2)       ; Y-axis aligned
            (brick idx z1 z1 (for/list ([ y (in-inclusive-range (min y1 y2) (max y1 y2)) ])
                               (make-rectangular x1 y))) ]
          [ else            ; X-axis aligned
            (brick idx z1 z1 (for/list ([ x (in-inclusive-range (min x1 x2) (max x1 x2)) ])
                               (make-rectangular x y1))) ])))
  
(define (drop-bricks bricks)
  (define (update-floors hsh b)
    (let ([ z-max (brick-z-max b) ])
      (for/fold ([ hsh hsh ])
                ([ point (in-list (brick-points b)) ])
        (hash-set hsh point z-max))))

  (let loop ([ bricks bricks ][ floors (hash) ][ result '() ])
    (if (null? bricks)
        (reverse result)
        (let* ([ b (car bricks)        ]
               [ z-min (brick-z-min b) ]
               [ z-max (brick-z-max b) ]
               [ z (for/fold ([ z 0 ])
                             ([ point (in-list (brick-points b)) ])
                     (max z (hash-ref floors point 0))) ]
               [ delta (sub1 (- z-min z)) ]
               [ b* (if (zero? delta)
                        b
                        (struct-copy brick b [ z-min (- z-min delta) ][ z-max (- z-max delta) ])) ])
          (loop (cdr bricks) (update-floors floors b*) (cons b* result))))))

(define (supported-by b bricks)
  (let ([ z-max+1 (add1 (brick-z-max b)) ]
        [ points  (brick-points b)       ])
    (filter (λ (b*)
              (and (= (brick-z-min b*) z-max+1)
                   (not (null? (set-intersect points (brick-points b*))))))
            bricks)))

(define (supporting b bricks)
  (let ([ z-min-1 (sub1 (brick-z-min b)) ]
        [ points  (brick-points b)       ])
    (filter (λ (b*)
              (and (= (brick-z-max b*) z-min-1)
                   (not (null? (set-intersect points (brick-points b*))))))
            bricks)))

(define bricks (drop-bricks (sort (for/list ([ lst (in-list (parse-aoc 22 numbers)) ]
                                             [ idx (in-naturals)                    ])
                                    (parse-brick idx lst))
                                  <
                                  #:key brick-z-min)))

(define (part1)
  (define (only-supported-by b bricks)
    (filter (λ (b*)
              (= 1 (length (supporting b* bricks))))
            (supported-by b bricks)))

  (length (filter (λ (b) (null? (only-supported-by b bricks))) bricks)))

(define (part2)
  (define cache (make-hash))
  
  (define (chain-reaction b bricks removed)
    (let ([ val (hash-ref cache (cons b removed) #f) ])
      (if val
          val
          (let ([ would-fall (filter (λ (b1)
                                       (andmap (λ (b2) (set-member? removed b2))
                                               (supporting b1 bricks)))
                                     (supported-by b bricks)) ])
            (cond [ (null? would-fall)
                    (hash-set! cache (cons b removed) removed)
                    removed ]
                  [ else
                    (let ([ result (for/fold ([ removed (set-union removed (list->set would-fall)) ])
                                             ([ b* (in-list would-fall) ])
                                     (set-union removed (chain-reaction b* bricks removed))) ])
                      (hash-set! cache (cons b removed) result)
                      result) ])))))
    
  (for/sum ([ b (in-list (sort bricks > #:key brick-z-min)) ])
    (let ([ n (sub1 (set-count (chain-reaction b bricks (set b)))) ])
      (printf "Brick: ~a = ~a\n" b n)
      n)))

;; Tests --------------------------------------------------------------------------------------

(check-equal? (part1) 482)
(check-equal? (part2) 103010)


          
  
                 
                   

