#lang racket
(require "../advent.rkt")

(define-values (seeds1 seeds2 categories)
  (let* ([ lines (parse-aoc 5 #:sep "\n\n" #:print-sample #f) ]
         [ seeds (cdr (atoms (car lines)))                    ])
    (values seeds
            (map (λ (pair)
                   (match-let ([ (list beg len) pair ])
                     (cons beg (+ beg len)))) (chunk 2 seeds))
            (map (λ (l)
                   (map (λ (l)
                          (match-let ([ (list dst src len) l ])
                            (list (- dst src) (cons src (+ src len))))) (chunk 3 (drop (atoms l) 2))))
                 (cdr lines)))))

(define (convert-map seed cat)
  (define (increment-range delta pair)
    (cons (+ delta (car pair)) (+ delta (cdr pair))))

  (match-let ([ (cons seed-beg seed-end)           seed ]
              [ (list delta (cons cat-beg cat-end)) cat  ])
    (if (< seed-beg cat-beg)
        (cond [ (<= seed-end cat-beg)
                (values #f (list seed)) ]
              [ (> seed-end cat-end)
                (values (increment-range delta (cons cat-beg cat-end))
                        (list (cons seed-beg cat-beg)
                              (cons cat-end seed-end))) ]
              [ else
                (values (increment-range delta (cons cat-beg seed-end))
                        (list (cons seed-beg cat-beg))) ])
        (cond [ (>= seed-beg cat-end)
                (values #f (list seed)) ]
              [ (<= seed-end cat-end)
                (values (increment-range delta seed) '()) ]
              [ else
                (values (increment-range delta (cons seed-beg cat-end))
                        (list (cons cat-end seed-end))) ]))))

(define (convert-category seed category)
  (if (null? category)
      (list seed)
      (let-values ([ (seed remaining) (convert-map seed (car category)) ])
        (let ([ lst (apply append (map (λ (s) (convert-category s (cdr category))) remaining)) ])
          (if seed
              (cons seed lst)
              lst)))))

(define (convert-categories seed categories)
  (if (null? categories)
      (list seed)
      (apply append (map (λ (s)
                           (convert-categories s (cdr categories)))
                         (convert-category seed (car categories))))))

(define (part1 seeds) (part2 (map (λ (s) (cons s (add1 s))) seeds)))

(define (part2 seeds)
  (list-min (map car (apply append (map (λ (s) (convert-categories s categories)) seeds)))))

(check-equal? (part1 seeds1) 457535844)
(check-equal? (part2 seeds2) 41222968)
