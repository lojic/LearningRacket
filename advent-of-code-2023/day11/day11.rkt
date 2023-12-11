#lang racket
(require "../advent.rkt")

(define-values (rows columns galaxies)
  (let* ([ empty?   (compose1 not false? (curry andmap (curry char=? #\.))) ]
         [ lines    (parse-aoc 11 string->list) ]
         [ rows     (~> (for/fold ([ lst '() ])
                                  ([ i    (in-range (length lines)) ]
                                   [ line (in-list lines)           ])
                          (if (empty? line)
                              (cons i lst)
                              lst))) ]
         [ columns  (~> (for/list ([ i (in-range (length (car lines))) ])
                          (empty? (map (curry (flip list-ref) i) lines)))
                        enumerate
                        (filter car _)
                        (map cdr _)) ]
         [ galaxies (map car
                         (for/fold ([ result '() ])
                                   ([ row (enumerate
                                           (map (curry filter (compose1 (curry char=? #\#) car))
                                                (map enumerate lines))) ])
                           (for/fold ([ result result ])
                                     ([ col (car row) ])
                             (let ([ pos (make-rectangular (cdr col) (cdr row)) ])
                               (cons (cons pos (car col)) result))))) ])

    (values rows columns galaxies)))

(define (distance pos1 pos2 inc)
  (define (expanded a b lst)
    (count (λ (n)
             (and (> n (min a b))
                  (< n (max a b))))
           lst))

  (let* ([ x1 (real-part pos1) ]
         [ y1 (imag-part pos1) ]
         [ x2 (real-part pos2) ]
         [ y2 (imag-part pos2) ])
    (+ (abs (- x1 x2))
       (abs (- y1 y2))
       (* (expanded y1 y2 rows)   (sub1 inc))
       (* (expanded x1 x2 columns) (sub1 inc)))))

(define (solve mult)
  (~> galaxies
      (combinations _ 2)
      (map (match-lambda [ (list gal1 gal2)
                           (distance gal1 gal2 mult) ]) _)
      list-sum))

;; Tests --------------------------------------------------------------------------------------

(check-equal? (solve 2) 9734203)
(check-equal? (solve 1000000) 568914596391)