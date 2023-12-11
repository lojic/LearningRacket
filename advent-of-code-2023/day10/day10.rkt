#lang racket
(require "../advent.rkt")

(define (parse-input)
  (define (cell->neighbors pos c)
    (define (inc a b) (list (+ pos a) (+ pos b)))

    (match c
      [ #\| (inc -i +i) ] ; north and south
      [ #\- (inc 1  -1) ] ; east and west
      [ #\L (inc -i 1)  ] ; north and east
      [ #\J (inc -i -1) ] ; north and west
      [ #\7 (inc +i -1) ] ; south and west
      [ #\F (inc +i 1)  ] ; south and east
      [ _   c           ]))

  (let* ([ lines (parse-aoc 10 (compose1 enumerate string->list)) ]
         [ positions (for/fold ([ result '() ])
                               ([ row (enumerate lines) ])
                       (for/fold ([ result result ])
                                 ([ col (car row) ])
                         (let ([ pos (make-rectangular (cdr col) (cdr row)) ])
                           (cons (cons pos
                                       (cell->neighbors pos (car col)))
                                 result)))) ]
         [ start (car (findf (compose1 (curry eq? #\S) cdr)
                             positions)) ]
         [ pipes (make-immutable-hash (filter (compose1 list? cdr)
                                              positions)) ]
         [ start-neighbors (for/list ([ (pos neighbors) pipes ]
                                      #:when (memv start neighbors))
                             pos) ])

    (values start (hash-set pipes start start-neighbors) (length (car lines)) (length lines))))

(define-values (start pipes width height) (parse-input))

(define (part1 start)
  (define (make-loop start prev step)
    (if (= step start)
        (list (cons start (hash-ref pipes start)))
        (let* ([ neighbors (hash-ref pipes step) ]
               [ next-step (car (filter (compose1 not (curry = prev))
                                        neighbors)) ])
          (cons (cons step neighbors) (make-loop start step next-step)))))

  (make-loop start start (car (hash-ref pipes start))))

(define (part2 start)
  (let ([ cells (make-immutable-hash (part1 start)) ])
    (for/sum ([ row (in-range height) ])
      (for/fold ([ sum 0 ][ inside #f ] #:result sum)
                ([ col (in-range width) ])
        (let* ([ pos        (make-rectangular col row)             ]
               [ neighbors  (hash-ref cells pos '())               ]
               [ has-north? (ormap (curry = (+ pos -i)) neighbors) ])
          (cond [ (null? neighbors) (values (if inside (add1 sum) sum) inside) ]
                [ else              (values sum (xor has-north? inside))       ]))))))

;; Tests --------------------------------------------------------------------------------------

(check-equal? (/ (length (part1 start)) 2) 6897)

(check-equal? (part2 start) 367)