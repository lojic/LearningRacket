#lang racket
(require "../advent.rkt")

(define (parse-input)
  (define (cell->neighbors pos c)
    (define inc (curry + pos))

    (match c
      [ #\| (list (inc -i) (inc +i)) ] ; north and south
      [ #\- (list (inc 1)  (inc -1)) ] ; east and west
      [ #\L (list (inc -i) (inc 1))  ] ; north and east
      [ #\J (list (inc -i) (inc -1)) ] ; north and west
      [ #\7 (list (inc +i) (inc -1)) ] ; south and west
      [ #\F (list (inc +i) (inc 1))  ] ; south and east
      [ #\. #f                       ] ; ignore
      [ #\S 'start                   ]))

  (let* ([ positions (for/fold ([ result '() ])
                               ([ row (enumerate (parse-aoc 10 (compose1 enumerate string->list))) ])
                       (for/fold ([ result result ])
                                 ([ col (car row) ])
                         (let ([ pos (make-rectangular (cdr col) (cdr row)) ])
                           (cons (cons pos (cell->neighbors pos (car col)))
                                 result)))) ]
         [ start (car (findf (compose1 (curry eq? 'start) cdr) positions)) ]
         [ pipes (make-immutable-hash (filter (compose1 list? cdr) positions)) ]
         [ start-neighbors (for/list ([ (pos neighbors) pipes ]
                                      #:when (memv start neighbors))
                             pos) ])

    (values start start-neighbors pipes)))

(define-values (start start-neighbors pipes) (parse-input))

(define (make-loop prev step loop)
  (if (= step start)
      loop
      (let ([ next-step (car (filter (compose1 not (curry = prev))
                                     (hash-ref pipes step))) ])
        (make-loop step next-step (cons step loop)))))

(define (part1 start)
  (let ([ loop (make-loop start (car start-neighbors) (list start)) ])
    (/ (length loop) 2)))

;; Tests --------------------------------------------------------------------------------------

(check-equal? (part1 start) 6897)
