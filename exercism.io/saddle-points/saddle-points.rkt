#lang racket
(provide saddle-points)

(define (saddle-points str)
  (define (string->matrix str)
    (for/vector ([row (in-list (string-split str "\n"))])
                (list->vector (map string->number (string-split row)))))
  (let* ([vec (string->matrix str)]
         [row-maxs (vector-map (curry vector-argmax identity) vec)]
         [col-mins (for/vector ([col (in-range (vector-length (vector-ref vec 0)))])
                               (vector-argmin identity (for/vector ([v (in-vector vec)])
                                                                   (vector-ref v col))))])
    (for*/list ([row (in-range (vector-length row-maxs))]
                [col (in-range (vector-length col-mins))]
                [element (list (vector-ref (vector-ref vec row) col))]
                #:when (and (>= element (vector-ref row-maxs row))
                            (<= element (vector-ref col-mins col))))
               (list row col))))
