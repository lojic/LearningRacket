#lang racket

;; Port of https://github.com/tckmn/polyaoc-2020/blob/master/11/rb/11.rb

;; Pre-requisites -----------------------------------------------------------------------------
(define-syntax-rule (printvec v)
  (for ([ row (in-vector v) ])
    (println row)))
(define-syntax-rule (vget v r c) (vector-ref (vector-ref v r) c))
(define-syntax-rule (vset v r c val) (vector-set! (vector-ref v r) c val))
(define-syntax-rule (vequal? v1 v2)
  (for/and ([ row1 (in-vector v1) ][ row2 (in-vector v2) ])
    (equal? row1 row2)))
(define-syntax-rule (dup v) (for/vector ([ row (in-vector seats) ]) (vector-copy row)))
;; --------------------------------------------------------------------------------------------

(define seats (let* ([ lines (map (compose list->vector string->list (curry format "*~a*"))
                                  (file->lines "day11.txt")) ]
                     [ tmp (make-vector (vector-length (car lines)) #\*) ])
                (list->vector (append (list tmp) lines (list tmp)))))

(define adj8 (remove '(0 0) (cartesian-product '(-1 0 1) '(-1 0 1))))

(define sight (for/vector ([ i   (in-naturals)     ]
                           [ row (in-vector seats) ])
                (for/vector ([ j    (in-naturals)   ]
                             [ seat (in-vector row) ])
                  (and (not (char=? #\* seat))
                       (map (match-lambda
                             [ (list dx dy)
                               (let loop ([ x (+ i dx) ][ y (+ j dy) ])
                                 (let ([ seat (vget seats x y) ])
                                   (if (char=? seat #\.)
                                       (loop (+ x dx) (+ y dy))
                                       (list (- x i) (- y j)))))])
                            adj8)))))

(define (part pt)
  (let loop ([ arr (dup seats) ])
    (let ([ new (for/vector ([ i   (in-naturals)   ]
                             [ row (in-vector arr) ])
                  (for/vector ([ j    (in-naturals)   ]
                               [ seat (in-vector row) ])
                    (if (or (char=? seat #\L) (char=? seat #\#))
                        (let ([ num (count (match-lambda
                                            [ (list dx dy)
                                              (char=? #\# (vget arr (+ i dx) (+ j dy))) ])
                                           (if (= pt 1) adj8 (vget sight i j))) ])
                          (if (char=? seat #\L)
                              (if (= num 0) #\# #\L)
                              (if (>= num (if (= pt 1) 4 5)) #\L #\#)))
                        seat))) ])
      (if (vequal? new arr)
          (for/sum ([ row (in-vector arr) ])
            (vector-count (curry char=? #\#) row))
          (loop new)))))

(part 1)
(part 2)
