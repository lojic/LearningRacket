#lang racket

(require threading "../../advent/advent.rkt")
(struct image (dim vec def))

(define (solve file-name n)
  (let-values ([ (iea img) (parse file-name) ])
    (~>> (iterate (enhance-image iea) img n)
         image-vec
         (vector-count (curry char=? #\#)))))

(define ((enhance-image iea) img)
  (define (next-default c)
    (or (and (char=? #\# (vector-ref iea 0)) (char=? #\. c) #\#) #\.))

  (define (enhance-pixel x y)
    (~>> (for*/list ([ dy (inclusive-range -1 1) ]
                     [ dx (inclusive-range -1 1) ])
           (vec-get img (+ x dx) (+ y dy)))
         (map (λ (c) (match c [ #\. 0 ][ #\# 1 ])))
         (bool-list->decimal)
         (vector-ref iea)))

  (let* ([ dim     (+ (image-dim img) 2) ]
         [ img-out (image dim
                          (make-vector (* dim dim))
                          (image-def img)) ])
    (for* ([ x (in-range dim) ]
           [ y (in-range dim) ])
      (vec-set! img-out x y (enhance-pixel (sub1 x) (sub1 y))))
    (struct-copy image img-out [ def (next-default (image-def img-out)) ])))

(define (vec-get img x y)
  (let* ([ dim (image-dim img) ]
         [ end (sub1 dim)      ])
    (if (and (<= 0 x end) (<= 0 y end))
        (vector-ref (image-vec img) (+ (* y dim) x))
        (image-def img))))

(define (vec-set! img x y c)
  (vector-set! (image-vec img) (+ (* y (image-dim img)) x) c))

(define (parse file-name)
  (let ([ lines (file->lines file-name) ])
    (values (~> (first lines) string->list list->vector)
            (let ([ lines (cddr lines) ])
              (image (string-length (car lines))
                     (~> (apply string-append lines) string->list list->vector)
                     #\.)))))

;; Tests --------------------------------------------------------------------------------------

(module+ test
  (require rackunit)
  (let ([ solver (curry solve "day20.txt") ])
    (check-equal? (solver 2) 5057)
    (check-equal? (solver 50) 18502)))
