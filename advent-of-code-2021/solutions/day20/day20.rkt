#lang racket

;; Initial, un-optimized version. Run times are as follows:
;; This version          = 610 ms
;; day20-fast-safe.rkt   = 165 ms
;; day20-performance.rkt =  53 ms

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
    (~>> (for*/list ([ dy (in-range -1 2) ]
                     [ dx (in-range -1 2) ])
           (match (get-pixel img (+ x dx) (+ y dy))
             [ #\. 0 ][ #\# 1 ]))
         (bool-list->decimal)
         (vector-ref iea)))

  (let* ([ dim     (+ (image-dim img) 2) ]
         [ img-out (image dim
                          (make-vector (* dim dim))
                          (image-def img)) ])
    (for* ([ x (in-range dim) ]
           [ y (in-range dim) ])
      (set-pixel! img-out x y (enhance-pixel (sub1 x) (sub1 y))))
    (struct-copy image img-out [ def (next-default (image-def img-out)) ])))

(define (get-pixel img x y)
  (let* ([ dim (image-dim img) ]
         [ end (sub1 dim)      ])
    (if (and (<= 0 x end) (<= 0 y end))
        (vector-ref (image-vec img) (+ (* y dim) x))
        (image-def img))))

(define (set-pixel! img x y c)
  (vector-set! (image-vec img) (+ (* y (image-dim img)) x) c))

(define (parse file-name)
  (let ([ lines (file->lines file-name) ])
    (values (~> (first lines) string->list list->vector)
            (let ([ lines (cddr lines) ])
              (image (string-length (car lines))
                     (~> (apply string-append lines) string->list list->vector)
                     #\.)))))

(time (solve "day20.txt" 50))

;; Tests --------------------------------------------------------------------------------------

(module+ test
  (require rackunit)
  (let ([ solver (curry solve "day20.txt") ])
    (check-equal? (solver 2) 5057)
    (check-equal? (time (solver 50)) 18502)))
