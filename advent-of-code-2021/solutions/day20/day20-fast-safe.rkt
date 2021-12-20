#lang racket

;; Some optimizations, but no unsafe operations. Run times are as follows:
;; day20.rkt             = 610 ms
;; This version          = 165 ms
;; day20-performance.rkt =  53 ms

(require threading
         "./syntax.rkt"
         "../../advent/advent.rkt")
(struct image (dim vec def) #:transparent)

(define (solve file-name n)
  (let-values ([ (iea img) (parse file-name) ])
    (let ([ vec (~> (iterate (enhance-image iea) img n)
                    image-vec) ])
      (for/sum ([ i (in-range (vector-length vec)) ])
        (vector-ref vec i)))))

(define ((enhance-image iea) img)
  (define (next-default i)
    (or (and (= 1 (vector-ref iea 0))
             (= 0 i)
             1)
        0))

  (define (enhance-pixel x y)
    (let ([ idx (+ (arithmetic-shift (bit -1 -1) 8)
                   (arithmetic-shift (bit  0 -1) 7)
                   (arithmetic-shift (bit  1 -1) 6)
                   (arithmetic-shift (bit -1  0) 5)
                   (arithmetic-shift (bit  0  0) 4)
                   (arithmetic-shift (bit  1  0) 3)
                   (arithmetic-shift (bit -1  1) 2)
                   (arithmetic-shift (bit  0  1) 1)
                   (bit  1  1)) ])
      (vector-ref iea idx)))

  (let* ([ dim     (+ (image-dim img) 2) ]
         [ img-out (image dim
                          (make-vector (* dim dim))
                          (image-def img)) ])
    (for* ([ x (in-range dim) ]
           [ y (in-range dim) ])
      (set-pixel! img-out x y (enhance-pixel (- x 1)
                                             (- y 1))))
    (struct-copy image img-out [ def (next-default (image-def img-out)) ])))

(define (get-pixel img x y)
  (let* ([ dim (image-dim img) ]
         [ end (- dim 1)       ])
    (if (and (<= 0 x end) (<= 0 y end))
        (vector-ref (image-vec img) (+ (* y dim) x))
        (image-def img))))

(define (set-pixel! img x y c)
  (vector-set! (image-vec img) (+ (* y (image-dim img)) x) c))

(define (parse file-name)
  (define (make-iea s)
    (let ([ iea (make-vector 512) ])
      (for ([ i (in-range 512) ])
        (vector-set! iea i (if (char=? #\# (string-ref s i))
                               1
                               0)))
      iea))

  (define (make-img lines)
    (let* ([ dim (string-length (car lines)) ]
           [ vec (make-vector (* dim dim))   ])
      (for ([ line (in-list lines) ]
            [ y    (in-range dim)  ])
        (for ([ x (in-range dim) ])
          (vector-set! vec
                       (+ (* y dim) x)
                       (if (char=? #\# (string-ref line x))
                           1
                           0))))
      (image dim vec 0)))

  (let ([ lines (file->lines file-name) ])
    (values (make-iea (first lines))
            (make-img (cddr lines)))))

(time (solve "day20.txt" 50))

;; Tests --------------------------------------------------------------------------------------

(module+ test
  (require rackunit)
  (let ([ solver (curry solve "day20.txt") ])
    (check-equal? (solver 2) 5057)
    (check-equal? (time (solver 50)) 18502)))
