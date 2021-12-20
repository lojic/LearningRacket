#lang racket

(require threading
         racket/unsafe/ops
         "../../advent/advent.rkt"
         (for-syntax racket/syntax))
(struct image (dim vec def))

(define-syntax (bit stx)
  (syntax-case stx ()
    [(_ dx dy)
     (with-syntax ([ img       (format-id stx "img")       ]
                   [ x         (format-id stx "x")         ]
                   [ y         (format-id stx "y")         ]
                   [ get-pixel (format-id stx "get-pixel") ])
       #`(if (unsafe-char=? #\# (get-pixel img (unsafe-fx+ x dx) (unsafe-fx+ y dy)))
             1
             0)) ]))

(define (solve file-name n)
  (let-values ([ (iea img) (parse file-name) ])
    (~>> (iterate (enhance-image iea) img n)
         image-vec
         (vector-count (λ (c) (char=? c #\#))))))

(define ((enhance-image iea) img)
  (define (next-default c)
    (or (and (unsafe-char=? #\# (unsafe-vector-ref iea 0)) (unsafe-char=? #\. c) #\#) #\.))

  (define (enhance-pixel x y)
    (let ([ idx (unsafe-fx+ (unsafe-fxlshift (bit -1 -1) 8)
                            (unsafe-fxlshift (bit  0 -1) 7)
                            (unsafe-fxlshift (bit  1 -1) 6)
                            (unsafe-fxlshift (bit -1  0) 5)
                            (unsafe-fxlshift (bit  0  0) 4)
                            (unsafe-fxlshift (bit  1  0) 3)
                            (unsafe-fxlshift (bit -1  1) 2)
                            (unsafe-fxlshift (bit  0  1) 1)
                            (bit  1  1)) ])
      (unsafe-vector-ref iea idx)))

  (let* ([ dim     (unsafe-fx+ (image-dim img) 2) ]
         [ img-out (image dim
                          (make-vector (unsafe-fx* dim dim))
                          (image-def img)) ])
    (for* ([ x (in-range dim) ]
           [ y (in-range dim) ])
      (set-pixel! img-out x y (enhance-pixel (unsafe-fx- x 1) (unsafe-fx- y 1))))
    (struct-copy image img-out [ def (next-default (image-def img-out)) ])))

(define (get-pixel img x y)
  (let* ([ dim (image-dim img)    ]
         [ end (unsafe-fx- dim 1) ])
    (if (and (unsafe-fx<= 0 x end) (unsafe-fx<= 0 y end))
        (unsafe-vector-ref (image-vec img) (unsafe-fx+ (unsafe-fx* y dim) x))
        (image-def img))))

(define (set-pixel! img x y c)
  (unsafe-vector-set! (image-vec img) (unsafe-fx+ (unsafe-fx* y (image-dim img)) x) c))

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
