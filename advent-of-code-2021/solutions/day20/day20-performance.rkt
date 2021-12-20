#lang racket

(require threading
         racket/fixnum
         racket/unsafe/ops
         "../../advent/advent.rkt"
         (for-syntax racket/syntax))
(struct image (dim vec def) #:transparent)

(define-syntax (bit stx)
  (syntax-case stx ()
    [(_ dx dy)
     (with-syntax ([ img       (format-id stx "img")       ]
                   [ x         (format-id stx "x")         ]
                   [ y         (format-id stx "y")         ]
                   [ get-pixel (format-id stx "get-pixel") ])
       #`(get-pixel img (unsafe-fx+ x dx) (unsafe-fx+ y dy))) ]))

(define (solve file-name n)
  (let-values ([ (iea img) (parse file-name) ])
    (let* ([ vec (~> (iterate (enhance-image iea) img n)
                     image-vec) ]
           [ len (unsafe-fxvector-length vec) ])
      (let loop ([ i 0 ][ sum 0 ])
        (if (unsafe-fx< i len)
            (loop (unsafe-fx+ i 1) (unsafe-fx+ sum (unsafe-fxvector-ref vec i)))
            sum)))))

(define ((enhance-image iea) img)
  (define (next-default i)
    (or (and (unsafe-fx= 1 (unsafe-fxvector-ref iea 0)) (unsafe-fx= 0 i) 1) 0))

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
      (unsafe-fxvector-ref iea idx)))

  (let* ([ dim     (unsafe-fx+ (image-dim img) 2) ]
         [ img-out (image dim
                          (make-fxvector (unsafe-fx* dim dim))
                          (image-def img)) ])
    (for* ([ x (in-range dim) ]
           [ y (in-range dim) ])
      (set-pixel! img-out x y (enhance-pixel (unsafe-fx- x 1) (unsafe-fx- y 1))))
    (struct-copy image img-out [ def (next-default (image-def img-out)) ])))

(define (get-pixel img x y)
  (let* ([ dim (image-dim img)    ]
         [ end (unsafe-fx- dim 1) ])
    (if (and (unsafe-fx<= 0 x end) (unsafe-fx<= 0 y end))
        (unsafe-fxvector-ref (image-vec img) (unsafe-fx+ (unsafe-fx* y dim) x))
        (image-def img))))

(define (set-pixel! img x y c)
  (unsafe-fxvector-set! (image-vec img) (unsafe-fx+ (unsafe-fx* y (image-dim img)) x) c))

(define (parse file-name)
  (define (make-iea s)
    (let ([ iea (make-fxvector 512) ])
      (for ([ i (in-range 512) ])
        (unsafe-fxvector-set! iea i (if (unsafe-char=? #\# (string-ref s i))
                                        1
                                        0)))
      iea))

  (define (make-img lines)
    (let* ([ dim (string-length (car lines)) ]
           [ vec (make-fxvector (unsafe-fx* dim dim)) ])
      (for ([ line (in-list lines) ]
            [ y    (in-range dim)  ])
        (for ([ x (in-range dim) ])
          (unsafe-fxvector-set! vec
                                (unsafe-fx+ (unsafe-fx* y dim) x)
                                (if (unsafe-char=? #\# (string-ref line x))
                                    1
                                    0))))
      (image dim vec 0)))

  (let ([ lines (file->lines file-name) ])
    (values (make-iea (first lines))
            (make-img (cddr lines)))))

(time (solve "day20.txt" 50))

;; Tests --------------------------------------------------------------------------------------

#;(module+ test
  (require rackunit)
  (let ([ solver (curry solve "day20.txt") ])
    (check-equal? (solver 2) 5057)
    (check-equal? (time (solver 50)) 18502)))
