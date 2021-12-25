#lang racket

(define-values (vec width height east south)
  (let* ([ lines  (file->lines "day25.txt")         ]
         [ width  (string-length (car lines))       ]
         [ height (length lines)                    ]
         [ vec    (make-vector (* width height) #f) ])
    (for* ([ x (in-range width)  ]
           [ y (in-range height) ])
      (vector-set! vec (+ (* y width) x) (match (string-ref (list-ref lines y) x)
                                           [ #\>  1 ]
                                           [ #\v +i ]
                                           [ _   #f ])))
    (values vec width height 1 +i)))

(define (x+y idx) (values (modulo (real-part idx) width)
                          (modulo (imag-part idx) height)))

(define (vget idx)      (let-values ([ (x y) (x+y idx) ]) (vector-ref vec (+ (* y width) x))))
(define (vset! idx val) (let-values ([ (x y) (x+y idx) ]) (vector-set! vec (+ (* y width) x) val)))

(define (movers dir)
  (for*/fold ([ result '() ])
             ([ x (in-range width)  ]
              [ y (in-range height) ])
    (let* ([ idx (+ x (* y +i)) ]
           [ cuc (vget idx)     ])
      (if (and cuc
               (= cuc dir)
               (not (vget (+ dir idx))))
          (cons idx result)
          result))))

(define (move-all! dir)
  (for/sum ([ idx (in-list (movers dir)) ])
    (let ([ dir (vget idx) ])
      (vset! idx #f)
      (vset! (+ idx dir) dir))
    1))

(define (solve [step 1])
  (if (zero? (+ (move-all! east)
                (move-all! south)))
      step
      (solve (add1 step))))

(= (time (solve)) 334)
