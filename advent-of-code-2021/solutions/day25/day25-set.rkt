#lang racket

;; This version uses two sets instead of a vector.

(define-values (east-set south-set width height east south)
  (let* ([ lines     (file->lines "day25.txt")   ]
         [ width     (string-length (car lines)) ]
         [ height    (length lines)              ]
         [ east-set  (mutable-set)               ]
         [ south-set (mutable-set)               ])
    (for* ([ x (in-range width)  ]
           [ y (in-range height) ])
      (let ([ dir (match (string-ref (list-ref lines y) x)
                    [ #\>  1 ]
                    [ #\v +i ]
                    [ _   #f ]) ])
        (when dir
          (if (= dir 1)
              (set-add! east-set (+ x (* y +i)))
              (set-add! south-set (+ x (* y +i)))))))
    (values east-set south-set width height 1 +i)))

(define (inc idx dir)
  (let* ([ x (modulo (+ (real-part idx) (real-part dir)) width) ]
         [ y (modulo (+ (imag-part idx) (imag-part dir)) height) ])
    (+ x (* y +i))))

(define (move-all! dir)
  (let* ([ s  (if (= dir east) east-set south-set) ]
         [ s* (mutable-set)                        ]
         [ sum (for/sum ([ idx (in-set s) ])
                 (let ([ n (inc idx dir) ])
                   (if (or (set-member? east-set n)
                           (set-member? south-set n))
                       (begin
                         (set-add! s* idx)
                         0)
                       (begin
                         (set-add! s* n)
                         1)))) ])
    (if (= dir east)
        (set! east-set s*)
        (set! south-set s*))
    sum))

(define (solve [step 1])
  (if (zero? (+ (move-all! east)
                (move-all! south)))
      step
      (solve (add1 step))))

(= (time (solve)) 334)
