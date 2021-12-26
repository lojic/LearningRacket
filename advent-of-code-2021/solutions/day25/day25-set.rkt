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

(define (movers s dir)
  (filter-map (λ (idx)
                (let ([ idx* (inc idx dir) ])
                  (if (not (or (set-member? east-set idx*)
                               (set-member? south-set idx*)))
                      idx
                      #f)))
              (set->list s)))

(define (move-all! dir)
  (let ([ s (if (= dir east) east-set south-set) ])
    (for/sum ([ idx (in-list (movers s dir)) ])
      (set-remove! s idx)
      (set-add! s (inc idx dir))
      1)))

(define (solve [step 1])
  (if (zero? (+ (move-all! east)
                (move-all! south)))
      step
      (solve (add1 step))))

(= (time (solve)) 334)
