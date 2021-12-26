#lang racket

;; Tried using a hash table instead of a vector, but it's
;; unsurprisingly slower. More than 15x slower. I think the grid would
;; have to be very sparse before this version would win.

(define-values (hsh width height east south)
  (let* ([ lines  (file->lines "day25.txt")   ]
         [ width  (string-length (car lines)) ]
         [ height (length lines)              ]
         [ hsh    (make-hash)                 ])
    (for* ([ x (in-range width)  ]
           [ y (in-range height) ])
      (let ([ dir (match (string-ref (list-ref lines y) x)
                    [ #\>  1 ]
                    [ #\v +i ]
                    [ _   #f ]) ])
        (when dir
          (hash-set! hsh (+ x (* y +i)) dir))))
    (values hsh width height 1 +i)))

(define (inc idx dir)
  (let* ([ x (modulo (+ (real-part idx) (real-part dir)) width) ]
         [ y (modulo (+ (imag-part idx) (imag-part dir)) height) ])
    (+ x (* y +i))))

(define (movers dir)
  (filter-map (λ (pair)
                (match-let ([ (cons idx cuc) pair ])
                  (if (and (= cuc dir)
                           (not (hash-ref hsh (inc idx dir) #f)))
                    idx
                    #f)))
              (hash->list hsh)))

(define (move-all! dir)
  (for/sum ([ idx (in-list (movers dir)) ])
    (let ([ dir* (hash-ref hsh idx) ])
      (when (not (= dir dir*)) (error "oops"))
      (hash-remove! hsh idx)
      (hash-set! hsh (inc idx dir) dir))
    1))

(define (solve [step 1])
  (if (zero? (+ (move-all! east)
                (move-all! south)))
      step
      (solve (add1 step))))

(= (time (solve)) 334)
