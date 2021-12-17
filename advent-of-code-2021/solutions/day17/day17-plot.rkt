#lang racket

(require plot)

(define-values (x-max y-min) (values 250 -105))

(define (trajectory dx dy [x 0] [y 0] [ result '() ])
  (if (or (> x x-max) (< y y-min))
      (reverse result)
      (let ([ result (cons (list x y) result) ]
            [ dx (cond [ (> dx 0) (sub1 dx) ]
                       [ else     dx        ]) ]
            [ dy  (sub1 dy)   ]
            [ x   (+ x dx)    ]
            [ y   (+ y dy)    ])
        (trajectory dx dy x y result))))

(define (plot-trajectory dx dy)
  (let ([ pts (trajectory dx dy) ])
    (plot (list (rectangles (list (vector (ivl 206 250) (ivl -57 -105))))
                (points pts))
          #:x-min -1
          #:x-max 300
          #:y-min -150
          #:y-max 200)))

(plot-trajectory 21 12)
