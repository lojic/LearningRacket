#lang racket
(require "../advent.rkt")

(define in (for/list ([ path (parse-aoc 14 numbers) ])
             (for/list ([ pair (chunk 2 path) ])
               (apply make-rectangular pair))))

(define cave   (make-hasheqv))
(define source 500)

(define (solve [ floor? #f ])
  (create-cave!)
  (when floor? (add-floor!))
  (let-values ([ (left right bottom) (bounds) ])
    (let loop ([ grains 0 ])
      (let ([ p (move-sand! source left right bottom) ])
        (cond [ (not p)      grains               ]
              [ (= p source) (add1 grains)        ]
              [ else         (loop (add1 grains)) ])))))

(define (create-cave!)
  (hash-clear! cave)
  (for ([ path in ]) (set-path! path)))

(define (add-floor!)
  (let-values ([ (left right bottom) (bounds) ])
    (set-line! (make-rectangular (- left  (* 2 bottom)) (+ 2 bottom))
               (make-rectangular (+ right (* 2 bottom)) (+ 2 bottom)))))

(define (set-path! path)
  (for ([ pair (zipn path (cdr path)) ])
    (set-line! (car pair) (cadr pair))))

(define set-point! (curry hash-set! cave))

(define (set-line! p1 p2)
  (for ([ p (coordinates-range p1 p2) ])
    (set-point! p 'rock)))

(define (bounds)
  (for/fold ([ left   (real-part source) ]
             [ right  (real-part source) ]
             [ bottom 0                  ])
            ([ point  (hash-keys cave)   ])
    (let ([ x (real-part point) ][ y (imag-part point) ])
      (values (min left x) (max right x) (max bottom y)))))

(define (move-sand! point l r b)
  (let ([ x  (real-part point) ]
        [ y  (imag-part point) ]
        [ d  (+ point +i)      ]
        [ dl (+ point -1+i)    ]
        [ dr (+ point 1+i)     ])
    (cond [ (or (< x l) (> x r) (> y b))  #f                    ] ; Into the void !
          [ (not (hash-has-key? cave d))  (move-sand! d l r b)  ] ; Down
          [ (not (hash-has-key? cave dl)) (move-sand! dl l r b) ] ; Down to left
          [ (not (hash-has-key? cave dr)) (move-sand! dr l r b) ] ; Down to right
          [ else (set-point! point 'sand)                         ; Rest
                 point ])))

(time (check-equal? (solve)      862))
(time (check-equal? (solve #t) 28744))
