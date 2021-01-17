#lang racket

(provide (struct-out move)
         create-move
         east
         king-offsets
         knight-offsets
         north
         north-east
         north-west
         south
         south-east
         south-west
         west)

(struct move (src
              src-idx
              dst-idx
              captured-piece
              promoted-piece
              is-castle-queenside?
              is-castle-kingside?
              is-ep-capture?)
        #:transparent
        #:mutable)

;; Directions
(define north      -10)
(define north-east  -9)
(define east         1)
(define south-east  11)
(define south       10)
(define south-west   9)
(define west        -1)
(define north-west -11)

(define king-offsets (list north north-east east south-east south south-west west north-west))

(define knight-offsets
  (list (+ north north east)
        (+ east east north)
        (+ east east south)
        (+ south south east)
        (+ south south west)
        (+ west west south)
        (+ west west north)
        (+ north north west)))

(define (create-move src
                     src-idx
                     dst-idx
                     #:captured-piece       [ captured-piece       #f ]
                     #:promoted-piece       [ promoted-piece       #f ]
                     #:is-castle-queenside? [ is-castle-queenside? #f ]
                     #:is-castle-kingside?  [ is-castle-kingside?  #f ]
                     #:is-ep-capture?       [ is-ep-capture?       #f ])
  (move src
        src-idx
        dst-idx
        captured-piece
        promoted-piece
        is-castle-queenside?
        is-castle-kingside?
        is-ep-capture?))
