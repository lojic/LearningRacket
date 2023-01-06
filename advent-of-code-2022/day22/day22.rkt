#lang racket
(require "../advent.rkt")

(define-values (vec width height commands)
  (match-let ([ (list str path) (parse-aoc 22 #:sep "\n\n" #:print-sample #f) ])
    (let* ([ lines    (string-split str "\n")               ]
           [ width    (list-max (map string-length lines))  ]
           [ height   (length lines)                        ]
           [ lines    (map (λ (s)
                             (let ([ len (string-length s) ])
                               (if (< len width)
                                   (string-append s (make-string (- width len) #\space))
                                   s)))
                           lines) ]
           [ vec      (apply vector (string->list (apply string-append lines))) ]
           [ commands (map atom (regexp-match* #px"(\\d+|[LR])" path))          ])
      (values vec width height commands))))

(define right 1)
(define left -1)
(define up   -i)
(define down +i)
(define start-tile 51+1i)

(define (c->i c) (+ (* (sub1 (imag-part c)) width) (sub1 (real-part c))))
(define (vget c) (vector-ref vec (c->i c)))

(define (clockwise dir)
  (match dir
    [ (== right) down  ]
    [ (== down)  left  ]
    [ (== left)  up    ]
    [ (== up)    right ]))

(define (counter-clockwise dir)
  (match dir
    [ (== right) up    ]
    [ (== down)  right ]
    [ (== left)  down  ]
    [ (== up)    left  ]))

(define (in-bounds? pos)
  (let ([ col (real-part pos) ]
        [ row (imag-part pos) ])
    (and (<= 1 col width)                     ; Horizontal ok
         (<= 1 row height)                    ; Vertical ok
         (not (char=? #\space (vget pos)))))) ; Not a space

(define (most-tile-pos pos dir)
  (let loop ([ pos pos ])
    (if (in-bounds? pos)
        (loop (+ pos dir))
        (- pos dir)))) ; We went to far, backup

(define (dir-value dir)
  (match dir
    [ (== right) 0 ]
    [ (== down)  1 ]
    [ (== left)  2 ]
    [ (== up)    3 ]))

(define (next-pos pos dir)
  (let ([ pos* (+ pos dir) ])
    (if (in-bounds? pos*)
        pos*
        (most-tile-pos pos (- dir)))))

(define (execute-move pos dir n)
  (if (zero? n)
      pos
      (let ([ pos* (next-pos pos dir) ])
        (if (char=? #\# (vget pos*))
            pos
            (execute-move pos* dir (sub1 n))))))

(define (execute-command command pos dir)
  (if (number? command)
      (let ([ pos* (execute-move pos dir command) ])
        (values pos* dir))
      (values  pos (match command
                     [ "L" (counter-clockwise dir) ]
                     [ "R" (clockwise dir)         ]
                     [ _   (error "what?")         ]))))

(define (part1)
  (let loop ([ pos      start-tile ]
             [ dir      right      ]
             [ commands commands   ])
    (if (null? commands)
        (+ (* 1000 (imag-part pos))
           (* 4 (real-part pos))
           (dir-value dir))
        (let-values ([ (pos* dir*) (execute-command (car commands) pos dir) ])
          (loop pos* dir* (cdr commands))))))

(time (check-equal? (part1) 136054))
