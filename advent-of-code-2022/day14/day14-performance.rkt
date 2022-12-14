#lang racket
(require "../advent.rkt")

;; Using a vector instead of a hash dropped from 2,700 ms to 249 ms

(define in (for/list ([ path (parse-aoc 14 numbers) ])
             (for/list ([ pair (chunk 2 path) ])
               (apply make-rectangular pair))))

(define source 500)
(define bottom (list-max (map imag-part (flatten in))))
(define floor  (+ 2 bottom))
(define W      (+ source floor 3))
(define cave   (make-vector (* W (+ 2 floor)) #f))

(define (solve [ floor? #f ])
  (define (add! p)       (vector-set! cave (p->i p) #t))
  (define (clear!)       (vector-fill! cave #f))
  (define (member? p)    (vector-ref cave (p->i p)))
  (define (p->i p)       (+ (* (imag-part p) W) (real-part p)))
  (define (set-point! p) (add! p))

  (define (set-line! p1 p2)
    (for ([ p (coordinates-range p1 p2) ])
      (set-point! p)))

  (define (move-sand! point)
    (let ([ d  (+ point  0+i) ]
          [ dl (+ point -1+i) ]
          [ dr (+ point  1+i) ])
      (cond [ (> (imag-part point) floor) #f     ]    ; Into the void !
            [ (not (member? d))  (move-sand! d)  ]    ; Down
            [ (not (member? dl)) (move-sand! dl) ]    ; Down to left
            [ (not (member? dr)) (move-sand! dr) ]    ; Down to right
            [ else (set-point! point) point      ]))) ; Sleeeep (in voice of Mantis)
  ;; ------------------------------------------------------------------------------------------
  (clear!)
  (for ([ path in ]) ; Add rocks
    (for ([ pair (zipn path (cdr path)) ])
      (set-line! (car pair) (cadr pair))))
  (when floor?       ; Add floor
    (set-line! (make-rectangular 0        floor)
               (make-rectangular (sub1 W) floor)))

  (let loop ([ grains 0 ])
    (let ([ p (move-sand! source) ])
      (cond [ (not p)      grains               ]
            [ (= p source) (add1 grains)        ]
            [ else         (loop (add1 grains)) ]))))

(time (check-equal? (solve)      862))
(time (check-equal? (solve #t) 28744))
