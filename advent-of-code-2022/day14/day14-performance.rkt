#lang racket
(require "../advent.rkt")

;; Original w/ hash             = 7,300 ms
;; Original w/ hasheqv          = 2,700 ms
;; This version w/ vector       =   249 ms
;; This version w/ backtracking =     7 ms !!

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

  (define (move-sand! path)
    (let* ([ point (car path)     ]
           [ d     (+ point  0+i) ]
           [ dl    (+ point -1+i) ]
           [ dr    (+ point  1+i) ])
      (cond [ (> (imag-part point) floor) #f                 ]    ; Into the void !
            [ (not (member? d))  (move-sand! (cons d path))  ]    ; Down
            [ (not (member? dl)) (move-sand! (cons dl path)) ]    ; Down to left
            [ (not (member? dr)) (move-sand! (cons dr path)) ]    ; Down to right
            [ else (set-point! point) path                   ]))) ; Sleeeep (in voice of Mantis)
  ;; ------------------------------------------------------------------------------------------
  (clear!)
  (for ([ path in ]) ; Add rocks
    (for ([ pair (zipn path (cdr path)) ])
      (set-line! (car pair) (cadr pair))))
  (when floor?       ; Add floor
    (set-line! (make-rectangular 0        floor)
               (make-rectangular (sub1 W) floor)))

  (let loop ([ grains 0 ][ path (list source) ])
    (let ([ path (move-sand! path) ])
      (cond [ (not path)            grains                          ]
            [ (= (car path) source) (add1 grains)                   ]
            [ else                  (loop (add1 grains) (cdr path)) ]))))

(time (check-equal? (solve)      862))
(time (check-equal? (solve #t) 28744))
