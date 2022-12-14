#lang racket
(require "../advent.rkt")

(define in (for/list ([ path (parse-aoc 14 numbers) ])
             (for/list ([ pair (chunk 2 path) ])
               (apply make-rectangular pair))))

(define (solve [ floor? #f ])
  (define source   500)
  (define cave     (make-hasheqv))
  (define bottom   (+ 2 (list-max (map imag-part (flatten in)))))
  (define member?  (curry hash-has-key? cave))

  (define (add! p)          (hash-set! cave p #t))
  (define (set-line! p1 p2) (map add! (coordinates-range p1 p2)))

  (define (move-sand! path)
    (let* ([ point (car path)     ]
           [ d     (+ point  0+i) ]
           [ dl    (+ point -1+i) ]
           [ dr    (+ point  1+i) ])
      (cond [ (> (imag-part point) bottom)  #f               ]    ; Into the void !
            [ (not (member? d))  (move-sand! (cons d path))  ]    ; Down
            [ (not (member? dl)) (move-sand! (cons dl path)) ]    ; Down to left
            [ (not (member? dr)) (move-sand! (cons dr path)) ]    ; Down to right
            [ else (add! point) path                         ]))) ; Sleeeep (in voice of Mantis)
  ;; ------------------------------------------------------------------------------------------
  (hash-clear! cave)
  (for ([ path in ]) ; Add rocks
    (for ([ pair (zipn path (cdr path)) ])
      (set-line! (car pair) (cadr pair))))
  (when floor?       ; Add floor
    (set-line! (make-rectangular 300 bottom)
               (make-rectangular 700 bottom)))

  (let loop ([ grains 0 ][ path (list source) ])
    (let ([ path (move-sand! path) ])
      (cond [ (not path)            grains                          ]
            [ (= (car path) source) (add1 grains)                   ]
            [ else                  (loop (add1 grains) (cdr path)) ]))))

(time (check-equal? (solve)      862))
(time (check-equal? (solve #t) 28744))
