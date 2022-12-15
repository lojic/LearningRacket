#lang racket
(require "../advent.rkt")

(define in (for/list ([ path (parse-aoc 14 numbers) ])
             (for/list ([ pair (chunk 2 path) ])
               (apply make-rectangular pair))))

(define (solve [ floor? #f ])
  (define source   500)
  (define bottom   (+ 2 (list-max (map imag-part (flatten in)))))
  (define invalid? (compose (curry <= bottom) imag-part))

  (define add-sand (λ (p c) (hash-set c p 'sand)))
  (define add-rock (λ (p c) (hash-set c p 'rock)))
  (define member?  (λ (p c) (hash-has-key? c p)))

  (define (count-sand c)
    (~> (hash-values c)              ; Types of items
        (filter (curry eq? 'sand) _) ; Keep only 'sand
        length))                     ; How many?

  (define (free? c p)
    (not                                       ; If neither of the following, it's free
     (or (and floor? (= bottom (imag-part p))) ; We have a floor and the point is on it
         (member? p c))))                      ; Something is already at that point

  (define (add-line cave p1 p2)
    (foldl add-rock
           cave
           (coordinates-range p1 p2)))

  (define (add-rocks cave)
    (for/fold ([ cave cave ])
              ([ path (in-list in) ])
      (for/fold ([ cave cave ])
                ([ pair (in-list (zipn path (cdr path))) ])
        (apply add-line cave pair))))

  (define (move-sand cave path)
    (let* ([ point (car path)     ]
           [ d     (+ point  0+i) ]
           [ dl    (+ point -1+i) ]
           [ dr    (+ point  1+i) ])
      (cond [ (invalid? point) (values cave #f)                    ]    ; Into the void !
            [ (free? cave d)   (move-sand cave (cons d path))      ]    ; Down
            [ (free? cave dl)  (move-sand cave (cons dl path))     ]    ; Down to left
            [ (free? cave dr)  (move-sand cave (cons dr path))     ]    ; Down to right
            [ else             (values (add-sand point cave) path) ]))) ; Sleeeep (in voice of Mantis)

  ;; ------------------------------------------------------------------------------------------

  (let drop-sand ([ cave (add-rocks (hasheqv)) ]
                  [ path (list source)         ])
    (let-values ([ (cave path) (move-sand cave path) ])
      (if (or (not path)              ; Fell into void
              (= (car path) source))  ; Source can't move
          (count-sand cave)
          (drop-sand cave (cdr path))))))

(time (check-equal? (solve)      862))
(time (check-equal? (solve #t) 28744))
