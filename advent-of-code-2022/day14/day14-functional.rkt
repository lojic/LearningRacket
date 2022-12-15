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
  (define member?  (λ (c p) (hash-has-key? c p)))
  (define free?    (λ (c p) (not (member? c p))))

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

  (define (add-floor cave)
    (if floor?
        (add-line cave
                  (make-rectangular (- source 200) bottom)
                  (make-rectangular (+ source 200) bottom))
        cave))

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

  (define create-cave (compose add-floor add-rocks hasheqv))
  (define count-sand  (λ (c) (length (filter (curry eq? 'sand) (hash-values c)))))

  ;; ------------------------------------------------------------------------------------------

  (let drop-sand ([ cave (create-cave) ]
                  [ path (list source) ])
    (let-values ([ (cave path) (move-sand cave path) ])
      (if (or (not path)              ; Fell into void
              (= (car path) source))  ; Source can't move
          (count-sand cave)
          (drop-sand cave (cdr path))))))

(time (check-equal? (solve)      862))
(time (check-equal? (solve #t) 28744))
