#lang racket
(require "../advent.rkt")

(define ns (make-base-namespace))
(define hsh (~> (parse-aoc 21 atoms)
                (map (λ (lst)
                       (cons (car lst)
                             (if (= 2 (length lst))
                                 (cadr lst)
                                 (match-let ([ (list left op right) (cdr lst) ])
                                   (list (eval (string->symbol op) ns) left right))))) _)
                (make-immutable-hash _)))

(define (eval-monkey id [ result identity ][ key #f ][ xform identity ])
  (cond [ (and key (string=? key id)) #t ]
        [ else (let ([ monkey (hash-ref hsh id) ])
                 (if (number? monkey)
                     (result monkey)
                     (match-let ([ (list op left right) monkey ])
                       ((xform op) (eval-monkey left result key xform)
                                   (eval-monkey right result key xform))))) ]))

(define has-human? (λ (id) (eval-monkey id (const #f) "humn" (λ (op) (λ (a b) (or a b))))))

(define (solve-for left? result op val)
  (cond [ (eq? op +) (- result val)                           ]
        [ (eq? op -) (if left? (+ result val) (- val result)) ]
        [ (eq? op *) (/ result val)                           ]
        [ (eq? op /) (if left? (* result val) (/ val result)) ]))

(define (evil-monkey id result)
  (if (string=? "humn" id)
      result
      (match-let ([ (list op left right) (hash-ref hsh id) ])
        (let ([ op (if (string=? "root" id) - op) ])
          (if (has-human? left)
              (evil-monkey left  (solve-for #t result op (eval-monkey right)))
              (evil-monkey right (solve-for #f result op (eval-monkey left))))))))

(time (check-equal? (eval-monkey "root") 56490240862410))  ; Part 1
(time (check-equal? (evil-monkey "root" 0) 3403989691757)) ; Part 2
