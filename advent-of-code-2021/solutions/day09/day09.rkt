#lang racket

(require "../../advent/advent.rkt" threading)

(define-values (get width height)
  (let* ([ lines  (file->lines "day09.txt")     ]
         [ width  (string-length (first lines)) ]
         [ height (length lines)                ]
         [ cave   (~>> lines
                      (apply string-append)
                      (string->list)
                      (map (compose (curryr - (char->integer #\0))
                                    char->integer))
                      list->vector) ]
         [ get (λ (x y)
                 (if (and (<= 0 x (sub1 width))
                          (<= 0 y (sub1 height)))
                     (vector-ref cave (+ (* y width) x))
                     9)) ])

    (values get width height)))

(define (low-points)
  (for*/fold ([ points '() ])
             ([ x (in-range width)  ]
              [ y (in-range height) ])
    (let ([ n (get x y) ])
      (if (and (< n (get x (sub1 y)))  ; North
               (< n (get (add1 x) y))  ; East
               (< n (get x (add1 y)))  ; South
               (< n (get (sub1 x) y))) ; West
          (cons (list x y n) points)
          points))))

(define (get-basin tuple)
  (define (flood x y prev-height)
    (let ([ h (get x y) ])
      (if (< prev-height h 9)
          (cons (list x y h)
                (append (flood x (sub1 y) h)   ; North
                        (flood (add1 x) y h)   ; East
                        (flood x (add1 y) h)   ; South
                        (flood (sub1 x) y h))) ; West
          '())))

  (match-let ([ (list x y h) tuple ])
    (remove-duplicates (flood x y (sub1 h)))))

(define (part1) (~>> (low-points)
                     (map (compose add1 third))
                     sum))

(define (part2)
  (~> (low-points)
      (map (compose length get-basin) _)
      (sort >)
      (take 3)
      product))

;; Tests --------------------------------------------------------------------------------------

(module+ test
  (require rackunit)
  (check-equal? (part1) 566)
  (check-equal? (part2) 891684))
