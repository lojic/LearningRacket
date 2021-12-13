#lang racket

;; This version uses ideas from:
;; https://github.com/tginsberg/advent-2021-kotlin/blob/master/src/main/kotlin/com/ginsberg/advent2021/Day13.kt

(require threading)

(struct point (x y) #:transparent)

(define (solve points folds)
  (for/fold ([ points points ])
            ([ fold   folds  ])
    (for/set ([ p points ])
      (if (string=? "x" (fold-type fold))
          (update-x p (fold-at (point-x p) (fold-axis fold)))
          (update-y p (fold-at (point-y p) (fold-axis fold)))))))

(define (fold-at val crease)
  (if (< val crease)
      val
      (- (* crease 2) val)))

(define (parse fname)
  (define (parse-points lines)
    (for/set ([ s lines ])
      (~>> (string-split s ",")
           (map string->number)
           (apply point))))

  (define (parse-folds lines)
    (for/list ([ s lines ])
      (match-let ([ (list axis val) (string-split (substring s 11) "=") ])
        (cons axis (string->number val)))))

  (let-values ([ (points folds) (splitf-at (file->lines fname) non-empty-string?) ])
    (values (parse-points points)
            (parse-folds (rest folds)))))

(define (display-code coords)
  (for ([ y (inclusive-range 0 (apply max (map point-y (set->list coords)))) ])
    (for ([ x (inclusive-range 0 (apply max (map point-x (set->list coords)))) ])
      (display (or (and (set-member? coords (point x y)) "#") " ")))
    (displayln " ")))

(define fold-type car)
(define fold-axis cdr)
(define update-x  (λ (p x) (struct-copy point p [ x x ])))
(define update-y  (λ (p y) (struct-copy point p [ y y ])))

(display-code (call-with-values (λ () (parse "day13.txt")) solve))
