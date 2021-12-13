#lang racket

;; This version uses ideas from:
;; https://github.com/tginsberg/advent-2021-kotlin/blob/master/src/main/kotlin/com/ginsberg/advent2021/Day13.kt

(require threading)

(struct point (x y) #:transparent)

(define (solve file-name)
  (let-values ([ (points folds) (parse file-name) ])
    (for/fold ([ points points ])
              ([ fold   folds  ])
      (for/set ([ p points ])
        (if (string=? "x" (car fold))
            (struct-copy point p [ x (fold-at (point-x p) (cdr fold)) ])
            (struct-copy point p [ y (fold-at (point-y p) (cdr fold)) ]))))))

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
            (parse-folds (cdr folds)))))

(define (display-code coords)
  (for ([ y (inclusive-range 0 (apply max (map point-y (set->list coords)))) ])
    (for ([ x (inclusive-range 0 (apply max (map point-x (set->list coords)))) ])
      (display (or (and (set-member? coords (point x y)) "#") " ")))
    (displayln " ")))

(display-code (solve "day13.txt"))
