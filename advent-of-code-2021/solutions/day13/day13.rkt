#lang racket

(require threading racket/fixnum)

(struct point (x y) #:transparent)

(define (solve file-name)
  (let-values ([ (points folds) (parse file-name) ])
    (for/fold ([ points points ])
              ([ fold   folds  ])
      (fold-points fold points))))

(define (fold-points fold points)
  (define sentinel (most-positive-fixnum))
  (define axis (point (or (point-x fold) sentinel)
                      (or (point-y fold) sentinel)))

  (define (non-axis? p)
    (not (or (eqv? (point-x axis) (point-x p))
             (eqv? (point-y axis) (point-y p)))))

  (define (fold-point p)
    (define (min-point p1 p2)
      (point (min (point-x p1) (point-x p2))
             (min (point-y p1) (point-y p2))))

    (min-point p (point (- (* 2 (point-x axis)) (point-x p))
                        (- (* 2 (point-y axis)) (point-y p)))))

  (~>> (filter non-axis? points)
       (map fold-point)
       remove-duplicates))

(define (parse fname)
  (define (parse-points lines)
    (for/list ([ s lines ])
      (~>> (string-split s ",")
           (map string->number)
           (apply point))))

  (define (parse-folds lines)
    (for/list ([ s lines ])
      (match-let* ([ (list axis val) (string-split (substring s 11) "=") ]
                   [ val             (string->number val)                ])
        (if (string=? "x" axis)
            (point val #f)
            (point #f val)))))

  (let-values ([ (points folds) (splitf-at (file->lines fname) non-empty-string?) ])
    (values (parse-points points)
            (parse-folds (cdr folds)))))

(define (display-code coords)
  (for ([ y (inclusive-range 0 (apply max (map point-y coords))) ])
    (for ([ x (inclusive-range 0 (apply max (map point-x coords))) ])
      (display (or (and (member (point x y) coords) "#") " ")))
    (displayln " ")))

(display-code (solve "day13.txt"))
