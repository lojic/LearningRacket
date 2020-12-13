#lang racket

(define (parse-line line) (cons (string-ref line 0) (string->number (substring line 1))))
(define (manhattan n)     (+ (abs (real-part n)) (abs (imag-part n))))
(define input             (map parse-line (file->lines "day12.txt")))

(define (delta letter num)
  (match letter
    [ #\E num             ]
    [ #\S (- (* +i num))  ]
    [ #\W (- num)         ]
    [ #\N (* +i num)      ]
    [ _   0               ]))

(define (rotate point letter num)
  (match letter
    [ #\L (* (expt +i (/ num 90)) point) ]
    [ #\R (* (expt -i (/ num 90)) point) ]
    [ _   point                          ]))

(define (part1 pair result)
  (match-let ([ (cons letter num)    pair   ]
              [ (cons point default) result ])
    (cons (if (char=? letter #\F)
              (+ point (* default num))
              (+ point (delta letter num)))
          (rotate default letter num))))

(define (part2 pair result)
  (match-let ([ (cons letter num) pair   ]
              [ (cons point ship) result ])
    (cons (rotate (+ point (delta letter num)) letter num)
          (if (char=? letter #\F)
              (+ ship (* point num))
              ship))))

(manhattan (car (foldl part1 (cons 0 1) input)))    ; Part 1
(manhattan (cdr (foldl part2 (cons 10+i 0) input))) ; Part 2
