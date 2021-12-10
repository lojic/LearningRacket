#lang racket

(require threading)

(define (part1 input)
  (define (value-of c)
    (match c
      [ #\)     3 ]
      [ #\]    57 ]
      [ #\}  1197 ]
      [ #\> 25137 ]))

  (for/sum ([ c (filter corrupt? input) ])
    (value-of c)))

(define (part2 input)
  (let* ([ incompletes (filter incomplete? input) ]
         [ midpoint    (floor (/ (length incompletes)
                                 2)) ])
    (~> (map stack-value incompletes)
        (sort <)
        (list-ref midpoint))))

(define (parse fname)
  (define (parse-line chars [ stack '() ])
    (if (empty? chars)
        stack
        (let ([ c (first chars) ])
          (cond [ (is-open-bracket? c)
                  (parse-line (rest chars) (push c stack)) ]
                [ (is-bracket-pair? (top stack) c)
                  (parse-line (rest chars) (pop stack)) ]
                [ else c ]))))

  (map (compose parse-line string->list)
       (file->lines fname)))

(define (stack-value stack)
  (define (value-of close-bracket)
    (match close-bracket
      [ #\) 1 ]
      [ #\] 2 ]
      [ #\} 3 ]
      [ #\> 4 ]))

  (for/fold ([ sum 0 ])
            ([ open-bracket stack ])
    (+ (* sum 5)
       (value-of (closing-bracket-for open-bracket)))))

(define (closing-bracket-for c)
  (match c
    [ #\( #\) ]
    [ #\[ #\] ]
    [ #\{ #\} ]
    [ #\< #\> ]
    [ _   #f  ]))

(define (is-bracket-pair? left right)
  (char=? (closing-bracket-for left) right))

;; Aliases ------------------------------------------------------------------------------------

(define corrupt?         char?)
(define incomplete?      list?)
(define is-open-bracket? closing-bracket-for)
(define pop              (λ (s) (drop s 1)))
(define push             cons)
(define top              car)

;; Tests --------------------------------------------------------------------------------------

(module+ test
  (require rackunit)
  (check-equal? (part1 (parse "day10.txt")) 390993)
  (check-equal? (part2 (parse "day10.txt")) 2391385187))
