#lang racket

(require threading)

(define (part1 input)
  (for/sum ([ c (filter char? input) ])
    (hash-ref (hash #\) 3 #\] 57 #\} 1197 #\> 25137) c)))

(define (part2 input)
  (let* ([ incompletes (filter list? input) ]
         [ pos (floor (/ (length incompletes) 2)) ])
    (~> incompletes
        (map stack-value _)
        (sort <)
        (list-ref pos))))

(define (parse fname)
  (define (parse-line chars [ stack '() ])
    (cond [ (null? chars) stack ]
          [ else (let ([ c (car chars) ])
                   (if (is-open? c)
                       (parse-line (cdr chars) (cons c stack))
                       (if (matches? (car stack) c)
                           (parse-line (cdr chars) (drop stack 1))
                           c))) ]))

  (map (compose parse-line string->list)
       (file->lines fname)))

(define (stack-value stack)
  (for/fold ([ sum 0 ])
            ([ left stack ])
    (+ (* sum 5)
       (hash-ref (hash #\) 1 #\] 2 #\} 3 #\> 4)
                 (match-char left)))))

(define (is-open? c)   (member c '(#\( #\[ #\{ #\<)))
(define (match-char c) (hash-ref (hash #\( #\) #\[ #\] #\{ #\} #\< #\>) c))

(define (matches? left right)
  (member (cons left right)
          '((#\( . #\)) (#\[ . #\]) (#\{ . #\}) (#\< . #\>))))

;; Tests --------------------------------------------------------------------------------------

(module+ test
  (require rackunit)
  (check-equal? (part1 (parse "day10.txt")) 390993)
  (check-equal? (part2 (parse "day10.txt")) 2391385187))
