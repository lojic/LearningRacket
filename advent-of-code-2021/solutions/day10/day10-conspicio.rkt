#lang racket

;; This version inspired by Jack Anderson. The goal was to store the
;; configuration information for brackets in one place. I used a list
;; of tuples, with each tuple containing the following:
;;
;; (<left-bracket> <right-bracket> <part 1 value> <part 2 value>

(require threading)

(define (part1 input)
  (for/sum ([ c (filter char? input) ])
    (p1-value c)))

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
                   (if (closing-bracket c)
                       (parse-line (cdr chars) (cons c stack))
                       (if (equal? (closing-bracket (car stack)) c)
                           (parse-line (cdr chars) (drop stack 1))
                           c))) ]))
  (map (compose parse-line string->list) (file->lines fname)))

(define (stack-value stack)
  (for/fold ([ sum 0 ])
            ([ left stack ])
    (+ (* sum 5) (p2-value (closing-bracket left)))))

(define-values (closing-bracket p1-value p2-value)
  (let ([ tuple (λ (v key value)
                  (with-handlers ([ exn? (λ (e) #f) ])
                    (value (findf (λ (tuple) (equal? v (key tuple)))
                                  '((#\( #\)     3 1)
                                    (#\[ #\]    57 2)
                                    (#\{ #\}  1197 3)
                                    (#\< #\> 25137 4)))))) ])
    (values (λ (c) (tuple c first second))
            (λ (c) (tuple c second third))
            (λ (c) (tuple c second fourth)))))

;; Tests --------------------------------------------------------------------------------------

(module+ test
  (require rackunit)
  (check-equal? (part1 (parse "day10.txt")) 390993)
  (check-equal? (part2 (parse "day10.txt")) 2391385187))
