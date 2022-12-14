#lang racket
(require "../advent.rkt")

(define in (for/list ([ str (parse-aoc 13 #:sep "\n\n") ])
             (read (~> (string-replace str #px"[,\\s]" " ")
                       (string-append "[" _ "]")
                       (open-input-string _)))))

(define (compare left right)
  (define (compare* l r)
    (cond [ (and (number? l) (number? r))
            (cond [ (< l r) 'l ]
                  [ (< r l) 'r ]
                  [ else (compare (cdr left) (cdr right)) ]) ]
          [ (and (list? l) (list? r))
            (let ([ c (compare l r) ])
              (if (eq? c 'n)
                  (compare (cdr left) (cdr right))
                  c)) ]
          [ (list? l) (compare left (cons (list r) (cdr right))) ]
          [ (list? r) (compare (cons (list l) (cdr left)) right) ]))

  (cond [ (and (null? left) (null? right))    'n ]
        [ (null? left)                        'l ]
        [ (null? right)                       'r ]
        [ else (compare* (car left) (car right)) ]))

(define (part1 in)
  (~> (map (curry apply compare) in)
      enumerate
      (filter (compose (curry eq? 'l) car) _)
      (map (compose add1 cdr) _)
      list-sum))

(define (part2 in)
  (define (pred? n) (compose (curry eq? 'r) (curry compare (list (list n)))))

  (let ([ in (apply append in) ])
    (* (+ 1 (count (pred? 2) in))
       (+ 2 (count (pred? 6) in)))))

(check-equal? (part1 in) 6484)
(check-equal? (part2 in) 19305)
