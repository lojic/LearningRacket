#lang racket
(require "../advent.rkt")

(define get-x first)
(define get-m second)
(define get-a third)
(define get-s fourth)

(define-values (workflows ratings)
  (match-let ([ (list workflows ratings) (parse-aoc 19 string-split #:sep "\n\n") ])
    (let ([ workflows (make-immutable-hash
                       (map (λ (s)
                              (let ([ lst (regexp-match #px"^([a-z]+)\\{([^\\}]+)\\}$" s) ])
                                (cons (cadr lst) (string-split (caddr lst) ","))))
                            workflows)) ])
    (values workflows (map numbers ratings)))))

workflows
ratings

(define (evaluate rating s)
  (if (string-contains? s ":")
      (let* ([ lst  (string-split s ":") ]
             [ expr (car lst)            ]
             [ key  (cadr lst)           ]
             [ get  (match (substring expr 0 1)
                      [ "x" get-x ]
                      [ "m" get-m ]
                      [ "a" get-a ]
                      [ "s" get-s ]) ]
             [ op   (match (substring expr 1 2)
                      [ "<" < ]
                      [ ">" > ]) ]
             [ n    (string->number (substring expr 2)) ])
        (if (op (get rating) n)
            key
            #f))
      s))

(define (flow lst rating)
  (let ([ s (evaluate rating (car lst)) ])
    (if s
        (cond [ (string=? "A" s) s ]
              [ (string=? "R" s) s ]
              [ else (flow (hash-ref workflows s) rating) ])
        (flow (cdr lst) rating))))

(define (accepted? rating) (equal? "A" (flow (hash-ref workflows "in") rating)))

(define (part1)
  (~> (filter accepted? ratings)
      (map (curry apply +) _)
      list-sum))

(check-equal? (part1) 368523)
