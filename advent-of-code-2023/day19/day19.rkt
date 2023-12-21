#lang racket
(require "../advent.rkt")

(define-values (workflows ratings)
  (match-let ([ (list workflows ratings) (parse-aoc 19 string-split #:sep "\n\n") ])
    (let ([ workflows (make-immutable-hash
                       (map (λ (s)
                              (let ([ lst (regexp-match #px"^([a-z]+)\\{([^\\}]+)\\}$" s) ])
                                (cons (cadr lst) (string-split (caddr lst) ","))))
                            workflows)) ])
    (values workflows (map numbers ratings)))))

(struct state (x-min x-max m-min m-max a-min a-max s-min s-max) #:transparent)

(define (evaluate s)
  (if (string-contains? s ":")
      (let* ([ lst  (string-split s ":") ]
             [ expr (car lst)            ]
             [ key  (cadr lst)           ]
             [ var  (string-ref expr 0)  ]
             [ op   (string-ref expr 1)  ]
             [ n    (string->number (substring expr 2)) ])
        (list var op n key))
      s))

(define (split-state obj val)
  (match-let ([ (list var op n _) val ])
    (match (cons op var)
      [ (cons #\< #\x) (values (struct-copy state obj [ x-max (min (state-x-max obj) (sub1 n)) ])
                               (struct-copy state obj [ x-min (max (state-x-min obj) n) ])) ]
      [ (cons #\< #\m) (values (struct-copy state obj [ m-max (min (state-m-max obj) (sub1 n)) ])
                               (struct-copy state obj [ m-min (max (state-m-min obj) n) ])) ]
      [ (cons #\< #\a) (values (struct-copy state obj [ a-max (min (state-a-max obj) (sub1 n)) ])
                               (struct-copy state obj [ a-min (max (state-a-min obj) n) ])) ]
      [ (cons #\< #\s) (values (struct-copy state obj [ s-max (min (state-s-max obj) (sub1 n)) ])
                               (struct-copy state obj [ s-min (max (state-s-min obj) n) ])) ]
      [ (cons #\> #\x) (values (struct-copy state obj [ x-min (max (state-x-min obj) (add1 n)) ])
                               (struct-copy state obj [ x-max (min (state-x-max obj) n) ])) ]
      [ (cons #\> #\m) (values (struct-copy state obj [ m-min (max (state-m-min obj) (add1 n)) ])
                               (struct-copy state obj [ m-max (min (state-m-max obj) n) ])) ]
      [ (cons #\> #\a) (values (struct-copy state obj [ a-min (max (state-a-min obj) (add1 n)) ])
                               (struct-copy state obj [ a-max (min (state-a-max obj) n) ])) ]
      [ (cons #\> #\s) (values (struct-copy state obj [ s-min (max (state-s-min obj) (add1 n)) ])
                               (struct-copy state obj [ s-max (min (state-s-max obj) n) ])) ])))

(define (flow obj lst)
  (if (null? lst)
      '()
      (let ([ val (evaluate (car lst)) ])
        (if (string? val)
            (cond [ (string=? val "A") (list obj)                ]
                  [ (string=? val "R") '()                       ]
                  [ else (flow obj (hash-ref workflows val)) ])
            (let-values ([ (yes no) (split-state obj val) ])
              (let* ([ key  (fourth val)        ]
                     [ left (flow no (cdr lst)) ]
                     [ right (cond [ (string=? "A" key) (list yes) ]
                                   [ (string=? "R" key) '() ]
                                   [ else (flow yes (hash-ref workflows key)) ]) ])
                (append left right)))))))

(define (combos obj)
  (* (- (add1 (state-x-max obj)) (state-x-min obj))
     (- (add1 (state-m-max obj)) (state-m-min obj))
     (- (add1 (state-a-max obj)) (state-a-min obj))
     (- (add1 (state-s-max obj)) (state-s-min obj))))

(define (accepted? states rating)
  (findf (λ (obj)
           (and (<= (state-x-min obj) (first rating)  (state-x-max obj))
                (<= (state-m-min obj) (second rating) (state-m-max obj))
                (<= (state-a-min obj) (third rating)  (state-a-max obj))
                (<= (state-s-min obj) (fourth rating) (state-s-max obj))))
         states))

(define (part1)
  (let ([ states (flow (state 1 4000 1 4000 1 4000 1 4000) (hash-ref workflows "in")) ])
    (~> (filter (curry accepted? states) ratings)
        (map (curry apply +) _)
        list-sum)))
       
(define (part2)
  (~> (flow (state 1 4000 1 4000 1 4000 1 4000) (hash-ref workflows "in"))
      (map combos _)
      list-sum))

;; Tests --------------------------------------------------------------------------------------

(check-equal? (part1) 368523)
(check-equal? (part2) 124167549767307)
