#lang racket

(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))

(define input (vector 1 1 1 1 1 1 1 1 1 1 1 1 1 1))
(define idx #f)
(define   w #f)
(define   x #f)
(define   y #f)
(define   z #f)

(define (reset!)
  (set! idx 0)
  (set! w 0)
  (set! x 0)
  (set! y 0)
  (set! z 0))

(define (inp var)
  (λ ()
    (eval `(begin
             (set! ,var (vector-ref input idx))
             (set! idx (add1 idx)))
          ns)))

(define (add a b)
  (λ ()
    (eval `(set! ,a (+ ,a ,b)) ns)))

(define (mul a b)
  (λ ()
    (eval `(set! ,a (* ,a ,b)) ns)))

(define (div a b)
  (λ ()
    (eval `(set! ,a (quotient ,a ,b)) ns)))

(define (mod a b)
  (λ ()
    (eval `(set! ,a (modulo ,a ,b)) ns)))

(define (eql a b)
  (λ ()
    (eval `(set! ,a (if (= ,a ,b) 1 0)) ns)))

(define (parse file-name)
  (for/list ([ line (in-list (file->lines file-name)) ])
    (let* ([ lst  (string-split line)        ]
           [ op   (string->symbol (car lst)) ]
           [ args (map (λ (s)
                         (if (regexp-match? #px"^[-0-9]+$" s)
                             (string->number s)
                             (string->symbol s)))
                       (cdr lst)) ])
      (match op
        [ 'inp (apply inp args) ]
        [ 'add (apply add args) ]
        [ 'mul (apply mul args) ]
        [ 'div (apply div args) ]
        [ 'mod (apply mod args) ]
        [ 'eql (apply eql args) ]))))

(define (display-alu)
  (printf "idx = ~a\n" idx)
  (printf "w   = ~a\n" w)
  (printf "x   = ~a\n" x)
  (printf "y   = ~a\n" y)
  (printf "z   = ~a\n" z)
  (printf "\n"))

(define (run-program cmds)
  (reset!)
  (for ([ cmd (in-list cmds) ])
    (cmd)))

(define (solve file-name)
  (let ([ cmds (parse file-name) ])
    (set! input (vector 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1))
    (run-program cmds)
    (display-alu)))

(solve "day24-example.txt")
