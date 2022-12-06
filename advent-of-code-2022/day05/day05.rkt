#lang racket
(require "../advent.rkt")

(define (parse-stacks stack-lines)
  (let* ([ lines (take stack-lines (sub1 (length stack-lines))) ]
         [ n     (/ (+ (string-length (first lines)) 1) 4)      ])
    (~> (for/list ([ line lines ])
          (for/list ([ i (in-range n) ])
            (string-ref line (add1 (* i 4)))))
        (apply zipn _)
        (map (curry filter char-alphabetic?) _)
        (cons '() _))))

(define (move-crates strategy stacks n from-i to-i)
  (let* ([ from   (list-ref stacks from-i)                                    ]
         [ to     (list-ref stacks to-i)                                      ]
         [ stacks (list-set stacks to-i (append (strategy (take from n)) to)) ])
    (list-set stacks from-i (drop from n))))

(define-values (stacks commands)
  (~> (file->string "./day05.txt")
      (string-split _ "\n\n")
      (map (λ (s) (string-split s "\n")) _)
      (spread-combine _ (list parse-stacks (curry map numbers)))))

(define (solve strategy)
  (let loop ([ stacks stacks ][ commands commands ])
    (if (null? commands)
        (list->string (map car (cdr stacks)))
        (loop (apply move-crates strategy stacks (car commands)) (cdr commands)))))

(solve reverse)  ; Part 1
(solve identity) ; Part 2
