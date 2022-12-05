#lang racket
(require "../advent.rkt")

(match-define (list stack-lines command-lines)
  (~> (file->string "./day05.txt")
      (string-split _ "\n\n")
      (map (λ (s) (string-split s "\n")) _)))

(define (parse-stacks stack-lines)
  (define (parse-stack lines i)
    (define (get-crate line) (string-ref line (add1 (* i 4))))

    (~> (map get-crate lines)
        (filter char-alphabetic? _)))

  (let* ([ lines (take stack-lines (sub1 (length stack-lines))) ]
         [ n     (/ (+ (string-length (first lines)) 1) 4)      ])
    (let loop ([ i 0 ][ stacks '(()) ])
      (if (>= i n)
          (reverse stacks)
          (loop (add1 i) (cons (parse-stack lines i) stacks))))))

(define (move-crates strategy stacks n from-i to-i)
  (let* ([ from   (list-ref stacks from-i)                                    ]
         [ to     (list-ref stacks to-i)                                      ]
         [ stacks (list-set stacks to-i (append (strategy (take from n)) to)) ])
    (list-set stacks from-i (drop from n))))

(define (solve strategy)
  (let loop ([ stacks (parse-stacks stack-lines) ][ commands ((curry map numbers) command-lines) ])
    (if (null? commands)
        (list->string (map car (cdr stacks)))
        (loop (apply move-crates strategy stacks (car commands)) (cdr commands)))))

(solve reverse)  ; Part 1
(solve identity) ; Part 2
