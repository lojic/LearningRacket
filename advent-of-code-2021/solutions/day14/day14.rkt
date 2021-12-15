#lang racket

(require threading "../../advent/advent.rkt")

(struct state (rules letters pairs))

(define (solve the-state iterations)
  (let* ([ result  (iterate step the-state iterations)  ]
         [ letters (hash-values (state-letters result)) ])
    (- (list-max letters)
       (list-min letters))))

(define (step the-state)
    (let* ([ rules       (state-rules the-state)               ]
           [ src-pairs   (state-pairs the-state)               ]
           [ dst-letters (hash-copy (state-letters the-state)) ]
           [ dst-pairs   (hash-copy src-pairs)                 ])
      (for ([ pair (hash-keys src-pairs) ])
        (let* ([ letter         (hash-ref rules pair)           ]
               [ pair-count     (hash-ref src-pairs pair)       ]
               [ add-pair-count (curry + pair-count)            ]
               [ sub-pair-count (curry + (- pair-count))        ]
               [ left           (~a (string-ref pair 0) letter) ]
               [ right          (~a letter (string-ref pair 1)) ])
          (hash-update! dst-letters letter add-pair-count)     ; Add letter
          (hash-update! dst-pairs   pair   sub-pair-count)     ; Consume pair
          (hash-update! dst-pairs   left   add-pair-count 0)   ; Produce left child
          (hash-update! dst-pairs   right  add-pair-count 0))) ; Produce right child

      (state rules dst-letters dst-pairs)))

(define (parse fname)
  (define (make-pairs s)
    (for/list ([ i (in-range (sub1 (string-length s))) ])
      (substring s i (+ i 2))))

  (let* ([ lines    (file->lines fname) ]
         [ template (first lines)       ])
    (state (for/hash ([ line (cddr lines) ])
             (match-let ([ (list pair element) (string-split line " -> ") ])
               (values pair (string-ref element 0))))

           (~>> (string->list template)
                (group-by identity)
                (map (λ (chars) (cons (first chars) (length chars))))
                (make-immutable-hash))

           (for/hash ([ pair (make-pairs template) ])
             (values pair 1)))))

;; Tests --------------------------------------------------------------------------------------

(module+ test
  (require rackunit)
  (let ([ the-state (parse "day14.txt") ])
    (check-equal? (solve the-state 10) 2549)
    (check-equal? (solve the-state 40) 2516901104210)))
