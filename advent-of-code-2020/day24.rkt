#lang racket

(define directions
  (hash "e" 1 "se" (+ 0.5 -i) "ne" (+ 0.5 +i) "w" -1 "sw" (+ -0.5 -i) "nw" (+ -0.5 +i) ))

(define (parse-input fname)
  (for/list ([ line (in-list (file->lines fname)) ])
    (map (curry hash-ref directions) (regexp-match* #px"(e|se|sw|w|nw|ne)" line))))

(define (part1 fname)
  (foldl (λ (dir hsh)
           (let ([ key (apply + dir) ])
             (if (hash-ref hsh key #f)
                 (hash-remove hsh key)
                 (hash-set hsh key #t))))
         (hash)
         (parse-input fname)))

;; --------------------------------------------------------------------------------------------

(define (adjacent-keys key) (map (curry + key) (hash-values directions)))

(define (flip-em h0)
  (foldl (λ (key h) (if (is-black? h0 key) (hash-set h key #t) h))
         (hash)
         (set->list
          (foldl (λ (key s) (set-add s key))
                 (set)
                 (foldl (λ (key result) (append (cons key (adjacent-keys key)) result))
                        '()
                        (hash-keys h0))))))

(define (is-black? h0 key)
  (let ([ black (hash-ref h0 key #f) ]
        [ num-black (for/sum ([ key (adjacent-keys key) ]) (if (hash-ref h0 key #f) 1 0)) ])
    (cond [ (and black (or (= num-black 0) (> num-black 2))) #f    ]
          [ (and (not black) (= num-black 2))                #t    ]
          [ else                                             black ])))

(define (part2 fname num-days)
  (let loop ([ hsh (part1 fname) ][ day 0 ])
    (if (>= day num-days) hsh (loop (flip-em hsh) (add1 day)))))

(module+ test (require rackunit)
  (check-equal? (hash-count (part1 "day24.txt")) 360)
  (check-equal? (time (hash-count (part2 "day24.txt" 100))) 3924))
