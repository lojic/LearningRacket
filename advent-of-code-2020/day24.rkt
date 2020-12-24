#lang racket

(define directions
  (hash "e" 1 "se" (+ 0.5 -i) "ne" (+ 0.5 +i) "w" -1 "sw" (+ -0.5 -i) "nw" (+ -0.5 +i) ))

(define (parse-input fname)
  (for/list ([ line (in-list (file->lines fname)) ])
    (map (curry hash-ref directions) (regexp-match* #px"(e|se|sw|w|nw|ne)" line))))

(define (part1 fname)
  (for/fold ([ hsh (hash) ])
            ([ dir (parse-input fname) ])
    (let ([ key (apply + dir) ])
      (if (hash-ref hsh key #f)
          (hash-remove hsh key)
          (hash-set hsh key #t)))))

(define (part2 fname num-days)
  (define (adjacent-keys key) (map (curry + key) (hash-values directions)))
  (define (all-keys keys)
    (foldl (λ (key result) (append (cons key (adjacent-keys key)) result)) '() keys))
  (define (key-set keys) (set->list (foldl (λ (key s) (set-add s key)) (set) keys)))
  (define (flip-em h0) (for/fold ([ h (hash) ])([ key (key-set (all-keys (hash-keys h0))) ])
                         (if (is-black? h0 key) (hash-set h key #t) h)))
  (define (is-black? h0 key)
    (let ([ black (hash-ref h0 key #f) ]
          [ num-black (for/sum ([ key (adjacent-keys key) ]) (if (hash-ref h0 key #f) 1 0)) ])
      (cond [ (and black (or (= num-black 0) (> num-black 2))) #f    ]
            [ (and (not black) (= num-black 2))                #t    ]
            [ else                                             black ])))
  (let loop ([ hsh (part1 fname) ][ day 0 ])
    (if (>= day num-days) hsh (loop (flip-em hsh) (add1 day)))))

(module+ test (require rackunit)
  (check-equal? (hash-count (part1 "day24.txt")) 360)
  (check-equal? (time (hash-count (part2 "day24.txt" 100))) 3924))
