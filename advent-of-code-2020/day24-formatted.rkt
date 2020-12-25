#lang racket
(require threading)

;; Nicely formatted version

(define directions (hash "ne" (+  0.5 +i) "e"  1 "se" (+  0.5 -i)
                         "nw" (+ -0.5 +i) "w" -1 "sw" (+ -0.5 -i)))

(define (parse-input fname)
  (for/list ([ line (in-list (file->lines fname)) ])
    (~> (regexp-match* #px"(e|se|sw|w|nw|ne)" line)
        (map (curry hash-ref directions) _))))

(define (part1 fname)
  (for/fold ([ hsh (hash)              ])
            ([ dir (parse-input fname) ])
    (let ([ key (apply + dir) ])
      (if (hash-ref    hsh key #f)
          (hash-remove hsh key)
          (hash-set    hsh key #t)))))

(define (part2 fname num-days)
  (define (adjacent-keys key)
    (map (curry + key)
         (hash-values directions)))

  (define (all-keys keys)
    (~> (for/fold ([ result '()            ])
                  ([ key    (in-list keys) ])
          (append (cons key (adjacent-keys key))
                  result))
        (list->set _)
        (set->list _)))

  (define (black-adjacent hsh key)
    (count (curry hash-has-key? hsh) (adjacent-keys key)))

  (define (is-black-tile? hsh key)
    (let ([ black-tile?        (hash-ref hsh key #f)    ]
          [ num-black-adjacent (black-adjacent hsh key) ])
      (cond [ (and black-tile?
                   (or (= num-black-adjacent 0)
                       (> num-black-adjacent 2))) #f ]
            [ (and (not black-tile?)
                   (= num-black-adjacent 2)) #t ]
            [ else black-tile? ])))

  (define (flip-em prev-hsh)
    (for/fold ([ next-hsh (hash) ])
              ([ key      (all-keys (hash-keys prev-hsh)) ])
      (if (is-black-tile? prev-hsh key)
          (hash-set next-hsh key #t)
          next-hsh)))

  (let loop ([ hsh (part1 fname) ][ day 0 ])
    (if (>= day num-days)
        hsh
        (loop (flip-em hsh) (add1 day)))))

(module+ test (require rackunit)
  (check-equal? (hash-count (part1 "day24.txt")) 360)
  (check-equal? (time (hash-count (part2 "day24.txt" 100))) 3924))
