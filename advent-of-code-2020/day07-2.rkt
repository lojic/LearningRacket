#lang racket

(require (only-in "./advent.rkt" pair-stream))
(require threading)

(define (parse-spec str)
  (define color->symbol (compose string->symbol (λ (l) (string-join l "-")) string-split))

  (define (parse-bag str)
    (if (string=? "no other bags" str)
        #f
        (let ([ m (regexp-match #px"^[ ]?(\\d+) ([a-z]+ [a-z]+)(?: bags?)" str) ])
          (cons (string->number (second m)) (color->symbol (third m))))))

  (let ([ m (regexp-match #px"^([a-z]+ [a-z]+)(?: bags contain )(.+)" str) ])
    (cons (color->symbol (second m))
          (filter identity (map parse-bag (string-split (third m) #px"[,.]"))))))

(define (parse-specs fname) (make-immutable-hasheq (map parse-spec (file->lines fname))))

(define (count-bags hsh key)
  (for/sum ([ (n color) (pair-stream (hash-ref hsh key)) ])
    (+ n (* n (count-bags hsh color)))))

(module+ main
  (printf "A ~a bag contains a total of ~a bags within it\n"
          'shiny-gold
          (count-bags (parse-specs "day07.txt") 'shiny-gold)))

(module+ test
  (require rackunit)

  ;; ------------------------------------------------------------------------------------------
  ;; count-bags
  ;; ------------------------------------------------------------------------------------------
  (let ([ hsh (parse-specs "day07-test.txt") ])
    (check-equal? (count-bags hsh 'faded-blue) 0)
    (check-equal? (count-bags hsh 'dotted-black) 0)
    (check-equal? (count-bags hsh 'vibrant-plum) 11)
    (check-equal? (count-bags hsh 'dark-olive) 7)
    (check-equal? (count-bags hsh 'shiny-gold) 32))

  (let ([ hsh (parse-specs "day07-test2.txt") ])
    (check-equal? (count-bags hsh 'shiny-gold) 126))

  (let ([ hsh (parse-specs "day07.txt") ])
    (check-equal? (count-bags hsh 'shiny-gold) 13264))

  ;; ------------------------------------------------------------------------------------------
  ;; parse-spec
  ;; ------------------------------------------------------------------------------------------
  (check-equal? (parse-spec "light red bags contain 1 bright white bag, 2 muted yellow bags.")
                '(light-red (1 . bright-white) (2 . muted-yellow)))
  (check-equal? (parse-spec "bright white bags contain 1 shiny gold bag.")
                '(bright-white (1 . shiny-gold)))
  (check-equal? (parse-spec "faded blue bags contain no other bags.")
                '(faded-blue))

  ;; ------------------------------------------------------------------------------------------
  ;; parse-spec
  ;; ------------------------------------------------------------------------------------------

  (let ([ hsh (parse-specs "day07-test.txt") ])
    (check-equal? (hash-count hsh) 9)
    (check-equal? (hash-ref hsh 'light-red)
                  '((1 . bright-white) (2 . muted-yellow)))
    (check-equal? (hash-ref hsh 'bright-white)
                  '((1 . shiny-gold)))
    (check-equal? (hash-ref hsh 'faded-blue)
                  '()))

  (let ([ hsh (parse-specs "day07.txt") ])
    (check-equal? (hash-count hsh) 594)
    (check-equal? (hash-ref hsh 'vibrant-beige)
                  '((4 . drab-lime) (1 . muted-violet) (5 . drab-plum) (5 . shiny-silver)))
    (check-equal? (hash-ref hsh 'clear-olive)
                  '((2 . muted-gray) (2 . dark-red) (5 . clear-brown) (5 . bright-silver)))
    (check-equal? (hash-ref hsh 'wavy-turquoise)
                  '()))

  )
