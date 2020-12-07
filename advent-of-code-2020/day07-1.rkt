#lang racket

(require threading)

;; (parse-spec str) -> (listof symbol?)
;; str : string?
;;
;; Parse a string of the form:
;; "light red bags contain 1 bright white bag, 2 muted yellow bags."
;; into a list of symbols of bag colors:
;; '(light-red bright-white muted-yellow)
;;
;; For part 1, we don't care about quantities
(define (parse-spec str)
  (~> (regexp-match* #px"([a-z]+ [a-z]+)(?= bag)" str)
      (map (compose string->symbol (λ (l) (string-join l "-")) string-split) _)))

;; (parse-specs fname) -> (and/c hash? hash-eq? immutable?)
;; fname : string?
;;
;; Read a file containing bag specs and return a hash where the key is
;; the bag color and the value is a list of bag colors that can be
;; contained.
(define (parse-specs fname)
  (make-immutable-hasheq (map parse-spec (file->lines fname))))

;; (contains? hsh outer inner) -> boolean?
;; hsh : hash?
;; outer : symbol? e.g. 'light-red
;; inner : symbol? e.g. 'bright-white
;;
;; Indicate whether the outer bag can contain (directly, or
;; indirectly) the inner bag.
(define (contains? hsh outer inner)
  (let loop ([ bags (hash-ref hsh outer) ][ seen '() ])
    (if (null? bags)
        #f
        (let ([ bag (car bags) ])
          (cond [ (eq? bag inner)                            #t                     ]
                [ (or (eq? bag 'no-other) (member bag seen)) (loop (cdr bags) seen) ]
                [ else
                  (loop (append (hash-ref hsh bag) (cdr bags)) (cons bag seen)) ])))))

;; (bags-containing hsh bag) -> (listof symbol?)
;; hsh : hash?
;; bag : symbol?
;;
;; Return a list of bag symbols that contain the specified bag.
(define (bags-containing hsh bag)
  (let loop ([ outers (hash-keys hsh) ][ result '() ])
    (if (null? outers)
        result
        (let ([ key (car outers) ])
          (loop (cdr outers) (if (contains? hsh key bag)
                                 (cons key result)
                                 result))))))

(module+ main
  (printf "The number of bags that can contain ~a is ~a\n"
          'shiny-gold
          (length (bags-containing (parse-specs "day07.txt") 'shiny-gold))))

(module+ test
  (require rackunit)

  ;; ------------------------------------------------------------------------------------------
  ;; bags-containing
  ;; ------------------------------------------------------------------------------------------
  (let ([ hsh (parse-specs "day07-test.txt") ])
    (check-equal? (bags-containing hsh 'light-red) '())
    (check-equal? (bags-containing hsh 'dark-orange) '())
    (let ([ bags (bags-containing hsh 'bright-white) ])
      (check-not-false (member 'light-red bags))
      (check-not-false (member 'dark-orange bags)))
    (let ([ bags (bags-containing hsh 'shiny-gold) ])
      (check-equal? (length bags) 4)
      (check-not-false (member 'bright-white bags))
      (check-not-false (member 'muted-yellow bags))
      (check-not-false (member 'dark-orange bags))
      (check-not-false (member 'light-red bags))))


  ;; ------------------------------------------------------------------------------------------
  ;; contains?
  ;; ------------------------------------------------------------------------------------------
  (let ([ hsh (parse-specs "day07-test.txt") ])
    (for ([ pair (in-list '((dotted-black . shiny-gold)
                            (faded-blue . shiny-gold)
                            (shiny-gold . shiny-gold))) ])
      (check-false (contains? hsh (car pair) (cdr pair))))
    (for ([ pair (in-list '((light-red . bright-white)
                            (light-red . muted-yellow)
                            (light-red . shiny-gold)
                            (light-red . faded-blue)
                            (light-red . dotted-black))) ])
      (check-not-false (contains? hsh (car pair) (cdr pair)))))

  ;; ------------------------------------------------------------------------------------------
  ;; parse-spec
  ;; ------------------------------------------------------------------------------------------
  (check-equal? (parse-spec "light red bags contain 1 bright white bag, 2 muted yellow bags.")
                '(light-red bright-white muted-yellow))
  (check-equal? (parse-spec "bright white bags contain 1 shiny gold bag.")
                '(bright-white shiny-gold))
  (check-equal? (parse-spec "faded blue bags contain no other bags.")
                '(faded-blue no-other))

  ;; ------------------------------------------------------------------------------------------
  ;; parse-spec
  ;; ------------------------------------------------------------------------------------------
  (let ([ hsh (parse-specs "day07-test.txt") ])
    (check-equal? (hash-count hsh) 9)
    (check-equal? (hash-ref hsh 'light-red)
                  '(bright-white muted-yellow))
    (check-equal? (hash-ref hsh 'bright-white)
                  '(shiny-gold))
    (check-equal? (hash-ref hsh 'faded-blue)
                  '(no-other)))

  (let ([ hsh (parse-specs "day07.txt") ])
    (check-equal? (hash-count hsh) 594)
    (check-equal? (hash-ref hsh 'vibrant-beige)
                  '(drab-lime muted-violet drab-plum shiny-silver))
    (check-equal? (hash-ref hsh 'clear-olive)
                  '(muted-gray dark-red clear-brown bright-silver))
    (check-equal? (hash-ref hsh 'wavy-turquoise)
                  '(no-other)))
  )
