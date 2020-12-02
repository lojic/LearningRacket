#lang racket

(require "./advent.rkt")
(require threading)

(define input-file "day02.txt")
(define password-pat #px"^(\\d+)-(\\d+)\\s+([a-z]):\\s+([a-z]+)$")

(struct spec (min max letter password) #:transparent)

(define (count-letter letter str)
  (~> (string->list str)
      (filter (λ (c) (char=? c letter)) _)
      (length _)))

(define (get-specs fname)
  (with-input-from-file fname
    (thunk
     (for/list ([ l (in-lines) ])
       l))))

(define (parse-password str)
  (match (regexp-match password-pat str)
    [ #f (error "Invalid password specification") ]
    [ (list _ min max letter password)
      (spec (string->number min)
            (string->number max)
            (string-ref letter 0)
            password) ]))

(define (password-is-valid? obj)
  (<= (spec-min obj)
      (count-letter (spec-letter obj)
                    (spec-password obj))
      (spec-max obj)))

(module+ main
  (let* ([ lines (get-specs input-file)                                ]
         [ n    (for/sum ([ line (in-list lines) ])
                  (if (password-is-valid? (parse-password line)) 1 0)) ])
    (printf "~a passwords, out of ~a, are valid.\n" n (length lines))))

(module+ test
  (require rackunit)

  ;; ------------------------------------------------------------------------------------------
  ;; count-letter
  ;; ------------------------------------------------------------------------------------------
  (check-equal? (count-letter #\d "abc") 0)
  (check-equal? (count-letter #\a "abc") 1)
  (check-equal? (count-letter #\b "abcb") 2)

  ;; ------------------------------------------------------------------------------------------
  ;; parse-password
  ;; ------------------------------------------------------------------------------------------
  (check-equal? (parse-password "1-3 a: abcde")
                (spec 1 3 #\a "abcde"))
  (check-equal? (parse-password "1-3 b: cdefg")
                (spec 1 3 #\b "cdefg"))
  (check-equal? (parse-password "2-9 c: ccccccccc")
                (spec 2 9 #\c "ccccccccc"))

  ;; ------------------------------------------------------------------------------------------
  ;; password-is-valid?
  ;; ------------------------------------------------------------------------------------------
  (for ([ obj (in-list (list (spec 1 1 #\a "b")        ; 0 vs. [1,1]
                             (spec 1 1 #\a "aa")       ; 2 vs. [1,1]
                             (spec 0 0 #\a "a")        ; 1 vs. [0,0]
                             (spec 2 3 #\a "abc"))) ]) ; 1 vs. [2,3]
    (check-false (password-is-valid? obj)))

  (for ([ obj (in-list (list (spec 1 1 #\a "a")
                             (spec 1 1 #\a "bab")
                             (spec 0 0 #\a "bc")
                             (spec 2 3 #\a "abaca")
                             (spec 2 3 #\b "abcdb")
                             )) ])
    (check-not-false (password-is-valid? obj)))

  ;; ------------------------------------------------------------------------------------------
  ;; Solution
  ;; ------------------------------------------------------------------------------------------
  (check-equal? (for/sum ([ line (get-specs input-file) ])
                  (if (password-is-valid? (parse-password line)) 1 0))
                519)

  )
