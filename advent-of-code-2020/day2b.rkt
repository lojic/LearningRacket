#lang racket

(require "./advent.rkt")

(define input-file "day2.txt")
(define password-pat #px"^(\\d+)-(\\d+)\\s+([a-z]):\\s+([a-z]+)$")

(struct spec (min max letter password) #:transparent)

(define (has-letter-at? str letter pos)
  (and (<= 1 pos (string-length str))
       (char=? letter (string-ref str (sub1 pos)))))

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
  (let ([ password (spec-password obj) ]
        [ letter   (spec-letter obj)   ])
    (xor (has-letter-at? password letter (spec-min obj))
         (has-letter-at? password letter (spec-max obj)))))

(module+ main
  (let* ([ lines (get-specs input-file)                                ]
         [ n    (for/sum ([ line (in-list lines) ])
                  (if (password-is-valid? (parse-password line)) 1 0)) ])
    (printf "~a passwords, out of ~a, are valid.\n" n (length lines))))

(module+ test
  (require rackunit)

  ;; ------------------------------------------------------------------------------------------
  ;; has-letter-at?
  ;; ------------------------------------------------------------------------------------------
  (check-false (has-letter-at? "" #\a 1))  ; Position out of range
  (check-false (has-letter-at? "a" #\a 0)) ; Position out of range
  (check-false (has-letter-at? "b" #\a 1)) ; Incorrect letter

  (check-not-false (has-letter-at? "a" #\a 1))
  (check-not-false (has-letter-at? " a" #\a 2))
  (check-not-false (has-letter-at? "abc" #\c 3))

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
  (for ([ obj (in-list (list (spec 1 1 #\a "a")        ; Both positions
                             (spec 1 1 #\a "aa")       ; Both positions
                             (spec 1 3 #\b "cdefg")    ; Neither
                             (spec 2 9 #\c "ccccccccc"); Both positions
                             (spec 0 0 #\a "a")        ; Pos out of range
                             (spec 2 3 #\a "abc"))) ]) ; Neither
    (check-false (password-is-valid? obj)))

  (for ([ obj (in-list (list (spec 1 2 #\a "a")
                             (spec 1 3 #\a "abcde")
                             (spec 1 2 #\a "bab")
                             (spec 2 3 #\a "bbaa")
                             (spec 2 3 #\b "abab")
                             )) ])
    (check-not-false (password-is-valid? obj)))

  ;; ------------------------------------------------------------------------------------------
  ;; Solution
  ;; ------------------------------------------------------------------------------------------
  (check-equal? (for/sum ([ line (get-specs input-file) ])
                  (if (password-is-valid? (parse-password line)) 1 0))
                708)

  )
