#lang racket

;; Without comments

(struct field (key name is-required?) #:transparent)

(define fields (list (field "byr" "Birth Year"      #t)
                     (field "iyr" "Issue Year"      #t)
                     (field "eyr" "Expiration Year" #t)
                     (field "hgt" "Height"          #t)
                     (field "hcl" "Hair Color"      #t)
                     (field "ecl" "Eye Color"       #t)
                     (field "pid" "Passport ID"     #t)
                     (field "cid" "Country ID"      #f)))

(define (count-valid-passports fname)
  (call-with-input-file fname
    (λ (in)
      (let loop ([ passport (read-passport in) ][ num-valid 0 ])
        (if (eof-object? passport)
            num-valid
            (loop (read-passport in)
                  (if (passport-is-valid? passport)
                      (add1 num-valid)
                      num-valid)))))))

(define (get-field key)
  (findf (λ (f)
           (string=? key (field-key f)))
         fields))

(define (parse-passport tokens)
  (let loop ([ tokens tokens ][ passport (passport) ])
    (if (null? tokens)
        passport
        (loop (cdr tokens) (parse-token passport (car tokens))))))

(define (parse-token passport token)
  (match token
    [(list key data) (if (get-field key)
                         (passport-set passport key data)
                         passport) ]
    [ _ (error "Invalid token") ]))

(define (passport)                       (hash))
(define (passport? obj)                  (hash? obj))
(define (passport-get passport key)      (hash-ref passport key #f))
(define (passport-set passport key data) (hash-set passport key data))

(define (passport-is-valid? passport)
  (andmap (λ (f)
            (non-empty-string? (passport-get passport (field-key f))))
          (filter (λ (f) (field-is-required? f))
                  fields)))

(define (read-passport in)
  (let ([ str (read-passport-str in) ])
    (if (eof-object? str)
        eof
        (parse-passport (tokenize str)))))

(define (read-passport-str in)
  (let loop ([ line (read-line in) ][ result eof ])
    (if (eof-object? line)
        result
        (let ([ line (string-trim line) ])
          (if (non-empty-string? line)
              (loop (read-line in) (if (eof-object? result) line (string-append result " " line)))
              result)))))

(define (tokenize str) (map (λ (s) (string-split s ":")) (string-split str)))

(module+ main
  (printf "Number of valid passports is ~a\n" (count-valid-passports "day04.txt")))

(module+ test
  (require rackunit)

  ;; ------------------------------------------------------------------------------------------
  ;; get-field
  ;; ------------------------------------------------------------------------------------------
  (let ([ f (get-field "eyr") ])
    (check-equal? (field-key f) "eyr")
    (check-equal? (field-name f) "Expiration Year")
    (check-not-false (field-is-required? f)))

  (check-false (get-field "foo"))

  ;; ------------------------------------------------------------------------------------------
  ;; parse-passport
  ;; ------------------------------------------------------------------------------------------

  (let ([ passport (parse-passport
                    '(("ecl" "gry")("eyr" "2020") ("foo" "garbage"))) ])
    (check-equal? (passport-get passport "ecl") "gry")
    (check-equal? (passport-get passport "eyr") "2020")
    (check-false (passport-get passport "foo")))

  ;; ------------------------------------------------------------------------------------------
  ;; parse-token
  ;; ------------------------------------------------------------------------------------------

  (let ([ passport (parse-token (passport) (list "byr" "foo bar")) ])
    (check-equal? (passport-get passport "byr") "foo bar"))

  (let ([ passport (parse-token (passport) (list "foo" "foo bar")) ])
    (check-false (passport-get passport "foo")))

  ;; ------------------------------------------------------------------------------------------
  ;; passport
  ;; ------------------------------------------------------------------------------------------

  (check-not-false (hash? (passport)))

  (let ([ passport (passport-set (passport) "foo" 7) ])
    (check-equal? (passport-get passport "foo") 7))

  ;; ------------------------------------------------------------------------------------------
  ;; read-passport-str
  ;; ------------------------------------------------------------------------------------------

  (call-with-input-file "day04-test.txt"
    (λ (in)
      (check-equal? (read-passport-str in)
                    "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd byr:1937 iyr:2017 cid:147 hgt:183cm")
      (check-equal? (read-passport-str in)
                    "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884 hcl:#cfa07d byr:1929")
      (check-equal? (read-passport-str in)
                    "hcl:#ae17e1 iyr:2013 eyr:2024 ecl:brn pid:760753108 byr:1931 hgt:179cm")
      (check-equal? (read-passport-str in)
                    "hcl:#cfa07d eyr:2025 pid:166559648 iyr:2011 ecl:brn hgt:59in")))

  ;; ------------------------------------------------------------------------------------------
  ;; tokenize
  ;; ------------------------------------------------------------------------------------------

  (let ([ tokens (tokenize "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd cid:147 hgt:183cm") ])
    (check-equal? (first tokens) (list "ecl" "gry"))
    (check-equal? (second tokens) (list "pid" "860033327"))
    (check-equal? (last tokens) (list "hgt" "183cm")))

  ;; ------------------------------------------------------------------------------------------
  ;; Solutions
  ;; ------------------------------------------------------------------------------------------

  (check-equal? (count-valid-passports "day04-test.txt") 2)
  (check-equal? (count-valid-passports "day04.txt") 256)

  )
