#lang racket

;; Advent of Code 2020 Day 4 - Part 2
;;
;; Emphasizing clarity & extensibility over conciseness today. I tried
;; to make it general enough to anticipate Part 2 - time will tell how
;; that went! :)

(define (4digit-range-validator min max)
  (λ (str)
    (and (regexp-match? #px"^\\d{4}$" str)
         (<= min (string->number str) max))))

(define (pattern-validator pat) (λ (s) (regexp-match? pat s)))

(define (height-validator s)
  (let ([ lst (regexp-match #px"^(\\d+)(cm|in)$" s) ])
    (and lst
         (let ([ n     (string->number (second lst)) ]
               [ units (third lst)                   ])
           (cond [ (string=? "cm" units) (<= 150 n 193) ]
                 [ (string=? "in" units) (<= 59 n 76)   ]
                 [ else                  #f             ])))))

(struct field (key name is-required? is-valid?) #:transparent)

(define fields (list (field "byr" "Birth Year"      #t (4digit-range-validator 1920 2002))
                     (field "iyr" "Issue Year"      #t (4digit-range-validator 2010 2020))
                     (field "eyr" "Expiration Year" #t (4digit-range-validator 2020 2030))
                     (field "hgt" "Height"          #t height-validator)
                     (field "hcl" "Hair Color"      #t (pattern-validator #px"^#[0-9a-f]{6}$"))
                     (field "ecl" "Eye Color"       #t (pattern-validator #px"^amb|blu|brn|gry|grn|hzl|oth$"))
                     (field "pid" "Passport ID"     #t (pattern-validator #px"^\\d{9}$"))
                     (field "cid" "Country ID"      #f (const #t))))

;; (count-valid-passports fname) -> exact-integer?
;; fname : string?
;;
;; Compute AOC Day 4 solution given a filename.
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

;; (get-field key) -> (or/c field? #f)
;; key : string?
;;
;; Return the field associated with key, or #f if not found.
(define (get-field key)
  (findf (λ (f)
           (string=? key (field-key f)))
         fields))

;; (parse-passport tokens) -> passport?
;; tokens : (listof list?)
;;
;; Return a passport containing all of the fields from tokens.
(define (parse-passport tokens)
  (let loop ([ tokens tokens ][ passport (passport) ])
    (if (null? tokens)
        passport
        (loop (cdr tokens) (parse-token passport (car tokens))))))

;; (parse-token passport token) -> passport?
;; passport : passport?
;; token    : (listof string?)
;;
;; Update the passport with the key, data information in the token if
;; it's a valid field. Ignore invalid fields for now.
(define (parse-token passport token)
  (match token
    [(list key data) (if (get-field key)
                         (passport-set passport key data)
                         passport) ]
    [ _ (error "Invalid token") ]))

;; (passport) -> passport?
;; Create a new passport
(define (passport) (hash))

;; (passport? obj) -> boolean?
;; obj : any/c
;;
;; Indicate whether the specified obj is a passport.
(define (passport? obj) (hash? obj))

(define (passport-field-is-valid? passport f)
  (let ([ value (passport-get passport (field-key f)) ])
    (if value
        ((field-is-valid? f) value)
        (not (field-is-required? f)))))

;; (passport-get passport key) -> (or/c any #f)
;; passport : passport?
;; key : string?
;;
;; Return the data associated with key from passport, or #f if not found.
(define (passport-get passport key)
  (hash-ref passport key #f))

;; (passport-is-valid? passport) -> boolean?
;; passport : passport?
;;
;; Indicate whether the passport is valid. To be valid, the passport
;; must have data for all required fields.
(define (passport-is-valid? passport)
  (andmap (curry passport-field-is-valid? passport) fields))

;; (passport-set passport key data) -> passport?
;; passport : passport?
;; key      : string?
;; data     : string?
;;
;; Functionally update a passport and return it.
(define (passport-set passport key data) (hash-set passport key data))

;; (read-passport in) -> (or/c passport? eof-object?)
;; in : input-port?
;;
;; Read and parse a passport from the input port. Returns either a
;; passport? or eof-object?
(define (read-passport in)
  (let ([ str (read-passport-str in) ])
    (if (eof-object? str)
        eof
        (parse-passport (tokenize str)))))

;; (read-passport-str in) -> (or/c string? eof-object?)
;; in : input-port?
;;
;; Read lines from the input port until either a blank line or eof is
;; read. Return the combined lines as a single string.
(define (read-passport-str in)
  (let loop ([ line (read-line in) ][ result eof ])
    (if (eof-object? line)
        result
        (let ([ line (string-trim line) ])
          (if (non-empty-string? line)
              (loop (read-line in) (if (eof-object? result) line (string-append result " " line)))
              result)))))

;; (tokenize str) -> (listof string?)
;; str : string?
;;
;; Return a list of the form ( ("ecl" "gry") ("hcl" "#fffffd") ... )
(define (tokenize str)
  (map (λ (s) (string-split s ":")) (string-split str)))

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

  (check-equal? (count-valid-passports "day04-2-invalid.txt") 0)
  (check-equal? (count-valid-passports "day04-2-valid.txt") 4)
  (check-equal? (count-valid-passports "day04.txt") 198)

  ;; 4digit-range-validator -------------------------------------------------------------------
  (for ([ s (in-list '("" "x" "5x" "x5x" "5." ".5" "5.0" "0.5" "500" "2003")) ])
    (check-false ((4digit-range-validator 1920 2002) s)))

  (for ([ s (in-list '("1920" "2002" "1930" "2000")) ])
    (check-not-false ((4digit-range-validator 1920 2002) s)))

  ;; pattern-validator ------------------------------------------------------------------------
  (for ([ s (in-list '("" "985abc" "985abc#" "#12345" "#abcdefg")) ])
    (check-false ((pattern-validator #px"^#[0-9a-f]{6}$") s)))

  (for ([ s (in-list '("#000000" "#aaaaaa" "#abc123" "#123abc" "#06af0f")) ])
    (check-not-false ((pattern-validator #px"^#[0-9a-f]{6}$") s)))

  ;; height-validator -------------------------------------------------------------------------
  (for ([ s (in-list '("" "x" "in" "cm" "0in" "0cm" "194cm" "77in" "160 cm" "70 in")) ])
    (check-false (height-validator s)))

  (for ([ s (in-list '("150cm" "193cm" "59in" "76in" "160cm" "70in")) ])
    (check-not-false (height-validator s)))


  )
