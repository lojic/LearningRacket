#lang racket

;; Without comments & tests

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

(define (passport) (hash))

(define (passport? obj) (hash? obj))

(define (passport-field-is-valid? passport f)
  (let ([ value (passport-get passport (field-key f)) ])
    (if value
        ((field-is-valid? f) value)
        (not (field-is-required? f)))))

(define (passport-get passport key)
  (hash-ref passport key #f))

(define (passport-is-valid? passport)
  (andmap (curry passport-field-is-valid? passport) fields))

(define (passport-set passport key data) (hash-set passport key data))

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

(define (tokenize str)
  (map (λ (s) (string-split s ":")) (string-split str)))

(module+ main
  (printf "Number of valid passports is ~a\n" (count-valid-passports "day04.txt")))
