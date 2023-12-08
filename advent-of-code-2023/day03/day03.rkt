#lang racket

(require "../advent.rkt")

(define neighbors '(0-i 1-i 1 1+i 0+i -1+i -1 -1-i))

;; Parsing ------------------------------------------------------------------------------------

(define (parse-input tokens)
  (values (filter (compose1 string? cdr) tokens)
          (filter (compose1 char? cdr) tokens)))

(define (parse-tokens lines)
  (let next-line ([ lines lines ][ row 0 ][ tokens '() ])
    (if (null? lines)
        tokens
        (let next-token ([ line (car lines) ][ col 0 ][ tokens tokens ])
          (if (null? line)
              (next-line (cdr lines) (add1 row) tokens)
              (let-values ([ (token new-col) (parse-token line col) ])
                (next-token (drop line (- new-col col))
                            new-col
                            (cons (cons (make-rectangular col row) token) tokens))))))))

(define (parse-token line col)
  (let ([ ch (car line) ])
    (cond [ (char=? #\. ch)    (parse-dots line col) ]
          [ (char-numeric? ch) (parse-num line col)  ]
          [ else               (parse-sym line col)  ])))

(define (parse-dots line col)
  (parse-chars line col (λ (ch) (not (char=? #\. ch))) (const #f)))

(define (parse-num  line col)
  (parse-chars line col (λ (ch) (not (char-numeric? ch))) list->string))

(define (parse-sym  line col)
  (values (car line) (add1 col)))

(define (parse-chars line col pred? [ convert identity ])
  (let ([ pos (or (index-where line pred?) (length line)) ])
    (values (convert (take line pos)) (+ col pos))))

(define-values (nums syms) (parse-input (parse-tokens (parse-aoc 3 string->list))))

;; Helpers ------------------------------------------------------------------------------------

(define (adjacent? pos1 pos2)
  (ormap (λ (n) (= (+ pos1 n) pos2)) neighbors))

(define (is-part-number? num-str pos syms)
  (ormap (λ (sym-pair)
           (num-adjacent-to-sym? num-str pos (car sym-pair)))
         syms))

(define (num-adjacent-to-sym? num-str num-pos sym-pos)
  (ormap (λ (n)
           (adjacent? (+ num-pos n) sym-pos))
         (range (string-length num-str))))

(define (gear-with-nums pos)
  (~> (filter (λ (pair)
                (num-adjacent-to-sym? (cdr pair) (car pair) pos))
              nums)
      (map (compose1 string->number cdr) _)))

;; Parts --------------------------------------------------------------------------------------

(define (part1)
  (~> (filter (λ (pair)
                (is-part-number? (cdr pair) (car pair) syms))
              nums)
      (map (compose1 string->number cdr) _)
      (list-sum _)))

(define (part2)
  (~> (filter (compose1 (curry char=? #\*) cdr) syms)
      (map (compose1 gear-with-nums car) _)
      (filter (compose1 (curry = 2) length) _)
      (map list-prod _)
      list-sum))

;; Tests --------------------------------------------------------------------------------------

(check-equal? (part1) 556367)
(check-equal? (part2) 89471771)
