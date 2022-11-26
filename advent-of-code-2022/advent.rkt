#lang racket

;; Support code for the Advent of Code contest. See advent-test.rkt for tests.

(require racket/generator
         threading)

(provide (contract-out
          [ ascending-permutations-generator
            (-> exact-nonnegative-integer? list? generator?) ]
          [ bool-list->decimal
            (-> (listof exact-integer?) exact-integer?) ]
          [ bool-string-list->decimal
            (-> (listof string?) exact-integer?) ]
          [ chunk
            (-> list? exact-nonnegative-integer? list?) ]
          [ csv-file->numbers
            (-> string? list?) ]
          [ filter-ascending-permutations
            (-> procedure? exact-nonnegative-integer? list? list?) ]
          [ iterate
            (-> procedure? any/c exact-nonnegative-integer? any) ]
          [ list-max
            (-> list? number?) ]
          [ list-min
            (-> list? number?) ]
          [ list-prod
            (-> list? number?) ]
          [ list-sum
            (-> list? number?) ]
          [ vector-sum
            (-> vector? number?) ]
          [ vector-update!
            (-> vector? exact-nonnegative-integer? procedure? any) ]
          [ zipn
            (-> list? ... list?) ]
          )
         point-add
         point-sub
         (struct-out pair-stream)
         (struct-out point))

;; 3D Point
(struct point (x y z) #:transparent)

;; (point-add p1 p2) -> point?
;; p1 : point?
;; p2 : point?
;;
;; Add two points together. For example:
;; (point-add (point 1 2 3) (point 2 3 4)) -> (point 3 5 7)
(define (point-add p1 p2)
  (point (+ (point-x p1) (point-x p2))
         (+ (point-y p1) (point-y p2))
         (+ (point-z p1) (point-z p2))))

;; (point-sub p1 p2) -> point?
;; p1 : point?
;; p2 : point?
;;
;; Subtract p2 from p1. For example:
;; (point-sub (point 1 5 3) (point 2 3 4)) -> (point -1 2 -1)
(define (point-sub p1 p2)
  (point (- (point-x p1) (point-x p2))
         (- (point-y p1) (point-y p2))
         (- (point-z p1) (point-z p2))))

;; Allow streaming a list of pairs as values, e.g. for use in for
(define-struct pair-stream (v)
  #:methods gen:stream
  [(define (stream-empty? stream)
     (empty? (pair-stream-v stream)))
   (define (stream-first stream)
     (let ([ pair (first (pair-stream-v stream)) ])
       (values (car pair) (cdr pair))))
   (define (stream-rest stream)
     (pair-stream (rest (pair-stream-v stream))))])
  
;; (ascending-permutations-generator n lst) -> generator?
;; n   : exact-nonnegative-integer?
;; lst : list?
;;
;; Returns a generator that will yield permutations. For example:
;; (ascending-permutations-generator 3 '(1 2 3 4 5))
;; => '((1 2 3) (1 2 4) (1 2 5) (1 3 4) (1 3 5)
;;      (1 4 5) (2 3 4) (2 3 5) (2 4 5) (3 4 5))
(define (ascending-permutations-generator n lst)
  (generator ()
    (let loop ([ lst lst ][ n n ][ stack '() ])
      (if (= n 0)
          (yield (reverse stack))
          (if (null? lst)
              #f
              (begin
                (loop (cdr lst) (sub1 n) (cons (car lst) stack))
                (loop (cdr lst) n stack)))))))

;; (bool-list->decimal lst) -> exact-integer?
;; lst : (listof exact-integer?)
;;
;; Returns the decimal value represented by the list of boolean
;; numbers. For example:
;; (bool-list->decimal '(1 0 1 1)) -> 11
(define (bool-list->decimal lst)
  (let loop ([lst lst] [acc 0])
    (match lst [ '()        acc                              ]
               [ (cons 0 _) (loop (cdr lst) (* 2 acc))       ]
               [ (cons 1 _) (loop (cdr lst) (+ (* 2 acc) 1)) ]
               [ _          0                                ])))

;; (bool-string-list->decimal lst) -> exact-integer?
;; lst : (listof string?)
;;
;; Returns the decimal value represented by the list of strings. For example:
;; (list->decimal '("1" "0" "1" "1")) -> 11
(define (bool-string-list->decimal lst) (bool-list->decimal (map string->number lst)))

;; (chunk lst n) -> (listof list?)
;; lst : list?
;; n   : exact-nonnegative-integer?
;;
;; Return a list of chunks where each chunk is a list of n elements
;; from lst.
(define (chunk lst n)
  (define (get-chunk lst n)
    (let loop ([lst lst] [acc '()] [n n])
      (if (or (null? lst) (< n 1))
          (values (reverse acc) lst)
          (loop (cdr lst) (cons (car lst) acc) (- n 1)))))

  (let loop ([lst lst] [acc '()])
    (if (null? lst)
        (reverse acc)
        (let-values ([(chunk rest) (get-chunk lst n)])
          (loop rest (cons chunk acc))))))

;; (csv-file->numbers path) -> (listof number?)
;; path : string?
;;
;; Read a file consisting of one line of a comma delimited list of
;; numbers into a list of numbers.
(define (csv-file->numbers fname)
  (~>> (file->string fname)
       string-trim
       (string-split _ ",")
       (map string->number)))

;; (filter-ascending-permutations pred? n lst) -> list?
;; pred?  : (-> list? boolean?)
;; n      : exact-nonnegative-integer?
;; lst    : list?
;;
;; Return a list of permutations that satisfy the specified
;; predicate. For example, filtering the list of 3-tuples produced
;; from '(1 2 3 4 5) using a predicate of sum-is-even? results in:
;; '((1 2 3) (1 2 5) (1 3 4) (1 4 5) (2 3 5) (3 4 5))
(define (filter-ascending-permutations pred? n lst)
  (reverse
   (let loop ([ lst lst ][ n n ][ stack '() ][ result '() ])
     (if (= n 0)
         (let ([ s (reverse stack) ])
           (if (pred? s) (cons s result) result))
         (if (null? lst)
             result
             (loop (cdr lst)
                   n
                   stack
                   (loop (cdr lst) (sub1 n) (cons (car lst) stack) result)))))))

;; (iterate fun arg n) -> any/c
;; fun : procedure?
;; arg : any/c
;; n   : exact-nonnegative-integer?
;;
;; Repeatedly applies fun to arg n times. For example:
;; (iterate fun 'foo 3) -> (fun (fun (fun 'foo)))
(define (iterate fun arg n)
  (if (zero? n)
      arg
      (iterate fun (fun arg) (sub1 n))))

;; (list-max lst) -> number?
;; lst : (listof number?)
;;
;; Return the maximum number in the list
(define (list-max lst)
  (argmax identity lst))

;; (list-min lst) -> number?
;; lst : (listof number?)
;;
;; Return the minimum number in the list
(define (list-min lst)
  (argmin identity lst))

;; (list-prod lst) -> number?
;; lst : (listof number?)
;;
;; Return the product of the numbers in the list.
(define (list-prod lst)
  (foldl * 1 lst))

;; (list-sum lst) -> number?
;; lst : (listof number?)
;;
;; Return the sum of the numbers in the list.
(define (list-sum lst)
  (foldl + 0 lst))

;; (vector-sum v) -> number?
;; v : vector?
;;
;; Return the sum of vector elements.
(define (vector-sum v)
  (for/sum ([ i (in-range (vector-length v)) ])
    (vector-ref v i)))

;; (vector-update! vec i f) -> (void)
;; vec : vector?
;; i   : exact-nonnegative-integer?
;; f   : procedure?
;;
;; Update the ith element of vec by applying f to it
(define (vector-update! vec i f)
  (vector-set! vec i (f (vector-ref vec i))))

;; (zipn . args) -> (listof list?)
;; args : (listof list?)
;;
;; Zip all of the lists together. For example,
;; (zipn '(1 2 3 4 5) '(2 3 4 5) '(3 4 5)) -> '((1 2 3) (2 3 4) (3 4 5))
(define (zipn . args)
  (let loop ([ lists args ][ result '() ])
    (cond [ (ormap empty? lists) (reverse result) ]
          [ else (loop (map rest lists) (cons (map first lists) result)) ])))

