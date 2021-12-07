#lang racket

;; General utility functions for use in Advent of Code

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
          [ find-2
            (-> procedure? list? (or/c pair? #f)) ]
          [ iterate
            (-> procedure? any/c exact-nonnegative-integer? any) ]
          [ sum
            (-> list? number?) ]
          [ vector-sum
            (-> vector? number?) ]
          [ vector-update!
            (-> vector? exact-nonnegative-integer? procedure? any) ]
          ;; Todo better contract for zipn
          [ zipn (-> list? ... list?) ])
         (struct-out pair-stream))

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

;; (bool-string-list->decimal lst) -> exact-integer?
;; lst : (listof string?)
;;
;; Returns the decimal value represented by the list of strings. For example:
;; (list->decimal '("1" "0" "1" "1")) -> 11
(define (bool-string-list->decimal lst) (bool-list->decimal (map string->number lst)))

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

;; (find-2 pred? lst) -> (or/c pair? #f)
;; pred? : (-> <type> <type> boolean?)
;; lst   : (listof <type>)
;;
;; Searches for two distinct elements in a list that satisfy the
;; specified predicate. If found, returns (cons e1 e2); otherwise,
;; returns #f
(define (find-2 pred? lst)
  (if (null? lst)
      #f
      (let* ([ e1 (car lst) ]
             [ e2 (findf (curry pred? e1) (cdr lst)) ])
        (if e2
            (cons e1 e2)
            (find-2 pred? (cdr lst))))))

;; (iterate fun arg n) -> <type2>
;; fun : (-> <type1> <type2>)
;; arg : <type1>
;; n   : exact-nonnegative-integer?
;;
;; Repeatedly applys fun to arg n times. For example, (iterate fun
;; 'foo 3) results in:  (fun (fun (fun arg)))
(define (iterate fun arg n)
  (if (zero? n)
      arg
      (iterate fun (fun arg) (sub1 n))))

;; (sum lst) -> number?
;; lst : (listof number?)
;;
;; Return the sum of the nubmers in the list.
(define (sum lst)
  (if (null? lst)
      0
      (+ (car lst) (sum (cdr lst)))))

;; (vector-sum v) -> number?
;; v : vector?
;;
;; Return the sum of vector elements.
(define (vector-sum v)
  (for/sum ([ i (in-range 9) ])
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

;; --------------------------------------------------------------------------------------------
;; Tests
;; --------------------------------------------------------------------------------------------

(module+ test
  (require rackunit)

  ;; ascending-permutations-generator ---------------------------------------------------------

  (let ([ g (ascending-permutations-generator 3 '(1 2 3 4 5)) ])
    (for ([ lst (in-list '((1 2 3) (1 2 4) (1 2 5) (1 3 4) (1 3 5)
                           (1 4 5) (2 3 4) (2 3 5) (2 4 5) (3 4 5))) ])
      (check-equal? (g) lst)))

  ;; bool-string-list->decimal ----------------------------------------------------------------

  (check-equal? (bool-string-list->decimal '("1" "0" "1" "1")) 11)

  ;; bool-list->decimal ----------------------------------------------------------------

  (for ([ pair (in-list '(((1 0 1 1) 11)
                          ((0 0 0) 0)
                          ((0 0 1) 1)
                          ((0 1 0) 2)
                          ((0 1 1) 3)
                          ((1 0 0) 4)
                          ((1 1 1) 7))) ])
    (check-equal? (bool-list->decimal (first pair)) (second pair)))

  ;; chunk ------------------------------------------------------------------------------------

  (check-equal? (chunk (range 15) 5)
                '((0 1 2 3 4)
                  (5 6 7 8 9)
                  (10 11 12 13 14)))

  ;; filter-ascending-permutations ------------------------------------------------------------

  (let ([ sum-is-even? (λ (lst) (even? (foldl + 0 lst))) ])
    (check-equal? (filter-ascending-permutations sum-is-even? 3 '(1 2 3 4 5))
                  '((1 2 3) (1 2 5) (1 3 4) (1 4 5) (2 3 5) (3 4 5))))

  ;; find-2 -----------------------------------------------------------------------------------

  (let ([ pred? (λ (a b)
                  (and (= 3 (string-length a))
                       (= 4 (string-length b)))) ])
    (check-false (find-2 pred? '("ab" "abcde" "a" "abcdef")))
    (check-equal? (find-2 pred? '("ab" "abcd" "a" "abc" "abcdef" "1234"))
                  (cons "abc" "1234")))

  ;; iterate ----------------------------------------------------------------------------------

  (check-equal? (iterate add1 0 3) 3)
  (check-equal? (iterate reverse '(1 2 3) 3) '(3 2 1))
  (check-equal? (iterate (λ (s) (string-append s "Z")) "" 4)
                "ZZZZ")

  ;; sum --------------------------------------------------------------------------------------

  (check-equal? (sum '(1 7 23)) 31)

  ;; pair-stream ------------------------------------------------------------------------------

  (check-equal?
   (for/sum ([ (a b) (in-stream (pair-stream '((1 . 2) (2 . 3) (3 . 4) (4 . 5)))) ])
     (* a b))
   40)

  )
