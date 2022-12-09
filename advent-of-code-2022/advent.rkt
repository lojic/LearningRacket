#lang racket

;; Support code for the Advent of Code contest. See advent-test.rkt for tests.

(require racket/generator
         threading)

(provide (contract-out
          [ ascending-permutations-generator
            (-> exact-nonnegative-integer? list? generator?) ]
          [ atom
            (-> string? (or/c string? number?)) ]
          [ atom?
            (-> any/c boolean?) ]
          [ atoms
            (-> string? list?) ]
          [ bool-list->decimal
            (-> (listof exact-integer?) exact-integer?) ]
          [ bool-string-list->decimal
            (-> (listof string?) exact-integer?) ]
          [ chars
            (-> string? list?) ]
          [ chunk
            (-> exact-nonnegative-integer? list? list?) ]
          [ clamp
            (-> number? number? number? number?) ]
          [ csv-file->numbers
            (-> string? list?) ]
          [ digits
            (-> string? list?) ]
          [ enumerate
            (->* (list?)
                 (exact-nonnegative-integer?)
                 list?) ]
          [ filter-ascending-permutations
            (-> procedure? exact-nonnegative-integer? list? list?) ]
          [ flip
            (-> procedure? any) ]
          [ numbers
            (-> string? list?) ]
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
          [ parse-aoc
            (->* (positive-integer?)
                 (procedure?
                  #:sep        string?
                  #:head-lines exact-nonnegative-integer?
                  #:tail-lines exact-nonnegative-integer?
                  #:print-sample boolean?)
                 any) ]
          [ split-2
            (-> list? list?) ]
          [ split-at-list
            (-> list? exact-nonnegative-integer? list?) ]
          [ spread-combine
            (->* (list? list?)
                 (procedure?)
                 any) ]
          [ string-left
            (-> string? exact-nonnegative-integer? string?) ]
          [ string-index-of
            (-> string? char? (or/c exact-nonnegative-integer? #f)) ]
          [ string-right
            (-> string? exact-nonnegative-integer? string?) ]
          [ take-at-most
            (-> list? exact-nonnegative-integer? list?) ]
          [ take-right-at-most
            (-> list? exact-nonnegative-integer? list?) ]
          [ vector-sum
            (-> vector? number?) ]
          [ vector-update!
            (-> vector? exact-nonnegative-integer? procedure? any) ]
          [ windows
            (-> exact-positive-integer? list? list?) ]
          [ words
            (-> string? list?) ]
          [ zipn
            (-> list? ... list?) ]
          )
         point-add
         point-sub
         (struct-out pair-stream)
         (struct-out point)
         (all-from-out threading))

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

;; (atom str) -> (or/c number? symbol?)
;; str : string?
;;
;; Parse str into a single atom (number or string)
(define (atom str)
  (cond [ (regexp-match? #px"^-?[0-9]+(\\.[0-9]*)?$" str)
          (string->number str) ]
        [ else str ]))

;; (atom? x) -> boolean?
;; x : any/c
;;
;; Indicate whether the argument is an atom
(define (atom? x)
  (or (string? x)
      (number? x)
      (symbol? x)
      (boolean? x)
      (bytes? x)
      (char? x)))

;; (atoms str) -> list?
;; str : string?
;;
;; Return a list of all atoms (numbers or symbol name) in str.
(define (atoms str)
  (map atom
       (regexp-match* #px"[/$?a-zA-Z_0-9.+-]+" str)))

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

;; (chars str) -> list?
;; str : string?
;;
;; Return a list of all chars in str.
(define (chars str)
  (define (string->char s)
    (if (not (= (string-length s) 1))
        (error "chars: error - string not of length 1")
        (string-ref s 0)))

  (map string->char
       (regexp-match* #px"\\b[a-zA-Z_0-9.+-]+\\b" str)))

;; (chunk n lst) -> (listof list?)
;; n   : exact-nonnegative-integer?
;; lst : list?
;;
;; Return a list of chunks where each chunk is a list of n elements
;; from lst.
(define (chunk n lst)
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

;; (clamp a b val) -> number?
;; a   : number?
;; b   : number?
;; val : number?
;;
;; Limit the value of val to the interval [a, b]
(define (clamp a b val)
  (cond [ (and (real? a) (real? b) (real? val))
          ;; Fast path for all real?
          (let ([ a* (min a b) ]
                [ b* (max a b) ])
            (max a* (min b* val))) ]
        [ (real? val)
          ;; Fast path for real? val
          (let ([ a* (min (real-part a) (real-part b)) ]
                [ b* (max (real-part a) (real-part b)) ])
            (max a* (min b* val))) ]
        [ else
          ;; Assume complex?
          (make-rectangular (clamp (real-part a) (real-part b) (real-part val))
                            (clamp (imag-part a) (imag-part b) (imag-part val))) ]))
      
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

;; (digits str) -> list?
;; str : string?
;;
;; Return a list of all digits in str (as integers 0 to 9), ignoring
;; non-digit characters.
(define (digits str)
  (map string->number
       (regexp-match* #px"[0-9]" str)))

;; (enumerate lst [i]) -> list?
;; lst : list?
;; i   : exact-nonnegative-integer?
;;
;; map a list of <a> to a list of (cons <a> <i>) where <i> begins at
;; 0, by default, and is incremented for each subsequent element.
;;
;; (enumerate '(a b c)) -> '((a . 0) (b . 1) (c . 2))
(define (enumerate lst [i 0])
  (if (null? lst)
      '()
      (cons (cons (car lst) i)
            (enumerate (cdr lst) (add1 i)))))

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

;; (flip proc) -> procedure?
;; proc : procedure?
;;
;; Given a function of arity 2, return a function that accepts the 2
;; arguments in reverse order.
(define (flip fun)
  (λ (b a)
    (fun a b)))

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

;; (numbers str) -> list?
;; str : string?
;;
;; Return a list of all numbers in str, ignoring non-number characters.
(define (numbers str)
  (map string->number
       (regexp-match* #px"((?<![0-9])-)?[0-9.]+" str)))

;; (parse-aoc day parser sep print-lines) -> list?
;; day         : positive-integer?
;; parser      : (-> string? any/c)
;; sep         : string?
;; print-lines : exact-nonnegative-integer?
;;
;; Translation of Peter Norvig's Advent of Code parse function.
;;
;; * Read the input file for <day>
;; * Print out the first few lines of the file to give an idea of the
;;   file's contents
;; * Break the file into a sequence of entries separated by <sep>
;; * Apply <parser> to each entry and return the results as a list
;;   - Example parser functions include:
;;     numbers, digits, atoms, words, and built-ins such as:
;;     string->number, identity
(define (parse-aoc day [ parser identity ]
                   #:sep          [ sep "\n" ]
                   #:head-lines   [ head-lines 3 ]
                   #:tail-lines   [ tail-lines 2 ]
                   #:print-sample [ print-sample #t ])
  (let* ([ fname   (format "day~a.txt" (~r day #:min-width 2 #:pad-string "0")) ]
         [ text    (file->string fname) ]
         [ entries (map parser (~> text
                                   string-trim
                                   (string-split _ sep))) ])
    (when (and print-sample
               (or (> head-lines 0) (> tail-lines 0)))
      (print-sample-data day fname text entries head-lines tail-lines))
    entries))

;; (print-sample-data day fname text entries num-lines) -> void?
;; day         : positive-integer?
;; fname       : string?
;; text        : string?
;; entries     : list?
;; num-lines   : exact-positive-integer?
;;
;; Helper function for parse-aoc, but due to its size, it's defined at
;; the top level.  Print out the first few lines of the file to give
;; an idea of the file's contents
(define (print-sample-data day fname text entries head-lines tail-lines)
  ;; Helper -----------------------------------------------------------------------------------
  (define (print-list obj)
    (cond [ (null? obj)
            (printf "()") ]
          [ (atom? (first obj))
            (printf "~a" (trunc (format "~s" obj))) ]
          [ else
            (printf "(")
            (print-list-elements obj)
            (printf ")") ]))

  (define (print-list-elements obj)
    (let ([ len (length obj) ])
      (cond [ (= len 0)
              (void) ]
            [ (= len 1)
              (let ([ x (first obj) ])
                (if (list? x)
                    (print-list x)
                    (printf "~a" (trunc (format "~s" x))))) ]
            [ (= len 2)
              (let ([ x (first obj)  ]
                    [ y (second obj) ])
                (cond [ (list? x)
                        (print-list x)
                        (printf "\n") ]
                      [ else
                        (printf "~a" (trunc (format "~s" x))) ])
                (cond [ (list? y)
                        (print-list y) ]
                      [ else
                        (printf "~a" (trunc (format "~s" y))) ])) ]
            [ else
              (let ([ x (first obj)  ]
                    [ y (second obj) ]
                    [ z (last obj)   ])
                (cond [ (list? x)
                        (print-list x)
                        (printf "\n") ]
                      [ else
                        (printf "~a" (trunc (format "~s" x))) ])
                (cond [ (list? y)
                        (print-list y)
                        (printf "\n") ]
                      [ else
                        (printf "~a" (trunc (format "~s" y))) ])
                (printf "...\n")
                (cond [ (list? z)
                        (print-list z) ]
                      [ else
                        (printf "~a" (trunc (format "~s" z))) ])) ])))

  (define (trunc s [left 70] [right 25] [dots " ... "])
    (if (<= (string-length s)
            (+ left right (string-length dots)))
        s
        (string-append (string-left s left)
                       dots
                       (string-right s right))))
  ;; ------------------------------------------------------------------------------------------

  (let* ([ all-lines    (string-split text "\n")                  ]
         [ top-lines    (take-at-most all-lines head-lines)       ]
         [ bottom-lines (take-right-at-most all-lines tail-lines) ]
         [ head (format "~a -> ~a chars, ~a lines; first ~a lines; last ~a lines:"
                        fname
                        (string-length text)
                        (length all-lines)
                        (length top-lines)
                        (length bottom-lines)) ]
         [ dash      (make-string 100 #\-) ])
    (printf "~a\n~a\n~a\n" dash head dash)
    (for ([line (in-list top-lines) ])
      (printf "~a\n" (trunc line)))
    (when (not (or (null? top-lines) (null? bottom-lines)))
      (printf "...\n"))
    (for ([line (in-list bottom-lines) ])
      (printf "~a\n" (trunc line)))
    (printf "~a\n(parse ~a) -> ~a entries:\n" dash day (length entries))
    (printf "~a\n" dash)
    (print-list entries)
    (printf "\n~a\n"  dash)))

;; (split-2 lst) -> list?
;; lst : list?
;;
;; Split a list into two equal halves
(define (split-2 lst)
  (split-at-list lst (/ (length lst) 2)))

;; (split-at-list lst n) -> list?
;; lst : list?
;; n   : exact-nonnegative-integer?
;;
;; Like split-at, but returns a list of 2 elements, instead of 2
;; values.
(define (split-at-list lst pos)
  (let-values ([ (left right) (split-at lst pos) ])
    (list left right)))

;; (spread-combine vals funs [ comb values ]) -> any/c
;; vals : list?
;; funs : list?
;; comb : procedure?
;;
;; Applies each function in funs to the corresponding value in vals,
;; and then returns the application of comb to the results.
;;
;; For example:
;; (spread-combine '(a 7 "foo") (list symbol->string add1 string->list) list) ->
;; '("a" 8 (#\f #\o #\o))
(define (spread-combine vals funs [ comb values ])
  (apply comb (for/list ([ val vals ]
                         [ fun funs ])
                (fun val))))

;; (string-left str n) -> string?
;; str : string?
;; n   : exact-nonnegative-integer?
;;
;; Return a string consisting of the leftmost <n> characters in str.
(define (string-left str n)
  (substring str 0 n))

;; (string-index-of str c) -> (or/c exact-nonnegative-integer? #f)
;; str : string?
;; c   : char?
;;
;; Return the index of the specified character in the string.
(define (string-index-of str c)
  (let ([ len (string-length str) ])
    (let loop ([ i 0 ])
      (cond [ (>= i len)                    #f ]
            [ (char=? c (string-ref str i)) i  ]
            [ else (loop (add1 i))             ]))))

;; (string-right str n) -> string?
;; str : string?
;; n   : exact-nonnegative-integer?
;;
;; Return a string consisting of the rightmost <n> characters in str.
(define (string-right str n)
  (let ([ len (string-length str) ])
    (substring str (- len n))))

;; (take-at-most lst n) -> list?
;; lst : list?
;; n   : exact-nonnegative-integer?
;;
;; Like take, but if there are less than n elements in the list,
;; return as many as there are.
(define (take-at-most lst n)
  (if (or (null? lst) (< n 1))
      '()
      (cons (car lst) (take-at-most (cdr lst) (sub1 n)))))

;; (take-right-at-most lst n) -> list?
;; lst : list?
;; n   : exact-nonnegative-integer?
;;
;; Like take-right, but if there are less than n elements in the list,
;; return as many as there are.
(define (take-right-at-most lst n)
  (let ([ len (length lst) ])
    (if (< len n)
        lst
        (take-right lst n))))

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

;; (windows n lst) -> list?
;; n   : exact-positive-integer?
;; lst : list?
;;
;; Return a list of n-element sliding windows. For example:
;; (windows 3 '(1 2 3 4 5 6)) -> '((1 2 3) (2 3 4) (3 4 5) (4 5 6))
(define (windows n lst)
  (let ([ window (with-handlers ([ exn:fail:contract? (λ (_) #f) ])
                   (take lst n)) ])
    (if window
        (cons window (windows n (cdr lst)))
        '())))

;; (words str) -> list?
;; str : string?
;;
;; Return a list of all alphabetic words in str, ignoring non-letters.
(define (words str)
  (regexp-match* #px"[a-zA-Z]+" str))

;; (zipn . args) -> (listof list?)
;; args : (listof list?)
;;
;; Zip all of the lists together. For example,
;; (zipn '(1 2 3 4 5) '(2 3 4 5) '(3 4 5)) -> '((1 2 3) (2 3 4) (3 4 5))
(define (zipn . args)
  (let loop ([ lists args ][ result '() ])
    (cond [ (ormap empty? lists) (reverse result) ]
          [ else (loop (map rest lists) (cons (map first lists) result)) ])))
