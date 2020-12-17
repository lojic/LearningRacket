#lang racket

(require racket/generator)

(provide (contract-out [ ascending-permutations-generator
                         (-> exact-nonnegative-integer? list? generator?) ])
         (contract-out [ filter-ascending-permutations
                         (-> (-> list? boolean?) exact-nonnegative-integer? list? list?) ])
         (contract-out [ find-2
                         (-> (-> number? number? boolean?) list? (or/c pair? #f)) ])
         (struct-out pair-stream)
         iterate)

;; (ascending-permutations-generator n lst) -> generator?
;; n   : exact-nonnegative-integer?
;; lst : list?
;;
;; Returns a generator that will yield permutations.
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

;; (filter-ascending-permutations pred? n lst) -> list?
;; pred?  : (-> list? boolean?)
;; n      : exact-nonnegative-integer?
;; lst    : list?
;;
;; Return a list of permutations that satisfy the specified
;; predicate. For example, the list '(foo bar foo baz) produces the
;; following list of 2-tuple ascending permutations:
;; '((foo bar) (foo foo) (foo baz) (bar foo) (bar baz) (foo baz))
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
;; pred? : (-> number? number? boolean?)
;; lst   : list?
;;
;; Searches for two distinct numbers in a list that satisfy the
;; specified predicate. If found, returns (cons n1 n2); otherwise,
;; returns #f
(define (find-2 pred? lst)
  (if (null? lst)
      #f
      (let* ([ n1 (car lst) ]
             [ n2 (findf (curry pred? n1) (cdr lst)) ])
        (if n2
            (cons n1 n2)
            (find-2 pred? (cdr lst))))))

(define (iterate fun arg n) 
  (if (zero? n) 
      arg 
      (iterate fun (fun arg) (sub1 n))))

(define-struct pair-stream (v)
  #:methods gen:stream
  [(define (stream-empty? stream)
     (empty? (pair-stream-v stream)))
   (define (stream-first stream)
     (let ([ pair (first (pair-stream-v stream)) ])
       (values (car pair) (cdr pair))))
   (define (stream-rest stream)
     (pair-stream (rest (pair-stream-v stream))))])
