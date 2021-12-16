#lang racket

;; A simple benchmark to compare the performance of:
;; 1) using a heap
;; 2) sorting a list after each add
;; 3) inserting an item in order in a list

;; Results expressed in number of operations for roughly 100 ms:
;; heap      = 115,000
;; insertion =   6,500
;; sort      =   1,650

;; Note: random-string is from a private library

(require data/heap file/sha1 racket/random)

;; I only used this because I had it handy from another project :)
(define (random-string n)
  (let* ([ half-n       (ceiling (/ n 2))                ]
         [ random-bytes (crypto-random-bytes half-n)     ]
         [ str          (bytes->hex-string random-bytes) ])
    (if (even? n)
        str
        (substring str 0 n))))

(define input (for/list ([ _ (in-range 115000) ])
                (random-string 15)))

(define (with-heap)
  (define obj (make-heap string<?))

  (for ([ s (in-list input) ])
    (heap-add! obj s))

  (heap-min obj))

(define (with-sort)
  (let loop ([ input input ][ result '() ])
    (if (null? input)
        (car result)
        (loop (cdr input)
              (sort (cons (car input) result)
                    string<?)))))

(define (with-insertion)
  (define (insert s lst)
    (cond [ (null? lst)
            (cons s lst) ]
          [ (string<? s (car lst))
            (cons s lst) ]
          [ else
            (cons (car lst) (insert s (cdr lst))) ]))

  (let loop ([ strs input ]
             [ result '() ])
    (if (null? strs)
        (car result)
        (loop (cdr strs) (insert (car strs) result)))))

(time (with-heap))
