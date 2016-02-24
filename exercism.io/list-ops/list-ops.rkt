#lang racket

(provide append concat count filter map reduce reduce-r reverse)

(define (count list)
  (let loop ([list list] [n 0])
    (if (null? list)
        n
        (loop (cdr list) (+ n 1)))))

(define (reverse list)
  (let loop ([list list] [acc '()])
    (if (null? list)
        acc
        (loop (cdr list)
              (cons (car list) acc)))))

(define (map fun list)
  (let loop ([list list] [acc '()])
    (if (null? list)
        (reverse acc)
        (loop (cdr list) (cons (fun (car list)) acc)))))

(define (filter fun list)
  (let loop ([list list] [acc '()])
    (if (null? list)
        (reverse acc)
        (let ([h (car list)])
          (loop (cdr list) (if (fun h)
                               (cons h acc)
                               acc))))))

(define (reduce fun init list)
  (if (null? list)
      init
      (reduce fun (fun (car list) init) (cdr list))))

(define (reduce-r fun init list)
  (if (null? list)
      init
      (fun (car list) (reduce-r fun init (cdr list)))))

(define (append a b)
  (let loop ([a (reverse a)] [b b])
    (if (null? a)
        b
        (loop (cdr a) (cons (car a) b)))))

(define (concat lists)
  (reduce-r append '() lists))

