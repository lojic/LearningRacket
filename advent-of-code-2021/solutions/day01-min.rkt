#lang racket

(require "../advent/advent.rkt")

(define input (file->numbers "day01.txt"))

(define (count-increases lst)
  (car (foldl (match-lambda* [ (list n (cons count last)) (cons (+ count (if (> n last) 1 0)) n) ])
              (cons 0 (car lst))
              (cdr lst))))

(define (windows n lst)
  (let ([ window (with-handlers ([ exn:fail:contract? (λ (_) #f) ])
                   (take lst n)) ])
    (if window (cons window (windows n (cdr lst))) '())))

(define (part1) (count-increases input))
(define (part2) (count-increases (map sum (windows 3 input))))
