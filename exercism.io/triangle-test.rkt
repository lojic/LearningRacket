#lang racket

(require "triangle.rkt")

(module+ test
  (require rackunit rackunit/text-ui)

  (define suite
    (test-suite
     "Tests for the triangle exercise"

     (test-case "equilateral triangles"
                (check-equal? (triangle 2 2 2) '(ok equilateral))
                (check-equal? (triangle 10 10 10) '(ok equilateral)))
     (test-case "isosceles triangles"
                (check-equal? (triangle 3 4 4) '(ok isosceles))
                (check-equal? (triangle 4 3 4) '(ok isosceles))
                (check-equal? (triangle 4 4 3) '(ok isosceles)))
     (test-case "scalene triangles"
                (check-equal? (triangle 3 4 5) '(ok scalene))
                (check-equal? (triangle 5 4 3) '(ok scalene))
                (check-equal? (triangle 0.4 0.6 0.3) '(ok scalene)))
     (test-case "illegal triangles"
                (check-equal? (car (triangle 0 0 0)) 'error)
                (check-equal? (car (triangle 3 4 -5)) 'error)
                (check-equal? (car (triangle 1 1 3)) 'error)
                (check-equal? (car (triangle 2 4 2)) 'error))
     ))
     

  (run-tests suite))
