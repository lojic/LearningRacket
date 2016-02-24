#lang racket

(require "hello-world.rkt")

(module+ test
  (require rackunit rackunit/text-ui)

  (define suite
    (test-suite
     "Tests for the hello-world exercise"

     (test-case "says hello with no name"
                (check-equal? (hello) "Hello, World!"))
     (test-case "says hello with name"
                (check-equal? (hello "Alice") "Hello, Alice!"))
     ))
     

  (run-tests suite))
