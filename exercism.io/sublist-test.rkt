#lang racket

(require "sublist.rkt")

(module+ test
  (require rackunit rackunit/text-ui)

  (define huge-list (range 1000000))
  
  (define suite
    (test-suite
     "Tests for the sublist exercise"
     
     (test-case "empty equals empty"
                (check-equal? (compare '() '()) 'equal))
     (test-case "empty is a sublist of anything"
                (check-equal? (compare '() '(())) 'sublist))
     (test-case "anything is a superlist of empty"
                (check-equal? (compare '(()) '()) 'superlist))
     (test-case "1 is not 2"
                (check-equal? (compare '(1) '(2)) 'unequal))
     (test-case "comparing massive equal lists"
                (check-equal? (compare huge-list huge-list) 'equal))
     (test-case "sublist at start"
                (check-equal? (compare '(1 2 3) '(1 2 3 4 5)) 'sublist))
     (test-case "sublist in middle"
                (check-equal? (compare '(4 3 2) '(5 4 3 2 1)) 'sublist))
     (test-case "sublist at end"
                (check-equal? (compare '(3 4 5) '(1 2 3 4 5)) 'sublist))
     (test-case "partially matching sublist at start"
                (check-equal? (compare '(1 1 2) '(1 1 1 2)) 'sublist))
     (test-case "sublist early in huge list"
                (check-equal? (compare '(3 4 5) huge-list) 'sublist))
     (test-case "huge sublist not in huge list for sublist"
                (check-equal? (compare (range 10 1000001) (range 1 1000000)) 'unequal))
     (test-case "superlist at start"
                (check-equal? (compare '(1 2 3 4 5) '(1 2 3)) 'superlist))
     (test-case "superlist in middle"
                (check-equal? (compare '(5 4 3 2 1) '(4 3 2)) 'superlist))
     (test-case "superlist at end"
                (check-equal? (compare '(1 2 3 4 5) '(3 4 5)) 'superlist))
     (test-case "partially matching superlist at start"
                (check-equal? (compare '(1 1 1 2) '(1 1 2)) 'superlist))
     (test-case "superlist early in huge list"
                (check-equal? (compare huge-list '(3 4 5)) 'superlist))
     (test-case "strict equality for sublist"
                (check-equal? (compare '(1) '(1.0 2)) 'unequal))
     (test-case "strict equality for superlist"
                (check-equal? (compare '(1.0 2) '(1)) 'unequal))
     (test-case "recurring values sublist"
                (check-equal? (compare '(1 2 1 2 3) '(1 2 3 1 2 1 2 3 2 1)) 'sublist))
     (test-case "recurring values unequal sublist"
                (check-equal? (compare '(1 2 1 2 3) '(1 2 3 1 2 3 2 3 2 1)) 'unequal))
     (test-case "recurring values unequal superlist"
                (check-equal? (compare '(1 2 3 1 2 3 2 3 2 1) '(1 2 1 2 3)) 'unequal))
     (test-case "not a sublist"
                (check-equal? (compare '(3 4 5) '(1 2 3 4)) 'unequal))
     (test-case "not a superlist"
                (check-equal? (compare '(5 4 3 2 1) '(4 3 8)) 'unequal))
     ))
  
  (run-tests suite))
  
