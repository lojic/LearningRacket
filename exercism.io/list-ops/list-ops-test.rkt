#lang racket

(require "list-ops.rkt")

(module+ test
  (require rackunit rackunit/text-ui)

  (define normal-list '(1 3 5 7))
  (define huge-list (range 1 1000001))

  (define suite
    (test-suite
     "Tests for the list-ops exercise"
     
     (test-case "count of empty list"
                (check-equal? (count '()) 0))
     (test-case "count of normal list"
                (check-equal? (count normal-list) 4))
     (test-case "count of huge list"
                (check-equal? (count huge-list) 1000000))

     (test-case "reverse of empty list"
                (check-equal? (reverse '()) '()))
     (test-case "reverse of normal list"
                (check-equal? (reverse normal-list) '(7 5 3 1)))
     (test-case "reverse of huge list"
                (check-true (equal? (reverse huge-list) (range 1000000 0 -1))))

     (test-case "map of empty list"
                (check-equal? (map add1 '()) '()))
     (test-case "map of normal list"
                (check-equal? (map add1 normal-list) '(2 4 6 8)))
     (test-case "map of huge list"
                (check-true (equal? (map add1 huge-list)
                                    (range 2 1000002))))

     (test-case "filter empty list"
                (check-equal? (filter odd? '()) '()))
     (test-case "filter normal list"
                (check-equal? (filter odd? '(1 2 3 4)) '(1 3)))
     (test-case "filter huge list"
                (check-true (equal? (filter odd? huge-list)
                                    (range 1 1000000 2))))

     (test-case "reduce empty ist"
                (check-equal? (reduce + 0 '()) 0))
     (test-case "reduce normal list"
                (check-equal? (reduce + 0 normal-list) 16))
     (test-case "reduce huge list"
                (check-equal? (reduce + 0 huge-list) (/ (* 1000000 1000001) 2)))
     (test-case "reduce with non-commutative function"
                (check-equal? (reduce (λ (x acc) (- acc x)) 10 '(1 2 3 4)) 0))
     
     (test-case "append empty lists"
                (check-equal? (append '() '()) '()))
     (test-case "append empty and non-empty"
                (check-equal? (append '() '(1 2 3 4)) '(1 2 3 4)))
     (test-case "append non-empty and empty"
                (check-equal? (append '(1 2 3 4) '()) '(1 2 3 4)))
     (test-case "append nonempty lists"
                (check-equal? (append '(1 2 3) '(4 5)) '(1 2 3 4 5)))
     (test-case "append huge lists"
                (check-true (equal? (append (range 1 1000001) (range 1000001 2000001))
                                    (range 1 2000001))))
     
     (test-case "concat empty list of lists"
                (check-equal? (concat '()) '()))
     (test-case "concat normal list of lists"
                (check-equal? (concat '((1 2) (3) () (4 5 6))) '(1 2 3 4 5 6)))
     (test-case "concat huge list of small lists"
                (check-true (equal? (concat (map (λ (x) (list x)) (range 1 1000001)))
                                    (range 1 1000001))))
     
     (test-case "concat small list of huge lists"
                (check-true (equal?
                             (concat (map (λ (x) (range
                                                  (+ (* x 100000) 1)
                                                  (+ (* (+ x 1) 100000) 1)))
                                          (range 10)))
                             (range 1 1000001))))
     ))
  
  (time (run-tests suite)))
  
