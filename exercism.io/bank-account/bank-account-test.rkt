#lang racket
(require "bank-account-semaphore.rkt")

(module+ test
  (require rackunit rackunit/text-ui)

  (define suite
    (test-suite
     "Tests for the bank-account exercise"

     (test-case "initial balance is 0"
                (define account (open-bank))
                (check-eq? (balance account) 0))
     (test-case "incremented and checking balance"
                (define account (open-bank))
                (check-eq? (balance account) 0)
                (update account 10)
                (check-eq? (balance account) 10))
     (test-case "amount is added to balance"
                (define account (open-bank))
                (check-eq? (balance account) 0)
                (update account 10)
                (update account 10)
                (check-eq? (balance account) 20))
     (test-case "incrementing balance from another thread then checking it from test thread"
                (define account (open-bank))
                (check-eq? (balance account) 0)
                (define child (thread (λ ()
                                        (update account 20))))
                (thread-wait child)
                (check-eq? (balance account) 20))
     (time
      (test-case "lots of threads updating the account"
                 (define account (open-bank))
                 (check-eq? (balance account) 0)
                 (define threads (let loop ([n 1000] [acc '()])
                                   (if (< n 1)
                                       acc
                                       (loop (- n 1) (cons (thread (λ () (update account n))) acc)))))
                 (for ([thread (in-list threads)])
                      (thread-wait thread))
                 (check-eq? (balance account) 500500)))
     ))
  
  (run-tests suite))
  
