#lang racket

(require "allergies.rkt")

(module+ test
  (require rackunit rackunit/text-ui)

  (define suite
    (test-suite
     "Tests for the allergies exercise"

     (for ([(flags result) #hash((0 . ())
                                 (1 . ("eggs"))
                                 (2 . ("peanuts"))
                                 (8 . ("strawberries"))
                                 (3 . ("eggs" "peanuts"))
                                 (5 . ("eggs" "shellfish"))
                                 (248 . ("strawberries" "tomatoes" "chocolate" "pollen" "cats"))
                                 (509 . ("eggs" "shellfish" "strawberries" "tomatoes" "chocolate"
                                                "pollen" "cats"))
                                 )])
          (check-equal? (list->set (allergen-list flags)) (list->set result))
     )))
  
  (run-tests suite))
  
