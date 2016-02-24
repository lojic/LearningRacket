#lang racket

(require "grade-school.rkt")

(module+ test
  (require rackunit rackunit/text-ui)
  
  (define db (make-immutable-hash))

  (define suite
    (test-suite
     "Tests for the grade-school exercise"

     (test-case "add student"
                (define actual (add db "Aimee" 2))
                (check-equal? actual #hash((2 . ("Aimee")))))
     (test-case "add more students in same class"
                (define actual (add-multiple db '(("James" 2)
                                                  ("Blair" 2)
                                                  ("Paul" 2))))
                (check-equal? actual #hash((2 . ("Blair" "James" "Paul")))))
     (test-case "add students to different grades"
                (define actual (add-multiple db '(("Chelsea" 3) ("Logan" 7))))
                (check-equal? actual #hash((3 . ("Chelsea"))(7 . ("Logan")))))
     (test-case "get students in a grade"
                (define actual (grade
                                (add-multiple db '(("Bradley" 5)
                                                   ("Franklin" 5)
                                                   ("Jeff" 1)))
                                5))
                (check-equal? actual '("Bradley" "Franklin")))
     (test-case "get students in non-existent grade"
                (check-equal? (grade db 1) '()))
     (test-case "sort school by grade and by student name"
                (define actual (add-multiple db '(("Bart" 4)
                                                  ("Jennifer" 4)
                                                  ("Christopher" 4)
                                                  ("Kareem" 6)
                                                  ("Kyle" 3))))
                (check-equal? actual #hash((3 . ("Kyle"))
                                           (4 . ("Bart" "Christopher" "Jennifer"))
                                           (6 . ("Kareem")))))
     ))

  (run-tests suite))
