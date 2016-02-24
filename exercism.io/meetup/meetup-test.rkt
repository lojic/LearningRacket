#lang racket

(require "meetup.rkt")
(require gregor)

(module+ test
  (require rackunit rackunit/text-ui)

  (define suite
    (test-suite
     "Tests for the meetup exercise"

     (test-case "monteenth of may 2013"
                (check-equal? (meetup 2013 5 'monday 'teenth) (date 2013 5 13)))
     (test-case "monteenth of august 2013"
                (check-equal? (meetup 2013 8 'monday 'teenth) (date 2013 8 19)))
     (test-case "monteenth of september 2013"
                (check-equal? (meetup 2013 9 'monday 'teenth) (date 2013 9 16)))
     (test-case "tuesteenth of march 2013"
                (check-equal? (meetup 2013 3 'tuesday 'teenth) (date 2013 3 19)))
     (test-case "wednesteenth of february 2013"
                (check-equal? (meetup 2013 2 'wednesday 'teenth) (date 2013 2 13)))
     (test-case "thursteenth of september 2013"
                (check-equal? (meetup 2013 9 'thursday 'teenth) (date 2013 9 19)))
     (test-case "friteenth of april 2013"
                (check-equal? (meetup 2013 4 'friday 'teenth) (date 2013 4 19)))
     (test-case "saturteenth of february 2013"
                (check-equal? (meetup 2013 2 'saturday 'teenth) (date 2013 2 16)))
     (test-case "sunteenth of may 2013"
                (check-equal? (meetup 2013 5 'sunday 'teenth) (date 2013 5 19)))
     (test-case "first monday of march 2013"
                (check-equal? (meetup 2013 3 'monday 'first) (date 2013 3 4)))
     (test-case "first tuesday of june 2013"
                (check-equal? (meetup 2013 6 'tuesday 'first) (date 2013 6 4)))
     (test-case "second wednesday of july 2013"
                (check-equal? (meetup 2013 7 'wednesday 'second) (date 2013 7 10)))
     (test-case "third thursday of september 2013"
                (check-equal? (meetup 2013 9 'thursday 'third) (date 2013 9 19)))
     (test-case "fourth friday of december 2013"
                (check-equal? (meetup 2013 12 'friday 'fourth) (date 2013 12 27)))
     (test-case "fifth saturday of january 2016"
                (check-equal? (meetup 2016 1 'saturday 'fifth) (date 2016 1 30)))
     (test-case "last sunday of april 2013"
                (check-equal? (meetup 2013 4 'sunday 'last) (date 2013 4 28)))
     ))
     

  (run-tests suite))
