#lang racket

(require "bob.rkt")

(module+ test
  (require rackunit rackunit/text-ui)

  (define whatev "Whatever.")
  (define chill  "Whoa, chill out!")
  (define sure   "Sure.")
  (define fine   "Fine. Be that way!")
  
  (define suite
    (test-suite
     "Tests for the bob exercise"
     
     (test-case "stating something"
                (check-equal? (hey "Tom-ay-to, tom-aaah-to.") whatev))
     (test-case "shouting"
                (check-equal? (hey "WATCH OUT!") chill))
     (test-case "shouting must have a capital letter"
                (check-equal? (hey "?!@") whatev))
     (test-case "asking a question"
                (check-equal? (hey "Does this cryogenic chamber make me look fat?") sure))
     (test-case "talking forcefully"
                (check-equal? (hey "Wow!") whatev))
     (test-case "talking in capitals"
                (check-equal? (hey "This isn't shouting!") whatev))
     (test-case "shouting numbers"
                (check-equal? (hey "1, 2, 3 GO!") chill))
     (test-case "shouting with special characters"
                (check-equal? (hey "ZOMG THE %^*@#$(*^ ZOMBIES ARE COMING!!11!!1!") chill))
     (test-case "shouting with no exclamation mark"
                (check-equal? (hey "I HATE YOU") chill))
     (test-case "statement containing question mark"
                (check-equal? (hey "Ending with ? means a question.") whatev))
     (test-case "Silence"
                (check-equal? (hey "") fine))
     (test-case "Prolonged silence"
                (check-equal? (hey "   ") fine))
     (test-case "Only numbers"
                (check-equal? (hey "1, 2, 3") whatev))
     (test-case "Question with numbers"
                (check-equal? (hey "4?") sure))
     (test-case "Shouting in Russian"
                (check-equal? (hey "УХОДИ") chill))))
  
  (run-tests suite))
  
