#lang racket

(require "atbash-cipher.rkt")

(module+ test
  (require rackunit rackunit/text-ui)

  (define suite
    (test-suite
     "Tests for the atbash-cipher exercise"

     (for ([(plain cipher) #hash(("" . "")
                                 ("a" . "9")
                                 ("9" . "a")
                                 ("z" . "k")
                                 ("0" . "j")
                                 ("Foo Bar" . "4vv89 s")
                                 ("The quick brown fox jumped over the lazy dog 123 times"
                                  .
                                  "q25tp 17z8s vnw4v m0pxu 56vo5 sq25y 9kl6v 3ihgq 1x5r")
                                 )])
          (check-equal? (encode plain) cipher)
     )))
  
  (run-tests suite))
  
