#lang racket

(require "anagram.rkt")

(module+ test
  (require rackunit rackunit/text-ui)

  (define suite
    (test-suite
     "Tests for the anagram exercise"
     
     (test-case "no matches"
                (check-equal? (anagrams "diaper" '("hello" "world" "zombies" "pants")) '()))
     (test-case "detect simple anagram"
                (check-equal? (anagrams "ant" '("tan" "stand" "at")) '("tan")))
     (test-case "detect multiple anagrams"
                (check-equal? (anagrams "master" '("stream" "pigeon" "maters")) '("stream" "maters")))
     (test-case "do not detect anagram subsets"
                (check-equal? (anagrams "good" '("dog" "goody")) '()))
     (test-case "detect anagram"
                (check-equal? (anagrams "listen" '("enlists" "google" "inlets" "banana"))
                              '("inlets")))
     (test-case "multiple anagrams"
                (check-equal? (anagrams "allergy"
                                        (map symbol->string
                                             '(gallery ballerina regally clergy largely leading)))
                              '("gallery" "regally" "largely")))
     (test-case "anagrams must use all letters exactly once"
                (check-equal? (anagrams "patter" '("tapper")) '()))
     (test-case "detect anagrams with case-insensitive subject"
                (check-equal? (anagrams "Orchestra" '("cashregister" "Carthorse" "radishes"))
                              '("Carthorse")))
     (test-case "detect anagrams with case-insensitive candidate"
                (check-equal? (anagrams "orchestra" '("cashregister" "Carthorse" "radishes"))
                              '("Carthorse")))
     (test-case "anagrams must not be the source word"
                (check-equal? (anagrams "corn" '("corn" "dark" "Corn" "rank" "CORN" "cron" "park"))
                              '("cron")))
     (test-case "do not detect words based on checksum"
                (check-equal? (anagrams "mass" '("last")) '()))
     ))
  
  (run-tests suite))
  
