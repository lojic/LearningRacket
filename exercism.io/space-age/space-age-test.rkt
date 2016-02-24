#lang racket

(require "space-age.rkt")

(module+ test
  (require rackunit rackunit/text-ui)

  (define suite
    (test-suite
     "Tests for the space-age exercise"

     (test-case "age on Earth"
                (check-= 31.69 (age-on 'earth 1000000000) 0.005))
     (test-case "age on Mercury"
                (let ([input 2134835688])
                  (check-= 67.65 (age-on 'earth input) 0.005)
                  (check-= 280.88 (age-on 'mercury input) 0.005)))
     (test-case "age on Venus"
                (let ([input 189839836])
                  (check-= 6.02 (age-on 'earth input) 0.005)
                  (check-= 9.78 (age-on 'venus input) 0.005)))
     (test-case "age on Mars"
                (let ([input 2329871239])
                  (check-= 73.83 (age-on 'earth input) 0.005)
                  (check-= 39.25 (age-on 'mars input) 0.005)))
     (test-case "age on Jupiter"
                (let ([input 901876382])
                  (check-= 28.58 (age-on 'earth input) 0.005)
                  (check-= 2.41 (age-on 'jupiter input) 0.005)))
     (test-case "age on Saturn"
                (let ([input 3000000000])
                  (check-= 95.06 (age-on 'earth input) 0.005)
                  (check-= 3.23 (age-on 'saturn input) 0.005)))
     (test-case "age on Uranus"
                (let ([input 3210123456])
                  (check-= 101.72 (age-on 'earth input) 0.005)
                  (check-= 1.21 (age-on 'uranus input) 0.005)))
     (test-case "age on Neptune"
                (let ([input 8210123456])
                  (check-= 260.16 (age-on 'earth input) 0.005)
                  (check-= 1.58 (age-on 'neptune input) 0.005)))
     ))
     

  (run-tests suite))
