#lang racket
(require "../lojic.rkt")

(displayln
 "Press C to convert from Fahrenheit to Celsius.\nPress F to convert from Celsius to Fahrenheit.")

(let-values ([(from to converter)
              (if (string=? "C" (gets "Your choice:"))
                  (values "Fahrenheit" "Celsius" (λ (f) (* (- f 32.0) 5/9)))
                  (values "Celsius" "Fahrenheit" (λ (c) (+ (* c 9/5) 32.0))))])
  (printf "The temperature in ~a is ~a."
          to
          (converter (get-num (format "Please enter the temperature in ~a" from)))))
