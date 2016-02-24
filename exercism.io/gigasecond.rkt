#lang racket
(require racket/date)
(provide from)

(define (from year month day)
  (let ([date (seconds->date (+ 1000000000
                                (find-seconds 0 0 0 day month year)))])
    (list (date-year date) (date-month date) (date-day date))))
