#lang racket
(require gregor)
(provide meetup)

(define (meetup year month weekday schedule)
  (select (filter-by-day-name (all-days-in year month) weekday) schedule))
  
(define (all-days-in year month)
  (map (λ (day) (date year month day))
       (range 1 (+ 1 (days-in-month year month)))))

(define (filter-by-day-name days day-name)
  (define daynums
    (make-hash (map cons '(sunday monday tuesday wednesday thursday friday saturday) (range 7))))
  (filter (λ (d) (= (->wday d) (hash-ref daynums day-name))) days))

(define (select days criteria)
  (match criteria
    [ 'first  (first days)  ]
    [ 'second (second days) ]
    [ 'third  (third days)  ]
    [ 'fourth (fourth days) ]
    [ 'fifth  (fifth days)  ]
    [ 'last   (last days)   ]
    [ 'teenth (findf (λ (day) (member (->day day) (range 13 20)))
                     days) ]))