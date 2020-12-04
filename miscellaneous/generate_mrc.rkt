#lang racket

;; Generate MRC files for use in Trainer Road
;; Each tuple is: (minutes begin-percent end-percent)

; Endurance
(define intervals
  '((5 50 50)
    (5 60 60)
    (2 70 70)
    (2 60 60)
    (120 70 70)
    (3 40 40)))

; 3xN @ FTP
;(define intervals
;  '((5 50 50)
;    (4 50 65)
;    (3 65 90)
;    (1 90 110)
;    (2 50 50)
;    (0.5 50 90)
;    (0.1 125 125)
;    (0.5 50 90)
;    (0.1 125 125)
;    (0.5 50 90)
;    (0.1 125 125)
;    (3 50 50)
;    (15 100 100)
;    (5 50 50)
;    (15 100 100)
;    (5 50 50)
;    (15 100 100)
;    (5 60 45)))

;; VO2max Ladder
;(define intervals
;  '((10.0 50 75)
;    (1.0 110 110)
;    (3.0 50 50)
;    (3.0 120 120)
;    (3.0 40 40)
;    (3.5 118 118)
;    (3.5 40 40)
;    (4.0 115 115)
;    (4.0 40 40)
;    (4.5 112 112)
;    (4.5 40 40)
;    (5.0 110 110)
;    (5.0 40 40)
;    (5.5 108 108)
;    (5.0 60 45)))

;; Variety 90
;(define intervals
;  '((2.0 50 50)
;    (1.8 55 55)
;    (1.6 60 60)
;    (1.4 65 65)
;    (1.2 70 70)
;    (1.0 75 75)
;    (3.0 50 50)
;    (0.3 50 120)
;    (1.0 120 120)
;    (0.5 50 50)
;    (0.3 50 105)
;    (4 105 105)
;    (1.0 50 50)
;    (0.3 50 99)
;    (5.0 99 99)
;    (1.0 50 50)
;    (0.3 50 88)
;    (8.0 88 88)
;    (1.5 50 50)
;    (2.0 50 125)
;    (2.0 125 50)
;    (0.5 50 50)
;    (3.0 50 105)
;    (3.0 105 50)
;    (0.5 50 50)
;    (4.0 50 99)
;    (4.0 99 50)
;    (0.5 50 50)
;    (2.0 50 88)
;    (2.0 88 50)
;    (1.0 50 50)
;    (1.0 50 120)
;    (1.5 120 120)
;    (3.0 105 105)
;    (7.0 94 94)
;    (7.8 75 75)
;    (10.0 75 50)))

(define (write-footer)
  (displayln "[END COURSE DATA]"))

(define (write-header)
  (displayln "[COURSE HEADER]")
  (displayln "VERSION = 2")
  (displayln "UNITS = ENGLISH")
  (displayln "DESCRIPTION = description") ; I think Trainer Road ignores this
  (displayln "FILE NAME = file.mrc")      ; I think Trainer Road ignores this
  (displayln "MINUTES PERCENT")
  (displayln "[END COURSE HEADER]")
  (displayln "[COURSE DATA]"))

(define (write-interval interval cursor)
  (let* ([len (car interval)]
         [beg-percent (cadr interval)]
         [end-percent (caddr interval)]
         [beg-time cursor]
         [end-time (+ cursor len)])
    (displayln (format "~a\t~a" (~0.1r beg-time) beg-percent))
    (displayln (format "~a\t~a" (~0.1r end-time) end-percent))
    end-time))

(define (write-intervals intervals)
  (let loop ([intervals intervals] [cursor 0])
    (if (null? intervals)
        #f
        (loop (cdr intervals)
              (write-interval (car intervals) cursor)))))

(define (~0.1r x)
  (~r x #:precision (list '= 1)))

(define (write-mrc intervals)
  (write-header)
  (write-intervals intervals)
  (write-footer))

(write-mrc intervals)
