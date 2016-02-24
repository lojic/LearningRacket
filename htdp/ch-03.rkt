#lang racket

(define CMperINCH 2.54)
(define INperFOOT 12)
(define FTperYARD 3)
(define YDperROD 5.5)
(define RODperFURLONG 40)
(define FURLONGperMILE 8)

(define (inches->cm in)
  (* in CMperINCH))

