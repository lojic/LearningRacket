#lang racket

;; ----------------------------------------------------------------------
;; Problem 5: Smallest multiple
;; https://projecteuler.net/problem=5

;; 2520 is the smallest number that can be divided by each of the
;; numbers from 1 to 10 without any remainder.

;; What is the smallest positive number that is evenly divisible by
;; all of the numbers from 1 to 20?

;; Answer: 232792560
;; ----------------------------------------------------------------------

;; This solution simply folds the lcm (least common multiple) function
;; over the specified list of integers.

;; The lcm of 1 & 2 is 2
;;            2 & 3 is 6
;;            6 & 4 is 12
;;            12 & 5 is 60
;;            60 & 6 is 60
;;            60 & 7 is 420
;;            420 & 8 is 840
;;            840 & 9 is 2,520
;;            2,520 & 10 is 2,520
;;            2,520 & 11 is 27,720
;;            27,720 & 12 is 27,720
;;            27,720 & 13 is 360,360
;;            360,360 & 14 is 360,360
;;            360,360 & 15 is 360,360
;;            360,360 & 16 is 720,720
;;            720,720 & 17 is 12,252,240
;;            12,252,240 & 18 is 12,252,240
;;            12,252,240 & 19 is 232,792,560
;;            232,792,560 & 20 is 232,792,560

(display
 (foldl lcm 1 (range 2 21)))
