#lang racket

;; Ben Knoble's version showing it's not necessary to compute the sum
;; of the sliding windows :) Here's why:
;;
;; Given a list of '(a b c d ...) the sums of the sliding-3 windows are:
;; a + b + c
;; b + c + d  an increase implies d > a
;; c + d + e  an increase implies e > b
;; ...        etc.

(require "../advent/advent.rkt")

(define input (file->numbers "day01.txt"))

(define ((partn n) scans) (count < (drop-right scans n) (drop scans n)))
(define (part1) ((partn 1) input))
(define (part2) ((partn 3) input))
