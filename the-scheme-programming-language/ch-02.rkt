#lang racket

(define (make-queue)
  (let ([end (mcons 'ignored '())])
    (mcons end end)))

(define (putq! q v)
  (let ([end (mcons 'ignored '())])
    (set-mcar! (mcdr q) v)
    (set-mcdr! (mcdr q) end)
    (set-mcdr! q end)))

(define (getq q)
  (mcar (mcar q)))

(define (delq! q)
  (set-mcar! q (mcdr (mcar q))))

