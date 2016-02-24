#lang racket
(require "../lojic.rkt")

(define tax-rate 0.055)
(struct line-item (quantity price total))
(define (cents x) (inexact->exact (floor (+ (* x 100.0) 0.5))))
(define (dollars x) (/ x 100.0))

; Returns line-item struct from user input
(define (get-line-item index)
  (define price (string->number (gets (format "Enter the price of item ~a:" index))))
  (define quantity (string->number (gets (format "Enter the quantity of item ~a:" index))))
  (line-item quantity price (cents (* price quantity))))

(define line-items (for/list ([idx '(1 2 3)]) (get-line-item idx)))

(define sub-total (foldl (λ (line-item sum) (+ (line-item-total line-item) sum))
                         0
                         line-items))

(define tax (cents (* tax-rate (dollars sub-total))))
(define total (+ sub-total tax))

(printf "Subtotal: ~a\n" (~0.2r (dollars sub-total)))
(printf "Tax: ~a\n"      (~0.2r (dollars tax)))
(printf "Total: ~a\n"    (~0.2r (dollars total)))




