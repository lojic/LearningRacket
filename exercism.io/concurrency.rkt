#lang racket
(require racket/async-channel)
(provide define/atomic define/atomchan)

(define-syntax-rule (define/atomic (name arg ...) E ...)
  (define name
    (let ([sema (make-semaphore 1)])
      (lambda (arg ...)
        (dynamic-wind (λ () (semaphore-wait sema))
                      (λ () E ...)
                      (λ () (semaphore-post sema)))))))

(define-syntax-rule (define/atomchan (name arg ...) E ...)
  (define name
    (let ([chan (make-async-channel)])
      (async-channel-put chan #t)
      (lambda (arg ...)
        (dynamic-wind (λ () (async-channel-get chan))
                      (λ () E ...)
                      (λ () (async-channel-put chan #t)))))))
