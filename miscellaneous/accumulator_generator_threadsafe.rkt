#lang racket

(define-syntax ++
  (syntax-rules ()
    [(_ n i) (begin
               (set! n (+ n i))
               n)]))

(define (accumuator-generator n) (λ (i) (++ n i)))
(define counter (accumuator-generator 0))
(define counter-semaphore (make-semaphore 1))
(define max 100)

(define (make-worker-thread counter counter-semaphore name)
  (define (work name token)
    (displayln (format "~a thread token is ~a" name token))
    (sleep 0.005))

  (define (do-work)
    (let ([token (call-with-semaphore counter-semaphore
                                      (λ ()
                                        (let ([current-value (counter 0)])
                                          (if (< current-value max)
                                              (counter 1)
                                              (+ max 1)))))])
      (when (<= token max)
        (work name token))
      token))

  (thread (λ ()
            (let loop ([token (do-work)])
              (when (< token max)
                (loop (do-work)))))))

(define threads
  (list (make-worker-thread counter counter-semaphore "fred")
        (make-worker-thread counter counter-semaphore "wilma")
        (make-worker-thread counter counter-semaphore "barney")))

(displayln (counter 0))
(for-each thread-wait threads)
(displayln (counter 0))
