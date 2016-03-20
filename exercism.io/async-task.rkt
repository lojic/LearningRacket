#lang racket
(provide async-task)

(define (async-task module-path fun units-of-work)
  ; 1) Create queue to send work to workers and retrieve results
  (define-values (parent child) (place-channel))

  ; 2) Create one place per core
  (for ([_ (in-range (processor-count))])
         (place-channel-put (dynamic-place module-path fun) child))

  ; 3) Write units of work to the queue in a thread
  (thread (λ ()
            (for ([work-unit (in-list units-of-work)])
                 (place-channel-put parent work-unit))))

  parent)