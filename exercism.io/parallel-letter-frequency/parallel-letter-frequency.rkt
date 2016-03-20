#lang racket
(require "../async-task.rkt")
(require racket/hash)
(provide frequency frequency-map)

(define (frequency texts)
  (define ch (async-task "parallel-letter-frequency.rkt" 'frequency-map texts))
  (merge (for/list ([i (in-range (length texts))])
                   (place-channel-get ch))))

(define (frequency-map ch)
  (define queue (place-channel-get ch))
  (let loop ()
    (place-channel-put queue (foldl (λ (c hsh) (hash-update hsh c add1 0))
                                    (hash)
                                    (string->list (place-channel-get queue))))
    (loop)))

(define (merge hashes)
  (foldl (λ (hsh result) (hash-union result hsh #:combine/key (λ (k v1 v2) (+ v1 v2))))
         (hash)
         hashes))
