#lang racket

(define receive thread-receive)
(define send    thread-send)
(define self    current-thread)

(define (start-link)
  (cons 'ok (thread (λ () (loop '())))))

(define (loop stack)
  (match (receive)
    [ (cons 'size sender) (send sender (cons 'ok (length stack)))
                          (loop stack) ]
    [ (cons 'push item)   (loop (cons item stack)) ]
    [ (cons 'pop sender)  (let ([item (car stack)] [stack (cdr stack) ])
                            (send sender (cons 'ok item))
                            (loop stack))]))

(define (size pid)
  (send pid (cons 'size (self)))
  (cdr (receive)))

(define (push pid item)
  (send pid (cons 'push item)))

(define (pop pid)
  (send pid (cons 'pop (self)))
  (cdr (receive)))

(define (run)
  (define pid (cdr (start-link)))
  (displayln (size pid))
  (push pid 7)
  (displayln (size pid))
  (displayln (pop pid))
  (displayln (size pid)))
