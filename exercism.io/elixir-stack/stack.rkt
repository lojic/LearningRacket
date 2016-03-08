#lang racket

(define receive place-channel-get)
(define send    place-channel-put)

(define (start-link)
  (cons 'ok (place ch (loop ch '()))))

(define (loop ch stack)
  (match (receive ch)
    [ (cons 'size sender) (send sender (cons 'ok (length stack)))
                          (loop ch stack) ]
    [ (cons 'push item)   (loop ch (cons item stack)) ]
    [ (cons 'pop sender)  (let ([item (car stack)] [stack (cdr stack) ])
                            (send sender (cons 'ok item))
                            (loop ch stack))]))

(define (size pid ch1 ch2)
  (send pid (cons 'size ch1))
  (cdr (receive ch2)))

(define (push pid item)
  (send pid (cons 'push item)))

(define (pop pid ch1 ch2)
  (send pid (cons 'pop ch1))
  (cdr (receive ch2)))

(define (run)
  (define pid (cdr (start-link)))
  (let-values ([(ch1 ch2) (place-channel)])
    (displayln (size pid ch1 ch2))
    (push pid 7)
    (displayln (size pid ch1 ch2))
    (displayln (pop pid ch1 ch2))
    (displayln (size pid ch1 ch2))))
