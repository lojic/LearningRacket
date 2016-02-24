#lang racket

(define-syntax bjalet
  (syntax-rules ()
    [(_ ((v e) ...) b1 b2 ...)
     ((λ (v ...) b1 b2 ...) e ...)]))

(define foo (bjalet ((a 7)) a))

foo

; Exercise 3.1.3 - had to look up answer in book :(

(define-syntax bjalet*
  (syntax-rules ()
    [(_ () b1 b2 ...)
     (let () b1 b2 ...)]
    [(_ ((v1 e1) (v2 e2) ...) b1 b2 ...)
     (let ((v1 e1))
       (bjalet* ((v2 e2) ...) b1 b2 ...))]))

(bjalet* ([a 5] [b (+ a a)] [c (+ a b)])
  (list a b c))

; Exercise 3.1.4

(define-syntax bja-when
  (syntax-rules ()
    [(_ t b1 b2 ...)
     (if t
         (begin b1 b2 ...)
         #f)]))

(bja-when (> 4 3)
          (displayln "one")
          (displayln "two"))
          
(bja-when (< 4 3)
          (displayln "one")
          (displayln "two"))

(define-syntax bja-unless
  (syntax-rules ()
    [(_ t b1 b2 ...)
     (when (not t) b1 b2 ...)]))

(bja-unless (> 3 4)
          (displayln "one")
          (displayln "two"))
            
(define (bjalist? x)
  (letrec ([race (lambda (h t)
                   (if (pair? h)
                       (let ([h (cdr h)])
                         (if (pair? h)
                             (and (not (eq? h t))
                                  (race (cdr h) (cdr t)))
                             (null? h)))
                       (null? h)))])
    (race x x)))

(define (bjalist2? x)
  (let race ([h x] [t x])
    (if (pair? h)
        (let ([h (cdr h)])
          (if (pair? h)
              (and (not (eq? h t))
                   (race (cdr h) (cdr t)))
              (null? h)))
        (null? h))))

((λ (x y) (+ x y)) 4 5)

((... <magic> ...) 7 8)
vs.
(funcall #'(... <magic> ...) 7 8)

