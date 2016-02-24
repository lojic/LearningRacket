#lang racket

(require racket/trace)

;; Exercise 3.2.2

(define factor1
  (λ (n)
    (let f ([n n] [i 2])
      (cond
        [(>= i n) (list n)]
        [(integer? (/ n i)) (cons i (f (/ n i) i))]
        [else (f n (+ i 1))]))))

(define (factor2 n)
  (let f ([n n] [i 2])
    (cond
      [(>= i n) (list n)]
      [(integer? (/ n i)) (cons i (f (/ n i) i))]
      [else (f n (+ i 1))])))

(define (factor3 n)
  (letrec ([f (λ (n i)
                (cond
                  [(>= i n) (list n)]
                  [(integer? (/ n i)) (cons i (f (/ n i) i))]
                  [else (f n (+ i 1))]))])
    (f n 2)))

(define (factor4 n)
  (define (f n i)
    (cond [(>= i n) (list n)]
          [(integer? (/ n i)) (cons i (f (/ n i) i))]
          [else (f n (+ i 1))]))
  (f n 2))

;; Exercise 3.2.4

(define count1 0)
(define (fib1 n)
  (let fib ([i n])
    (set! count1 (+ count1 1))
    (cond [(= i 0) 0]
          [(= i 1) 1]
          [else (+ (fib (- i 1)) (fib (- i 2)))])))

(define count2 0)
(define (fib2 n)
  (if (= n 0)
      0
      (let fib ([i n] [a1 1] [a2 0])
        (set! count2 (+ count2 1))
        (if (= i 1)
            a1
            (fib (- i 1) (+ a1 a2) a1)))))

;; Exercise 3.2.5

(define-syntax bjalet1
  (syntax-rules ()
    [(_ ((x e) ...) b1 b2 ...)
     ((λ (x ...) b1 b2 ...) e ...)]))

(define-syntax bjalet2
  (syntax-rules ()
    [(_ ((x e) ...) b1 b2 ...) 
     ((λ (x ...) b1 b2 ...) e ...)]
    [(_ name ((v e) ...) b1 b2 ...)
     (letrec ([name (λ (v ...) b1 b2 ...)]) (name e ...))]))

(define (factor2a n)
  (bjalet2 f ([n n] [i 2])
    (cond
      [(>= i n) (list n)]
      [(integer? (/ n i)) (cons i (f (/ n i) i))]
      [else (f n (+ i 1))])))

;; Exercise 3.2.7

(define (factor5 n)
  (let f ([n n] [i 2] [step 1])
    (if (> i (quotient n 2)) ; No factors are beyond n / 2
        (list n)
        (let ([x (/ n i)])
          (cond
            [(integer? x) (cons i (f x i step))]
            [else (f n (+ i step) 2)])))))

(define (factor6 n)
  (let f ([n n] [i 2] [step 1])
    (if (> i (quotient n 2)) ; No factors are beyond n / 2
        (list n)
        (if (= (gcd n i) 1)
            (f n (+ i step) 2)
            (cons i (f (/ n i) i step))))))

(let ([limit (+ 1 (expt 2 28))])
  (time (factor5 limit))
  (time (factor6 limit)))
