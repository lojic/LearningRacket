#lang racket

;; Jonathan Chan's version (lightly formatted)
(define input (map (λ (s) (read (open-input-string (format "(~a)" s)))) (file->lines "./day18.txt")))

(define (eval1 sexp)
  (match sexp
    [ `,(? number? n)        n                             ]
    [ `(,tail)               (eval1 tail)                  ]
    [ `(,rest ... + ,tail)   (+ (eval1 rest) (eval1 tail)) ]
    [ `(,rest ... * ,tail)   (* (eval1 rest) (eval1 tail)) ]))

(define (eval2 sexp)
  (match sexp
    [ `,(? number? n)                       n                                                        ]
    [ `(,tail)                              (eval2 tail)                                             ]
    [ `(,head ... ,left + ,right ,tail ...) (eval2 `(,@head ,(+ (eval2 left) (eval2 right)) ,@tail)) ]
    [ `(,head ... ,left * ,right ,tail ...) (eval2 `(,@head ,(* (eval2 left) (eval2 right)) ,@tail)) ]))

(module+ test (require rackunit)
  (check-equal? (apply + (map eval1 input)) 18213007238947)
  (check-equal? (apply + (map eval2 input)) 388966573054664))
