#lang racket
(require "../lojic.rkt")

;; Rules Engine
(define (make-leaf answer)
  (list answer '()))

(define (make-node question branches)
  (list question branches))

(define (is-leaf? node)
  (null? (cadr node)))

(define (answer leaf)
  (car leaf))

(define (question node)
  (car node))

(define (yes-node node)
  (let ([branches (cadr node)])
    (car branches)))

(define (no-node node)
  (let ([branches (cadr node)])
    (cadr branches)))

(define (run-rules node)
  (cond [ (is-leaf? node) (displayln (answer node)) ]
        [ else (if (get-boolean (question node))
                   (run-rules (yes-node node))
                   (run-rules (no-node node))) ]))

;; Rule Tree
(define rules (make-node
               "Is the car silent when you turn the key?"
               (list
                (make-node ; Yes
                 "Are the battery terminals corroded?"
                 (list
                  (make-leaf ; Yes
                   "Clean the terminals and try starting again.")
                  (make-leaf ; No
                   "Replace cables and try again.")))
                (make-node ; No
                 "Does the car make a clicking noise?"
                 (list
                  (make-leaf ; Yes
                   "Replace the battery.")
                  (make-node ; No
                   "Does the car crank up but fail to start?"
                   (list
                    (make-leaf ; Yes
                     "Check spark plug connections.")
                    (make-node ; No
                     "Does the engine start and then die?"
                     (list
                      (make-node ; Yes
                       "Does your car have fuel injection?"
                       (list
                        (make-leaf "Get it in for service.")
                        (make-leaf "Check to ensure the choke is opening and closing.")))
                      (make-leaf ; No
                       "I can't help you."))))))))))
