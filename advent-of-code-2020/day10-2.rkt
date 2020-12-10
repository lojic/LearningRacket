#lang racket

(define (get-input fname) (sort (file->list fname) <))

(define (get-tails prev lst [ result '() ])
  (cond [ (or (null? lst) (> (- (car lst) prev) 3)) result ]
        [ else (get-tails prev (cdr lst) (cons lst result)) ]))

(define (run prev lst [ memo (make-hash) ])
  (let ([ tails (get-tails prev lst) ])
    (if (null? tails)
        1
        (apply + (map (λ (l)
                        (let* ([ x (car l) ])
                          (or (hash-ref memo x #f) (let ([ val (run x (cdr l) memo) ])
                                                     (hash-set! memo x val)
                                                     val))))
                      (get-tails prev lst))))))

(module+ test
  (require rackunit)

  ;; get-tails --------------------------------------------------------------------------------
  (check-equal? (get-tails 0 '(1 2 3 4)) '((3 4) (2 3 4) (1 2 3 4)))
  (check-equal? (get-tails 0 '(2 3 4 5)) '((3 4 5) (2 3 4 5)))
  (check-equal? (get-tails 0 '(3 4 5 6)) '((3 4 5 6)))

  ;; run --------------------------------------------------------------------------------------
  (check-equal? (run 0 '()) 1)
  (check-equal? (run 0 '(1)) 1)
  (check-equal? (run 0 '(1 2)) 2)
  (check-equal? (run 0 '(1 2 3)) 4)
  (check-equal? (run 0 (get-input "day10-test.txt")) 8)
  (check-equal? (run 0 (get-input "day10-test2.txt")) 19208)
  (check-equal? (run 0 (get-input "day10.txt")) 193434623148032)

  )
