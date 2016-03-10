#lang racket

; Represent a position as a struct with file and rank members
(struct pos (file rank) #:transparent)

; Indicate whether q1 is attacking q2
(define (is-attacking? q1 q2)
  (or (= (pos-rank q1) (pos-rank q2))
      (= (abs (- (pos-rank q1) (pos-rank q2)))
         (abs (- (pos-file q1) (pos-file q2))))))

; Indicate whether the stack of positions is valid
(define (valid? stack)
  (not (ormap (curry is-attacking? (car stack)) (cdr stack))))

; Return a list of positions where no queen is attacking another queen
(define (queens n)
  (let loop ([result (list (pos 1 1))])
    (match-define (pos file rank) (car result))
    (cond
      [(> file n) (reverse (cdr result))] ; Return solution
      [(valid? result) (loop (cons (pos (+ file 1) 1) result))] ; Go to next file
      [(< rank n) (loop (cons (pos file (+ rank 1)) (cdr result)))] ; Go to next rank
      [else (let inner ([result result]) ; Backtrack
              (let ([new-rank (+ (pos-rank (cadr result)) 1)])
                (if (<= new-rank n)
                    (loop (cons (pos (- (pos-file (car result)) 1) new-rank)
                                (cddr result)))
                    (inner (cdr result)))))])))

(queens 8)