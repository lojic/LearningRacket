#lang racket

; Represent a position as a struct with file and rank members
(struct pos (file rank))

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
      ; If we've filled all n files, return the list of positions
      [(> file n) (reverse (cdr result))]
      ; If the current list is valid, create a new position with
      ; the next file and a rank of 1, add it to the list and loop
      [(valid? result) (loop (cons (pos (+ file 1) 1) result))]
      ; The current position is invalid, but we haven't exhausted all the ranks,
      ; so replace it with a position with the next highest rank and loop
      [(< rank n) (loop (cons (pos file (+ rank 1)) (cdr result)))]
      ; We've exhausted all the ranks in the current file, so backtrack to the previous position and
      ; increment the rank. If the rank is invalid, repeat backtracking until it's valid
      [else (let inner ([result result])
              (let ([new-rank (+ (pos-rank (cadr result)) 1)])
                (if (<= new-rank n)
                    (loop (cons (pos (- (pos-file (car result)) 1) new-rank)
                                (cddr result)))
                    (inner (cdr result)))))])))

(map (λ (p) (cons (pos-file p) (pos-rank p))) (queens 8))