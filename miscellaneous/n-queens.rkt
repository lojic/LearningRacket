#lang racket

; Represent a position as a struct with file and rank members
(struct pos (x y) #:transparent)

; Indicate whether q1 is attacking q2
(define (is-attacking? q1 q2)
  (or (= (pos-y q1) (pos-y q2))
      (= (abs (- (pos-y q1) (pos-y q2)))
         (abs (- (pos-x q1) (pos-x q2))))))

; Indicate whether the stack of positions is valid
(define (valid? stack)
  (not (ormap (curry is-attacking? (car stack)) (cdr stack))))

; Return a stack representing the next position, or #f if none exist
(define (next-position n stack)
  (cond [(null? stack) #f]
        [ else (match-define (pos x y) (car stack))
               (if (< y n)
                   (cons (pos x (+ y 1)) (cdr stack))
                   (next-position n (cdr stack)))]))

; Accept a board size and partial list of moves, and return the next solution
(define (queens n stack)
  (let loop ([stack stack])
    (match-define (pos x y) (car stack))
    (cond
      [(> x n) (cdr stack)] ; Return solution
      [(valid? stack) (loop (cons (pos (+ x 1) 1) stack))] ; Go to next file
      [(< y n) (loop (cons (pos x (+ y 1)) (cdr stack)))] ; Go to next rank
      [else (let ([next (next-position n (cdr stack))]) ;Backtrack
              (if next
                  (loop next)
                  #f))])))

; Return a list of all solutions for the specified board size
(define (main n)
  (let loop ([stack (list (pos 1 1))]
             [solutions '()])
    (if stack
        (let ([solution (queens n stack)])
          (if solution
              (loop (next-position n solution)
                    (cons solution solutions))
              solutions))
        solutions)))

(time (length (main 12)))
