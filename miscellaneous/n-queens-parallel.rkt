#lang racket

; A parallel version of N-Queens. On my mid-2014 Macbook Pro 2.5 GHz i7,
; the time to compute all solutions to N=13 is 11 seconds vs. 31 seconds
; for the sequential version. A 2.8x speedup on 4 cores.

(provide main worker)

; Represent a position as a struct with x (file) and y (rank) members
(struct pos (x y) #:prefab)

; Indicate whether q1 is attacking q2
(define (is-attacking? q1 q2)
  (let ([q1x (pos-x q1)] [q1y (pos-y q1)] [q2x (pos-x q2)] [q2y (pos-y q2)])
    (or (= q1y q2y)
        (= (abs (- q1y q2y))
           (abs (- q1x q2x))))))

; Indicate whether the stack of positions is valid
(define (valid? stack)
  (not (ormap (curry is-attacking? (car stack)) (cdr stack))))

; Return a stack representing the next position, or #f if none exist
(define (next-position n stack)
  (cond [ (null? stack) #f ]
        [ else (match-define (pos x y) (car stack))
              (if (< y n)
                  (cons (pos x (+ y 1)) (cdr stack))
                  (next-position n (cdr stack))) ]))

; Accept a board size and partial list of moves, and return
; the next solution if one exists, or #f if not
(define (queens n stack)
  (let loop ([stack stack])
    (match-define (pos x y) (car stack))
    (cond [ (> x n) (cdr stack)                                ] ; Return solution
          [ (valid? stack) (loop (cons (pos (+ x 1) 1) stack)) ] ; Go to next file
          [ (< y n) (loop (cons (pos x (+ y 1)) (cdr stack)))  ] ; Go to next rank
          [ else (let ([next (next-position n (cdr stack))])     ; Backtrack
                  (if next (loop next) #f))                    ])))

(define (prefix-matches? prefix stack)
  (let ([plen (length prefix)]
        [slen (length stack)])
    (and (>= slen plen)
         (equal? prefix (take-right stack plen)))))

; Accept a partial list of moves (prefix), and return a list of solutions with the same prefix.
(define (solutions-for-prefix n prefix)
  (let loop ([solution (queens n prefix)] [solutions '()])
    (if (and solution (prefix-matches? prefix solution))
      (let ([stack (next-position n solution)])
        (if stack ; Also verify stack matches prefix?
            (loop (queens n stack) (cons solution solutions))
            (cons solution solutions)))
      solutions)))

; Worker Place
(define (worker ch)
  (match-define (cons n queue) (place-channel-get ch))
  (let loop ([prefix (place-channel-get queue)])
    (let ([solutions (solutions-for-prefix n prefix)])
      (place-channel-put queue solutions))
    (loop (place-channel-get queue))))

(define (main)
  (define n 13) ; Size of board
  (define num-workers 4)
  (define-values (parent child) (place-channel))

  ; Create worker places
  (for ([i (in-range num-workers)])
       (place-channel-put (dynamic-place "n-queens-parallel.rkt" 'worker)
                          (cons n child)))

  ; Place all combinations of the first two columns (files) in the queue. Workers will return
  ; the set of solutions that match the first two columns. Using the first two columns instead
  ; of just the first column increases the granularity of units of work and helps to prevent the
  ; situation where one or more workers become inactive at the end while fewer workers are
  ; finishing their work.
  ; NOTE: filtering out the invalid combinations (i.e. adding #:when (> (abs (- i j)) 1))
  ;       had negligible effect on performance, so I removed it for simplicity
  (for* ([i (in-range n)]
         [j (in-range n)])
        (place-channel-put parent (list (pos 2 (+ j 1)) (pos 1 (+ i 1)))))

  ; Collect results
  (let loop ([count 0] [num-results 0] [result '()])
    (if (< num-results (* n n))
        (let ([solutions (place-channel-get parent)])
          (loop (+ count (length solutions)) (+ num-results 1) (append solutions result)))
        count)))
