#lang racket

;; Solve the Cracker Barrel Peg Board Puzzle

;; ---------------------------------------------------------------------------------------------------
;; Support code
;; ---------------------------------------------------------------------------------------------------

(define-syntax defpat
  (syntax-rules ()
    [(_ (fn pat) b1 b2 ...)
     (define fn (match-lambda [pat b1 b2 ...]))]))

(define-syntax lcomp
  (syntax-rules ()
    [(_ pat lst b1 b2 ...)
     (for/list ([elt lst])
       (match elt [pat b1 b2 ...]))]))
      
(define (lgen m n)   (range m (add1 n)))
(define (&& a b)     (and a b))

; Thanks Matthias Felleisen for these:
(define (.. m n)      (lgen m n))
(define (elem? m lst) (cons? (member m lst))) 

;; ---------------------------------------------------------------------------------------------------

(struct pos (r c) #:transparent)
(struct pos2 (p1 p2) #:transparent)
(struct move (src dst) #:transparent)

(define (is-occupied? b p)  (elem? p b))
(define (is-empty? b p)     (not (is-occupied? b p)))

;; Thanks Daniel Prager for the monotonically increasing observation
(defpat (is-pos? (pos r c))
  (<= 0 c r 4))

;; Possible moves for one position
(define (position-moves b p)
  (match-define (pos r c) p)
  ; List of pos2 where p1 is a neighbor to jump, p2 is a destination
  (define pairs (filter (λ (p2) (and (is-pos? (pos2-p1 p2)) 
                                     (is-pos? (pos2-p2 p2))))
                        (lcomp (list or oc)
                               '((-2 0) (0 2) (2 2) (2 0) (0 -2) (-2 -2))
                               (pos2 (pos (+ r (/ or 2)) (+ c (/ oc 2)))
                                     (pos (+ r or) (+ c oc))))))
  (for/list ([pair pairs]
             #:when (and (is-occupied? b (pos2-p1 pair))
                         (is-empty? b (pos2-p2 pair))))
    (move p (pos2-p2 pair))))

;; Possible moves for all positions on the board
(define (possible-moves b) 
  (append-map (λ (p) (position-moves b p)) b))
  
;; Make a move and return the new board
(define (move-peg b m)
  (match-define (move (pos sr sc) (pos dr dc)) m)
  (define neighbor (pos (/ (+ sr dr) 2) (/ (+ sc dc) 2)))
  (define pred (λ (p) (and (not (equal? p (move-src m)))
                           (not (equal? p neighbor)))))
  (cons (move-dst m) (filter pred b)))

;; Has the goal been met?
(define (goal? b p m)
  (and (equal? (length b) 1)
       (equal? (car b) p)))

;; Make moves until the goal position is met
(define (play b p moves)
  (define next-moves (possible-moves b))
  (define (try-moves lst)
    (cond [(null? lst) '()]
          [else (let* ([m (car lst)]
                       [result (play (move-peg b m) p (cons m moves))])
                  (if (null? result)
                      (try-moves (cdr lst))
                      result))]))
  (if (null? next-moves)
      (if (goal? b p moves)
          (reverse moves)
          '())
      (try-moves next-moves)))

;; Compute the initial empty position to know the goal, then solve the puzzle
(define (solve b)
  (let ([empty-pos (car (for*/list ([r [.. 0 4]]
                                    [c [.. 0 r]]
                                    #:when (is-empty? b (pos r c)))
                          (pos r c)))])
    (play b empty-pos '())))

(define board (lcomp (list a b)
                     '((1 0) (1 1) 
                       (2 0) (2 1) (2 2) 
                       (3 0) (3 1) (3 2) (3 3) 
                       (4 0) (4 1) (4 2) (4 3) (4 4))
                     (pos a b)))

(module* main #f
  (pretty-print (solve board)))


;; ---------------------------------------------------------------------------------------------------
;; Tests
;; ---------------------------------------------------------------------------------------------------
(module+ test
  (require rackunit)
  (define board '((1 0) (1 1) 
                  (2 0) (2 1) (2 2) 
                  (3 0) (3 1) (3 2) (3 3) 
                  (4 0) (4 1) (4 2) (4 3) (4 4)))
  (check-not-false (is-occupied? board '(1 1)))
  (check-false     (is-occupied? board '(1 3)))
  (check-not-false (is-empty? board '(5 0)))
  (check-false     (is-empty? board '(3 0)))
  (for ([pos (list (pos 0 0))])
    (check-not-false (is-pos? pos))))
