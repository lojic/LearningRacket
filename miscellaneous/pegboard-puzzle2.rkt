;; Solve the Cracker Barrel Peg Board Puzzle
#lang racket

(require defpat/defpat)

;; ---------------------------------------------------------------------------------------------------
;; Support code
;; ---------------------------------------------------------------------------------------------------

;; Not used at the moment, a rudimentary list comprehension macro
#;(define-syntax lcomp
  (syntax-rules ()
    [(_ pat lst b1 b2 ...)
     (for/list ([elt lst])
       (match elt [pat b1 b2 ...]))]))

#;(define (&& a b)     (and a b))

;; Use Alex' defpat instead
#;(define-syntax defpat
  (syntax-rules ()
    [(_ (fn pat) b1 b2 ...)
     (define fn (match-lambda [pat b1 b2 ...]))]))

(define-struct pair-stream (v)
  #:methods gen:stream
  [(define (stream-empty? stream)
     (empty? (pair-stream-v stream)))
   (define (stream-first stream)
     (let ([ pair (first (pair-stream-v stream)) ])
       (values (car pair) (cdr pair))))
   (define (stream-rest stream)
     (pair-stream (rest (pair-stream-v stream))))])

(define (lgen m n)    (range m (add1 n)))
(define (.. m n)      (lgen m n))
(define (elem? m lst) (cons? (member m lst)))

;; ---------------------------------------------------------------------------------------------------

(define (is-occupied? b p)   (elem? p b))
(define (is-empty? b p)      (not (is-occupied? b p)))
(defpat (is-pos? (cons r c)) (<= 0 c r 4))

;; Possible moves for one position
(define (position-moves b p)
  (match-define (cons r c) p)
  (define pairs (filter (λ (move) (and (is-pos? (car move)) (is-pos? (cdr move))))
                        (for/list ([ (or oc) (pair-stream '((-2 . 0) (0 . 2) (2 . 2) (2 . 0) (0 . -2) (-2 . -2))) ])
                          (cons (cons (+ r (/ or 2)) (+ c (/ oc 2))) (cons (+ r or) (+ c oc))))))
  (for/list ([ (neighbor dst) (pair-stream pairs) ]
             #:when (and (is-occupied? b neighbor)
                         (is-empty?    b dst)))
    (cons p dst)))

;; Possible moves for all positions on the board
(define (possible-moves b) (append-map (λ (p) (position-moves b p)) b))

(defpat (move b (cons src dst))
  (match-let* ([ (cons (cons sr sc) (cons dr dc))  (cons src dst) ]
               [ neighbor (cons (/ (+ sr dr) 2) (/ (+ sc dc) 2)) ]
               [ pred     (λ (p) (and (not (equal? p src)) (not (equal? p neighbor)))) ])
    (cons dst (filter pred b))))

;; define/match is a possibility
#;(define/match (move b move)
  [(b (cons src dst))
   (match-let* ([ (cons (cons sr sc) (cons dr dc))  move ]
                [ neighbor (cons (/ (+ sr dr) 2) (/ (+ sc dc) 2)) ]
                [ pred     (λ (p) (and (not (equal? p src)) (not (equal? p neighbor)))) ])
     (cons dst (filter pred b)))])

;; Make moves until the goal position is met
(define (play b p moves)
  (define next-moves (possible-moves b))
  (define/match (try-moves lst)
    [ ('())         '() ]
    [ ((cons m ms)) (let ([ result (play (move b m) p (cons m moves)) ])
                      (if (null? result)
                          (try-moves (cdr lst))
                          result)) ])
  (if (null? next-moves)
      (if (goal? b p)
          (reverse moves)
          '())
      (try-moves next-moves)))

;; Compute the initial empty position to know the goal, then solve the puzzle
(define (solve b)
  (let ([empty-pos (car (for*/list ([r [.. 0 4]] [c [.. 0 r]] #:when (is-empty? b (cons r c))) (cons r c)))])
    (play b empty-pos '())))

(define (goal? b p) (and (equal? (length b) 1) (equal? (car b) p)))

(define board '((1 . 0) (1 . 1)
                (2 . 0) (2 . 1) (2 . 2)
                (3 . 0) (3 . 1) (3 . 2) (3 . 3)
                (4 . 0) (4 . 1) (4 . 2) (4 . 3) (4 . 4)))

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
  (for ([pos (list (cons 0 0) (cons 4 4))])
    (check-not-false (is-pos? pos)))

  )
