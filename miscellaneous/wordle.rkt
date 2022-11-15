#lang racket

(require debug/repl)

;; Wordle Solver for hard mode

;; After each guess is proposed by the program, enter one of 3 letters
;; for each position:
;; p if the letter is correct for that position
;; i if the letter is included in the word at a different position
;; e if the letter is excluded from the word

;; TODO
;; Encode information if a letter is included more than once

(define (play words wordle included excluded n agent [ first-guess #f ])
  (if (> n 20)
      (error "Failed to guess the Wordle!")
      (let* ([ frequencies (make-letter-frequencies words)                              ]
             [ guess       (if (and first-guess (= n 1))
                               first-guess
                               (guess-word words wordle frequencies included excluded)) ])
        (let-values ([ (words wordle included excluded)
                       (update-state words wordle included excluded guess (agent guess)) ])
          (if (solved? wordle)
              n
              (play words wordle included excluded (add1 n) agent))))))

(define (human-agent guess)
  (printf "Guess: ~a\n" guess)
  (printf "Feedback: ")
  (read-line))

(define ((bot-agent word) guess)
  (let ([ s (make-string 5) ])
    (for ([ i (in-range 5) ])
      (let ([ wl (string-ref word i)  ]
            [ gl (string-ref guess i) ])
        (cond [ (char=? wl gl) (string-set! s i #\p) ]
              [ (for/or ([ j (in-range 5) ])
                  (char=? (string-ref word j) gl))
                (string-set! s i #\i) ]
              [ else (string-set! s i #\e) ])))
    s))

(define (update-state words wordle included excluded guess feedback)
  (let ([ wordle (vector-copy wordle) ])
    (let loop ([ i 0 ][ included included ][ excluded excluded ])
      (if (> i 4)
          (values (filter-words words wordle included excluded) wordle included excluded)
          (let ([ guess-letter (string-ref guess i)    ]
                [ fl           (string-ref feedback i) ])
            (cond [ (char=? #\p fl)
                    (vector-set! wordle i guess-letter)
                    (loop (add1 i) (add-included included guess-letter) excluded) ]
                  [ (char=? #\i fl)
                    (loop (add1 i) (add-included included guess-letter i) excluded) ]
                  [ (char=? #\e fl)
                    (loop (add1 i) included (add-excluded excluded guess-letter)) ]
                  [ else (error "Invalid feedback") ]))))))

(define (guess-word words wordle frequencies included excluded)
  (define (letter-already-included? w l pos)
    (for/or ([ i (in-range pos) ])
      (char=? (string-ref w i) l)))

  (define (score-word w)
    (for/sum ([ l   (in-string w) ]
              [ pos (in-naturals) ])
      (if (memv l excluded)
          0
          (let ([ freq  (get-frequency frequencies pos l) ]
                [ wrong (hash-ref included l #f)          ])
            (if wrong
                0
                (if (letter-already-included? w l pos)
                    0
                    freq))))))

  (let loop ([ words words ][ score 0 ][ guesses '() ])
    (if (null? words)
        (car guesses)
        (let* ([ word   (car words)       ]
               [ score* (score-word word) ])
          (cond [ (> score* score)
                  ;(printf "~a = ~a\n" word score*)
                  (loop (cdr words) score* (list word)) ]
                [ (= score* score)
                  (loop (cdr words) score* (cons word guesses)) ]
                [ else
                  (loop (cdr words) score guesses) ])))))

;; Return a two dimensional vector (implemented by a single vector) of
;; 5 rows (one per letter position) by 26 columns one per letter:
(define (make-letter-frequencies words)
  (let ([ vec (make-vector (* 5 26) 0) ])
    (for ([ word (in-list words) ])
      (for ([ letter (in-string word) ]
            [ pos (in-naturals) ])
        (inc-frequency! vec pos letter)))
    vec))

(define (inc-frequency! v p l)
  (let ([ idx (+ (* p 26) (letter-index l)) ])
    (vector-set! v idx (add1 (vector-ref v idx)))))

(define (get-frequency v p l)
  (let ([ idx (+ (* p 26) (letter-index l)) ])
    (vector-ref v idx)))

;; Index of letter: a = 0, b = 1, ... , z = 25
(define (letter-index l)
  (- (char->integer l)
     (char->integer #\a)))

(define (filter-words words wordle included excluded)
  (define (word-has-all-included? letters)
    (andmap (λ (l) (memv l letters)) (hash-keys included)))

  (define (word-has-none-included-positions? letters)
    (for/and ([ (l i) (in-indexed letters) ])
      (for/and ([ pos (in-list (hash-ref included l '())) ])
        (not (= i pos)))))

  (define (word-has-none-excluded? letters)
    (not (ormap (λ (l) (memv l letters)) excluded)))

  (define (word-matches-wordle? letters)
    (for/and ([ i (in-range 5) ]
              [ l (in-list letters) ])
      (let ([ v (vector-ref wordle i) ])
        (or (not v)
            (char=? v l)))))

  (define (pred? w)
    (let ([ letters (string->list w) ])
      (and (word-has-all-included? letters)
           (word-has-none-included-positions? letters)
           (word-has-none-excluded? letters)
           (word-matches-wordle? letters))))

  (filter pred? words))

(define (add-included hsh l [ pos #f ])
  (cond [ pos                 (hash-set hsh l (cons pos (hash-ref hsh l '()))) ]
        [ (hash-ref hsh l #f) hsh                                              ]
        [ else                (hash-set hsh l '())                             ]))

(define (add-excluded lst l)
  (if (memv l lst)
      lst
      (cons l lst)))

(define (solved? wordle)
  (for/and ([ i (in-range 5) ])
    (vector-ref wordle i)))

(define (game start-word targets words)
  (let* ([ num-words (length targets) ]
         [ total-guesses
           (for/sum ([ word (in-list targets) ])
             (play words
                   #(#f #f #f #f #f)
                   (hash)
                   '()
                   1
                   ;(bot-agent word)
                   (human-agent word)
                   start-word
                   )) ]
         [ average (/ total-guesses num-words) ])
    (exact->inexact average)))

(module+ main
  (let* ([ targets (file->lines "./wordle-targets.txt") ]
         [ words   (file->lines "./wordle-guesses.txt") ])
    ;; (time
    ;;  (game #f targets words))))
    (play words
          #(#f #f #f #f #f)
          (hash)
          '()
          1
          ;(bot-agent word)
          human-agent
          "salet"
          )))

(module+ test
  (require rackunit)

  ;; letter frequencies
  (let* ([ words '("abc" "aac" "abd" "dad") ]
         [ freq (make-letter-frequencies words) ])
    (check-equal? (get-frequency freq 0 #\a) 3)
    (check-equal? (get-frequency freq 0 #\b) 0)
    (check-equal? (get-frequency freq 0 #\d) 1)
    (check-equal? (get-frequency freq 1 #\a) 2)
    (check-equal? (get-frequency freq 1 #\b) 2)
    (check-equal? (get-frequency freq 1 #\c) 0)
    (check-equal? (get-frequency freq 2 #\a) 0)
    (check-equal? (get-frequency freq 2 #\c) 2)
    (check-equal? (get-frequency freq 2 #\d) 2))

  )
