#lang racket

;; Wordle Solver for hard mode

;; After each guess is proposed by the program, enter one of 3 letters
;; for each position:
;; p if the letter is correct for that position
;; i if the letter is included in the word at a different position
;; e if the letter is excluded from the word

;; TODO
;; Encode information if a letter is included more than once
;; Use letter frequency per position, not just globally

(define (play words wordle included excluded n agent)
  (if (> n 20)
      (error "Failed to guess the Wordle!")
      (let* ([ frequencies (letter-frequencies words)                       ]
             [ guess       (guess-word words frequencies (hash-keys included) excluded) ])
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

(define (guess-word words frequencies included excluded)
  (define (score-word w)
    (let ([ vec (make-vector 26 0) ])
      (for ([ l (in-string w) ])
        (when (not (or (memv l included)
                       (memv l excluded)))
          (let ([ i (letter-index l) ])
            (vector-set! vec
                         i
                         (vector-ref frequencies i)))))
      (vector-sum vec)))
    
  (let loop ([ words words ][ score 0 ][ guesses '() ])
    (if (null? words)
        (car guesses)
        (let* ([ word   (car words)       ]
               [ score* (score-word word) ])
          (cond [ (> score* score)
                  (loop (cdr words) score* (list word)) ]
                [ (= score* score)
                  (loop (cdr words) score* (cons word guesses)) ]
                [ else
                  (loop (cdr words) score guesses) ])))))

;; Return a vector of letter frequencies
(define (letter-frequencies words)
  (let ([ vec (make-vector 26 0) ])
    (for ([ word (in-list words) ])
      (for ([ letter (in-string word) ])
        (let ([ i (letter-index letter) ])
          (vector-set! vec i (add1 (vector-ref vec i))))))
    vec))

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

(define (vector-sum v)
  (for/sum ([ n (in-vector v) ])
    n))

(module+ main
  (let* ([ words (take (file->lines "./wordle-words.txt") 1000) ]
         [ num-words (length words)                 ]
         [ total-guesses
           (for/sum ([ word (in-list words) ])
             (play words
                   #(#f #f #f #f #f)
                   (hash)
                   '()
                   1
                   (bot-agent word))) ]
         [ average (/ total-guesses num-words) ])
    (printf "Average number of guesses = ~a\n" (exact->inexact average))))
         
