#lang racket

;; Load 73,052 words
(define words (for/set ([ word (in-lines (open-input-file "dictionary.txt")) ])
                       word))

(define MIN 6)

(define (is-multi-letter-word? word)
  (define (has-enough-letters? hsh)
    (let loop ([ keys (hash-keys hsh) ])
      (if (null? keys)
          #f
          (let* ([ l   (car keys)       ]
                 [ num (hash-ref hsh l) ])
            (if (>= num MIN)
                #t
                (loop (cdr keys)))))))

  (define (add-letter hsh l)
    (hash-set hsh l (add1 (hash-ref hsh l 0))))

  (let loop ([ hsh (hash) ][ lst (string->list word) ])
    (if (null? lst)
        (has-enough-letters? hsh)
        (let ([ l (car lst) ])
          (loop (add-letter hsh l) (cdr lst))))))

(define (find-multi-letter-words words)
  (let loop ([ result '() ][ word-stream (set->stream words) ])
    (if (stream-empty? word-stream)
        result
        (let ([ word (stream-first word-stream) ]
              [ rest (stream-rest word-stream)  ])
          (if (is-multi-letter-word? word)
              (loop (cons word result) rest)
              (loop result rest))))))

(module+ main
  (let ([ results (find-multi-letter-words words) ])
    (println results)))
