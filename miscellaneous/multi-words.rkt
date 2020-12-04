#lang racket

;; Load 73,052 words
(define words (for/set ([ word (in-lines (open-input-file "dictionary.txt")) ])
                       word))

;; Indicate whether a word is composed of two valid sub-words
(define (is-multi-word? word words)
  (let ([ len (string-length word) ])
    (let loop ([ n 1 ])
      (let ([ prefix (substring word 0 n)   ]
            [ suffix (substring word n len) ])
        (cond [ (>= n len) #f ]
              [ (and (set-member? words prefix)
                     (set-member? words suffix)) #t ]
              [ else (loop (+ n 1)) ])))))

;; Loop over all words and track the number of multi-words
(define (find-multi-words words)
  (let loop ([ succeed 0 ] [ fail 0 ] [ word-stream (set->stream words) ])
    (if (stream-empty? word-stream)
        (values succeed fail)
        (let ([ word (stream-first word-stream) ]
              [ rest (stream-rest word-stream)  ])
          (cond [ (is-multi-word? word words) (loop (+ succeed 1) fail rest) ]
                [ else (loop succeed (+ fail 1) rest) ])))))

(module+ main
  (let-values ([ (succeed fail) (find-multi-words words) ])
    (printf "~a succeeded out of ~a\n" succeed (+ succeed fail))))