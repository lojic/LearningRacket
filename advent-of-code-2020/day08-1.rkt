#lang racket

(require threading)

(struct instr (op arg [ seq #:mutable ]) #:transparent)
(struct mach (v pos acc halt) #:transparent)

(define-syntax-rule (mach! m a b ...) (struct-copy mach m a b ...))

(define (load fname)
  (~> (file->lines fname)
      (map parse _)
      (list->vector _)
      (mach _ 0 0 #f)))

(define (parse str)
  (let ([ m (regexp-match #px"^([a-z]+) ([-+0-9]+)$" str) ])
    (instr (second m) (string->number (third m)) #f)))

(define (exec m)
  (let ([ ins (vector-ref (mach-v m) (mach-pos m)) ])
    (if (instr-seq ins)
        (mach! m [ halt #t ])
        (begin
          (set-instr-seq! ins (mach-pos m))
          (match (instr-op ins)
            [ "acc" (mach! m [ acc (+ (instr-arg ins) (mach-acc m)) ]
                             [ pos (add1 (mach-pos m))              ]) ]
            [ "jmp" (mach! m [ pos (+ (instr-arg ins) (mach-pos m)) ]) ]
            [ "nop" (mach! m [ pos (add1 (mach-pos m)) ]) ])))))

(define (run m) (if (mach-halt m) m (run (exec m))))

(module+ main (printf "Accumulator is ~a\n" (mach-acc (run (load "day08.txt")))))



(module+ test
  (require rackunit)

  ;; load -------------------------------------------------------------------------------------
  (let* ([ m (load "day08-test.txt") ]
         [ v (mach-v m) ])
    (check-equal? (mach-acc m) 0)
    (check-equal? (mach-pos m) 0)
    (check-not-false (vector? v))
    (check-equal? (vector-length v) 9)
    (check-equal? (vector-ref v 0) (instr "nop" 0 #f))
    (check-equal? (vector-ref v 1) (instr "acc" 1 #f))
    (check-equal? (vector-ref v 4) (instr "jmp" -3 #f))
    (check-equal? (vector-ref v 5) (instr "acc" -99 #f))
    (check-equal? (vector-ref v 8) (instr "acc" 6 #f)))

  ;; parse ------------------------------------------------------------------------
  (for ([ tuple (in-list '(("nop +0" "nop" 0)
                           ("acc +1" "acc" 1)
                           ("jmp -4" "jmp" -4))) ])
    (let ([ obj (parse (first tuple)) ])
      (check-equal? (instr-op obj) (second tuple))
      (check-equal? (instr-arg obj) (third tuple))))

  )
