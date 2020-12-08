#lang racket

(require threading)

(struct instr ([ op #:mutable ] arg [ seq #:mutable ]) #:transparent)
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
  (if (>= (mach-pos m) (vector-length (mach-v m)))
      (mach! m [ halt 'normal ])
      (let ([ ins (vector-ref (mach-v m) (mach-pos m)) ])
        (if (instr-seq ins)
            (mach! m [ halt 'infinite ])
            (begin
              (set-instr-seq! ins (mach-pos m))
              (match (instr-op ins)
                [ "acc" (mach! m [ acc (+ (instr-arg ins) (mach-acc m)) ]
                               [ pos (add1 (mach-pos m))              ]) ]
                [ "jmp" (mach! m [ pos (+ (instr-arg ins) (mach-pos m)) ]) ]
                [ "nop" (mach! m [ pos (add1 (mach-pos m)) ]) ]))))))

(define (run m) (if (mach-halt m) m (run (exec m))))

(define (xor-op! ins)
  (if (string=? "nop" (instr-op ins))
      (set-instr-op! ins "jmp")
      (set-instr-op! ins "nop")))

(define (reset! m ins)
  (xor-op! ins)
  (for ([ i (in-range (vector-length (mach-v m))) ])
    (set-instr-seq! (vector-ref (mach-v m) i) #f)))

(define (try m i)
  (let ([ ins (vector-ref (mach-v m) i) ])
    (if (member (instr-op ins) '("nop" "jmp"))
        (begin
          (xor-op! ins)
          (let ([ m (run m) ])
            (reset! m ins)
            (if (eq? (mach-halt m) 'normal)
                m
                #f)))
        #f)))

(module+ main
  (define m (load "day08.txt"))

  (let loop ([ i 0 ])
    (let ([ new-m (try m i) ])
      (if new-m
          (printf "Accumulator is ~a\n" (mach-acc new-m))
          (loop (add1 i))))))
