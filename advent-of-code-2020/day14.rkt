#lang racket

(define sig-bit             35)
(define input               (file->lines "day14.txt"))
(define ((set-bit i) val)   (bitwise-ior val (arithmetic-shift 1 i)))
(define ((unset-bit i) val) (bitwise-and val (bitwise-not (arithmetic-shift 1 i))))
(define (mchar mask i)      (string-ref mask (- sig-bit i)))
(define (memory-sum hsh)    (for/sum ([ (k v) (in-hash hsh) ]) v))

(define (run part)
  (let loop ([ lst input ][ mem (hash) ][ mask #f ])
    (if (null? lst)
        (memory-sum mem)
        (match-let ([ (list lhs rhs) (string-split (car lst) " = ") ])
          (cond [ (string=? lhs "mask") (loop (cdr lst) mem rhs) ]
                [ else
                  (match-let* ([ (list _ addr) (regexp-match #px"^mem\\[(\\d+)\\]$" lhs) ]
                               [ (cons addr val) (cons (string->number addr) (string->number rhs))])
                    (loop (cdr lst) (part mem addr val mask) mask)) ])))))

(define (part1 mem addr val mask)
  (define (mask-bit c i val)
    (cond [ (char=? c #\X) val                 ]
          [ (char=? c #\0) ((unset-bit i) val) ]
          [ else           ((set-bit i) val)   ]))

  (hash-set mem addr (for/fold ([ val val ])([ i (in-range (add1 sig-bit)) ])
                       (mask-bit (mchar mask i) i val))))

(define (part2 mem addr val mask)
  (define (mask-bit c i addrs)
    (cond [ (char=? c #\0) addrs ]
          [ (char=? c #\1) (map (set-bit i) addrs) ]
          [ else (append (map (set-bit i) addrs)
                         (map (unset-bit i) addrs)) ]))

  (let loop ([ i 0 ][ addrs (list addr) ])
    (cond [ (> i sig-bit) (foldl (λ (addr obj) (hash-set obj addr val) ) mem addrs) ]
          [ else (loop (add1 i) (mask-bit (mchar mask i) i addrs)) ])))

(module+ test (require rackunit)
  (check-equal? (run part1) 10885823581193)
  (check-equal? (run part2) 3816594901962))
