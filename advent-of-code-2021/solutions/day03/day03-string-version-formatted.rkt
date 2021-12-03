#lang racket

(define input (file->lines "day03.txt"))

(define (col lst n)
  (map (curryr string-ref n) lst))

(define (common lst)
  (if (>= (count (curry char=? #\1) lst) (/ (length lst) 2))
      #\1
      #\0))

(define (to-decimal s)
  (read (open-input-string (format "#b~a" s))))

(define (flipc c)
  (if (char=? c #\1)
      #\0
      #\1))

(define (flip s)
  (build-string (string-length s)
                (λ (i)
                  (flipc (string-ref s i)))))

(define (gamma lst)
  (build-string (string-length (car lst))
                (λ (i)
                  (common (col lst i)))))

(define (part1 input)
  (let ([ g (gamma input) ])
    (* (to-decimal g)
       (to-decimal (flip g)))))

(define (part2 input)
  (define (life input oxy)
    (car (let loop ([ lst input ][ bit 0 ])
           (if (or (>= bit (string-length (car input)))
                   (< (length lst) 2))
               lst
               (loop (let ([ com-char (common (col lst bit)) ])
                       (filter (λ (s)
                                 (char=? (string-ref s bit)
                                         (if oxy
                                             com-char
                                             (flipc com-char))))
                               lst))
                     (add1 bit))))))

  (* (to-decimal (life input #t))
     (to-decimal (life input #f))))

(printf "Part1 = ~s, Part2 = ~s\n" (part1 input) (part2 input))
