#lang racket

;; Version inspired by Todd Ginsberg's version

(require threading)
(define input (file->lines "day03.txt"))

(define (solve-part1 input)
  (let* ([ gamma (~>> input first indices
                     (map (λ (column)
                            (if (> (count (λ (it) (char=? (chr it column) #\1)) input)
                                   (/ (length input) 2)) #\1 #\0))) list->string) ]
         [ epsilon  (~>> gamma string->list (map (λ (it)
                                                  (if (char=? it #\1) #\0 #\1))) list->string) ])
    (* (~> gamma to-int) (~> epsilon to-int))))

(define (solve-part2 input)
  (* (~> input (bitwise-filter #t) to-int) (~> input (bitwise-filter #f) to-int)))

(define (bitwise-filter input keep-most-common)
  (~>> input first indices (foldl (λ (column inputs)
                                    (if (= (length inputs) 1)
                                        inputs
                                        (let-values ([ (lst1 lst2) (~>> inputs
                                                                        (partition (λ (it)
                                                                                     (char=? (chr it column) #\1)))) ])
                                          (choose (if keep-most-common >= <) lst1 lst2)))) input) first))

(define (choose pred? lst1 lst2) (if (pred? (length lst1) (length lst2)) lst1 lst2))

;; --------------------------------------------------------------------------------------------
;; Support code
;; --------------------------------------------------------------------------------------------

(define (indices s) (range (string-length s)))
(define (to-int s)  (read (open-input-string (format "#b~a" s))))
(define (chr s pos) (string-ref s pos))

(module+ test
  (require rackunit)

  (check-equal? (solve-part1 input) 852500)
  (check-equal? (solve-part2 input) 1007985))
