#lang racket
(require "../advent.rkt")

(define hsh (for/fold ([ hsh (hash) ])
                      ([ pair (in-list (~> (parse-aoc 23 string->list #:print-sample #f)
                                           (map enumerate _)
                                           enumerate)) ])
              (match-let ([ (cons line y) pair ])
                (for/fold ([ hsh hsh ])
                          ([ pair (in-list line) ])
                  (match-let ([ (cons c x) pair ])
                    (if (char=? c #\#)
                        (hash-set hsh (make-rectangular x y) #t)
                        hsh))))))

(define-values (N NE E SE S SW W NW) (values -i 1-i 1 1+i +i -1+i -1 -1-i))

(define neighbors          (list N NE E SE S SW W NW))
(define directions         (list N S W E))
(define count-ground-tiles (λ (hsh) (- (elf-area hsh) (hash-count hsh))))

(define (play r limit [ hsh hsh ][ directions directions ])
  (if (> r limit)
      (cons r hsh)
      (let-values ([ (num-moved proposed) (propose-moves hsh directions) ])
        (if (zero? num-moved)
            (cons r proposed)
            (play (add1 r)
                  limit
                  (resolve-conflicts proposed)
                  (rotate-list directions))))))

(define (propose-moves hsh directions)
  (define (any-adjacent? hsh elf)
    (ormap (λ (dir)
             (hash-ref hsh (+ elf dir) #f))
           neighbors))

  (define (any-neighbors? hsh elf dir)
    (ormap (λ (d)
             (hash-ref hsh (+ elf d) #f)) (match dir
                                            [ (== N) (list NW N NE) ]
                                            [ (== E) (list NE E SE) ]
                                            [ (== S) (list SE S SW) ]
                                            [ (== W) (list SW W NW) ])))

  (for/fold ([ num-moved 0 ]
             [ next (hash) ])
            ([ (elf _) (in-hash hsh) ])
    (if (any-adjacent? hsh elf)
        (let ([ hsh* (ormap (λ (dir)
                              (if (not (any-neighbors? hsh elf dir))
                                  (hash-update next (+ elf dir) (λ (lst)
                                                                  (cons elf lst)) '())
                                  #f))
                            directions) ])
          (if hsh*
              (values (add1 num-moved) hsh*)
              (values num-moved (hash-update next elf (λ (lst)
                                                        (cons elf lst)) '()))))
        (values num-moved (hash-update next elf (λ (lst)
                                                  (cons elf lst)) '())))))

(define (resolve-conflicts hsh)
  (for/fold ([ result (hash) ])
            ([ (key val) (in-hash hsh) ])
    (if (= 1 (length val))
        (hash-set result key #t)
        (for/fold ([ result result ])
                  ([ elf (in-list val) ])
          (hash-set result elf #t)))))

(define (elf-area hsh)
  (let* ([ keys (hash-keys hsh)      ]
         [ xs   (map real-part keys) ]
         [ ys   (map imag-part keys) ])
    (* (- (list-max ys) (sub1 (list-min ys)))
       (- (list-max xs) (sub1 (list-min xs))))))

(time (check-equal? (count-ground-tiles (cdr (play 1 10))) 4288)) ; Part 1

(time (check-equal? (car (play 1 MAX-INTEGER)) 940))              ; Part 2
