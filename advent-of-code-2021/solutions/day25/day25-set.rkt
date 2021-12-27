#lang racket

;; Port of https://github.com/hyper-neutrino/advent-of-code/blob/main/2021/day25.py

(require threading)

(define grid (~>> (file->lines "day25.txt")
                  (map string->list)))

(define width (length (car grid)))
(define height (length grid))

(define-values (east south)
  (let ([ east  (make-hasheq) ]
        [ south (make-hasheq) ])
    (for* ([ (line row) (in-indexed grid) ]
           [ (c col)    (in-indexed line) ])
      (cond [ (char=? c #\>) (hash-set! east  (+ col (* row width)) #t) ]
            [ (char=? c #\v) (hash-set! south (+ col (* row width)) #t) ]))
    (values east south)))

(let loop ([ i 1 ][ east east ][ south south ])
  (let ([ ne (make-hasheq) ]
        [ ns (make-hasheq) ])

    (for ([ (idx _) (in-hash east) ])
      (let*-values ([ (row col) (quotient/remainder idx width) ]
                    [ (n) (+ (modulo (add1 col) width) (* row width)) ])
        (if (or (hash-ref east n #f)
                (hash-ref south n #f))
            (hash-set! ne idx #t)
            (hash-set! ne n #t))))

    (let ([ t    (equal? east ne) ]
          [ east ne               ])
      (for ([ (idx _) (in-hash south) ])
        (let*-values ([ (row col) (quotient/remainder idx width) ]
                      [ (n) (+ col (* (modulo (add1 row) height) width)) ])
          (if (or (hash-ref east n #f)
                  (hash-ref south n #f))
              (hash-set! ns idx #t)
              (hash-set! ns n #t))))

      (if (and t (equal? south ns))
          i
          (loop (add1 i) east ns)))))
