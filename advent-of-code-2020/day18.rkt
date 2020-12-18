#lang racket

(define (eval-expr parse evl str)
  (define-values (lhs rest) (parse str))
  (let loop ([ val (evl lhs (curry eval-expr parse evl)) ][ rest rest ])
    (if (non-empty-string? rest)
        (let*-values ([ (op rest)  (parse-token rest) ]
                      [ (rhs rest) (parse rest) ])
          (loop ((match (string-ref op 0)
                   [ #\+ + ][ #\* * ]) val (evl rhs (curry eval-expr parse evl))) rest))
        val)))

(define (eval-factor s eval-expr)
  (define-values (term rest) (parse-token s))
  (let loop ([ val (eval-term term eval-expr) ][ rest rest ])
    (if (non-empty-string? rest)
        (let*-values ([ (op rest)   (parse-token rest) ]
                      [ (term rest) (parse-token rest) ])
          (loop (+ val (eval-term term eval-expr)) rest))
        val)))

(define (eval-term term eval-expr)
  (cond [ (regexp-match? #px"^\\d+$" term)
          (string->number term) ]
        [ (regexp-match? #px"^\\(" term)
          (eval-expr (substring term 1 (sub1 (string-length term)))) ]))

(define (parse-factor s)
  (define-values (lhs rest) (parse-token s))
  (let loop ([ lhs lhs ][ rest rest ])
    (if (or (= (string-length rest) 0)
            (char=? (string-ref rest 0) #\*))
        (values lhs rest)
        (let-values ([(lh rs) (parse-token rest)])
          (loop (string-append lhs lh) rs)))))

(define (parse-token s)
  (if (non-empty-string? s)
      (let* ([ m     (regexp-match #px"^(\\d+|[-+*/(])(.*)$" s) ]
             [ token (second m)                                ]
             [ rest  (third m)                                 ])
        (if (string=? token "(")
            (parse-paren-expr rest)
            (values token rest)))
      (values #f "")))

(define (parse-paren-expr s)
  (let loop ([ n 1 ][ i 0 ])
    (if (= n 0)
        (values (string-append "(" (substring s 0 i)) (substring s i))
        (let ([ c (string-ref s i) ])
          (cond [ (char=? c #\() (loop (add1 n) (add1 i)) ]
                [ (char=? c #\)) (loop (sub1 n) (add1 i)) ]
                [ else           (loop n (add1 i))        ])))))

(define (part1 s) (eval-expr parse-token eval-term s))
(define (part2 s) (eval-expr parse-factor eval-factor s))

(define (run part)
  (for/sum ([ line (in-list (file->lines "day18.txt")) ])
    (part (string-replace line #px"\\s" ""))))

(module+ test (require rackunit)
  (check-equal? (run part1) 18213007238947)
  (check-equal? (run part2) 388966573054664))
