#lang racket

(require "../../advent/advent.rkt") ; for bool-list->decimal

(struct header          (version type)        #:transparent)
(struct packet          (header)              #:transparent)
(struct literal packet  (val)                 #:transparent)
(struct operator packet (len-type-id packets) #:transparent)
(struct chunk           (last? bits)          #:transparent)

;; --------------------------------------------------------------------------------------------

(define (solve part)
  (define-values (pkt _) (parse-packet (->binary (file->string "day16.txt"))))
  (part pkt))

(define (sum-versions pkt)
  (define version (compose header-version packet-header))

  (cond [ (literal? pkt) (version pkt) ]
        [ else (+ (version pkt)
                  (for/sum ([ pkt (operator-packets pkt) ])
                    (sum-versions pkt))) ]))

(define (eval-pkt pkt)
  (define (binary op) (λ (l) (if (op (first l) (second l)) 1 0)))

  (define op (match (header-type (packet-header pkt))
               [ 0 sum      ]
               [ 1 product  ]
               [ 2 list-min ]
               [ 3 list-max ]
               [ 4 #f       ]
               [ 5 (binary >) ]
               [ 6 (binary <) ]
               [ 7 (binary =) ]))

  (if (literal? pkt)
      (literal-val pkt)
      (op (map eval-pkt (operator-packets pkt)))))

;; Parsers ------------------------------------------------------------------------------------

(define (parse-packet bits)
  (let*-values ([ (a-header bits) (parse-header bits) ]
                [ (parser) (if (= 4 (header-type a-header))
                               parse-literal
                               parse-operator) ])
    (let-values ([ (pkt bits) (parser a-header bits) ])
      (values pkt bits))))

(define (parse-header bits)
  (let*-values ([ (version bits) (parse-version bits) ]
                [ (type bits)    (parse-type-id bits) ])
    (values (header version type) bits)))

(define (parse-version bits)
  (values (->number (take bits 3)) (drop bits 3)))

(define (parse-type-id bits)
  (values (->number (take bits 3)) (drop bits 3)))

(define (parse-literal a-header bits)
  (let-values ([ (lst bits) (parse-chunks bits) ])
    (values (literal a-header (->number lst)) bits)))

(define (parse-chunks bits [ result '() ])
  (let*-values ([ (a-chunk bits) (parse-chunk bits)                   ]
                [ (result)       (append result (chunk-bits a-chunk)) ])
    (if (chunk-last? a-chunk)
        (values result bits)
        (parse-chunks bits result))))

(define (parse-chunk bits)
  (let* ([ lst   (take bits 5)   ]
         [ last? (= 0 (car lst)) ])
    (values (chunk last? (drop lst 1)) (drop bits 5))))

(define (parse-operator a-header bits)
  (let* ([ len-type-id (car bits)    ]
         [ bits        (drop bits 1) ]
         [ fun (if (= 0 len-type-id)
                   parse-by-len
                   parse-by-num) ] )
    (let-values ([ (pkts bits) (fun bits) ])
      (values (operator a-header len-type-id pkts) bits))))

(define (parse-by-len bits)
  (let*-values ([ (len bits) (values (->number (take bits 15))
                                     (drop bits 15)) ]
                [ (sub-bits bits) (values (take bits len)
                                          (drop bits len)) ])
    (let loop ([ sub-bits sub-bits ][ packets '() ])
      (if (null? sub-bits)
          (values (reverse packets) bits)
          (let-values ([ (pkt sub-bits) (parse-packet sub-bits) ])
            (loop sub-bits (cons pkt packets)))))))

(define (parse-by-num bits)
  (let*-values ([ (num bits) (values (->number (take bits 11))
                                     (drop bits 11)) ])
    (let loop ([ bits bits ][ num num ][ packets '() ])
      (if (= num 0)
          (values (reverse packets) bits)
          (let-values ([ (pkt bits) (parse-packet bits) ])
            (loop bits (sub1 num) (cons pkt packets)))))))

;; Support ------------------------------------------------------------------------------------

(define (->number bits)
  (bool-list->decimal bits))

(define (->binary s)
  (append* (for/list ([ c (string->list s) ])
             (match c
               [ #\0 '(0 0 0 0) ][ #\1 '(0 0 0 1) ][ #\2 '(0 0 1 0) ][ #\3 '(0 0 1 1) ]
               [ #\4 '(0 1 0 0) ][ #\5 '(0 1 0 1) ][ #\6 '(0 1 1 0) ][ #\7 '(0 1 1 1) ]
               [ #\8 '(1 0 0 0) ][ #\9 '(1 0 0 1) ][ #\A '(1 0 1 0) ][ #\B '(1 0 1 1) ]
               [ #\C '(1 1 0 0) ][ #\D '(1 1 0 1) ][ #\E '(1 1 1 0) ][ #\F '(1 1 1 1) ]
               [ #\newline '() ]))))

;; Tests --------------------------------------------------------------------------------------

(module+ test
  (require rackunit)
  (check-equal? (solve sum-versions) 843)
  (check-equal? (solve eval-pkt) 5390807940351))
