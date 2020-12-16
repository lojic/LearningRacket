#lang racket

(require threading)

(define (parse-input fname)
  (match-let ([ (list fields ticket others)
                (~> (string-split (file->string fname) "\n\n")
                    (map (curryr string-split "\n") _)) ])
    (list
     (map parse-field fields) (parse-ticket (cadr ticket)) (map parse-ticket (cdr others)))))

(define (parse-field str)
  (match-let ([ (list name ranges) (string-split str ": ") ])
    (cons name (~> (string-split ranges " or ")
                   (map (compose (curry map string->number) (curryr string-split "-")) _)
                   (map (λ (l) (cons (car l) (cadr l))) _)))))

(define (parse-ticket str) (map string->number (string-split str ",")))

(define (run part fname) (apply part (parse-input fname)))

(define (valid? n field)
  (define (in-range? r) (<= (car r) n (cdr r)))
  (ormap in-range? (cdr field)))

(define (all-invalid? fields n) (not (ormap (curry valid? n) fields)))

(define (invalid-fields fields lst [ invalid '() ])
  (foldl (λ (n inv) (if (all-invalid? fields n) (cons n inv) inv)) '() lst))

(define (get-positions fields ticket others lst)
  (let loop ([ lst lst ] [ result '() ])
    (if (null? lst)
        result
        (let-values ([ (singles rest) (partition (compose (curry = 1) length car) lst) ])
          (define (remove-field pair)
            (cons (filter (λ (name) (not (member name (map caar singles)))) (car pair)) (cdr pair)))
          (loop (map remove-field rest) (append singles result))))))

(define ((reduce-fields fields) ticket positions)
  (for/list ([ field-names (in-list positions) ]
             [ n           (in-list ticket)    ])
    (filter (λ (name) (valid? n (assoc name fields))) field-names)))

(define (part1 fields _ others) (for/sum ([ lst others ]) (apply + (invalid-fields fields lst))))

(define (part2 fields ticket others)
  (~> (for/list ([ i (in-naturals) ]
                 [ pos (foldl (reduce-fields fields)
                              (for/list ([ i (in-range (length ticket)) ]) (map car fields))
                              (filter (λ (t) (null? (invalid-fields fields t))) others)) ])
        (cons pos i))
      (get-positions fields ticket others _)
      (map (λ (pair) (cons (caar pair) (cdr pair))) _)
      (filter (λ (pair) (string-contains? (car pair) "departure")) _)
      (map (λ (pair) (list-ref ticket (cdr pair))) _)
      (apply * _)))

(module+ test (require rackunit)
  (check-equal? (run part1 "day16.txt") 29019)
  (check-equal? (run part2 "day16.txt") 517827547723))
