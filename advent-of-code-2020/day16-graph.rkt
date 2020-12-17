#lang racket
(require threading)
(require graph)

;; A version (Part 2) using maximum bipartite matching thanks to haskal in the Racket slack
(define (parse-input fname)
  (define (parse-ticket str) (map string->number (string-split str ",")))

  (define (parse-field str)
    (match-let ([ (list name ranges) (string-split str ": ") ])
      (cons name (~> (string-split ranges " or ")
                     (map (compose (curry map string->number) (curryr string-split "-")) _)
                     (map (λ (l) (cons (car l) (cadr l))) _)))))

  (match-let ([ (list fields ticket others) (~> (string-split (file->string fname) "\n\n")
                                                (map (curryr string-split "\n") _)) ])
    (values
     (map parse-field fields) (parse-ticket (cadr ticket)) (map parse-ticket (cdr others)))))

(define (valid? n field)
  (define (in-range? r) (<= (car r) n (cdr r)))
  (ormap in-range? (cdr field)))

(define (part2 fname)
  (define ((matches-a-field? fields) n) (ormap (curry valid? n) fields))
  (let-values ([ (fields ticket others) (parse-input fname) ])
    (let* ([ valid-tickets (filter (curry andmap (matches-a-field? fields)) others) ]
           [ g (undirected-graph
                (for*/list ([ i     (in-range (length ticket)) ]
                            [ field (in-list fields)           ]
                            #:when (andmap (compose (curryr valid? field) (curryr list-ref i))
                                           valid-tickets))
                  (list i (car field)))) ])
      (for/product ([ tuple (in-list (maximum-bipartite-matching g)) ]
                    #:when (string-contains? (second tuple) "departure"))
                   (list-ref ticket (first tuple))))))

(module+ test (require rackunit)
  (check-equal? (part2 "day16.txt") 517827547723))
