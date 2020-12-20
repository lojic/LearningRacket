#lang racket

(define (parse-input fname)
  (define (parse-rules rules)
    (for/hash ([ s (in-list (string-split rules "\n")) ])
      (match-let ([ (list key parts) (string-split s ": ") ])
        (values key (map string-split (string-split (string-replace parts "\"" "") " | "))))))
  (match-let ([ (list rules data) (string-split (file->string fname) "\n\n") ])
    (values (parse-rules rules) (string-split data "\n"))))

(define-values (hsh data) (parse-input "./day19.txt"))

(define (create-regex key)
  (define (create-regex-str key)
    (let ([ val (hash-ref hsh key) ])
      (if (char-alphabetic? (string-ref (caar val) 0))
          (caar val)
          (if (> (length val) 1)
              (string-append "(" (string-join (map create-regex-lst val) "|") ")")
              (create-regex-lst (car val))))))
  (define (create-regex-lst lst) (string-join (map create-regex-str lst) ""))
  (regexp (string-append "^" (create-regex-str key) "$")))

(define (part1) (count (curry regexp-match? (create-regex "0")) data))

(define (part2)
  (define (update-rules hsh)
    (let* ([ rule8  (hash-ref hsh "8") ][ rule11 (hash-ref hsh "11") ])
      (hash-set* hsh "8" (append rule8 (list (cons "42" (last rule8))))
                    "11" (append rule11 (list (append '("42") (last rule11) '("31")))))))
  (let loop ([ last (part1) ])
    (set! hsh (update-rules hsh))
    (let ([ n (part1) ])
      (if (= n last) n (loop n)))))

(module+ test (require rackunit)
  (check-equal? (part1) 265)
  (check-equal? (part2) 394))
