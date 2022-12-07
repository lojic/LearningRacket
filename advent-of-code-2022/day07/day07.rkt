#lang racket
(require "../advent.rkt")

(struct dir (size dirs files))

(define in (cdr (parse-aoc 7 atoms)))

(define part1 (λ (s sizes) (list-sum (filter (curry > 100001) sizes))))
(define part2 (λ (s sizes) (findf (curry < (+ -40000000 (last sizes))) sizes)))

(define (solve part in)
  (let*-values ([ (s)     (process-commands (hash "/" (dir 0 (set) (set))) "/" in) ]
                [ (s n)   (compute-dir-sizes s "/") ]
                [ (sizes) (sort (for/list ([ (_ obj) (in-hash s) ]) (dir-size obj)) <) ])
    (part s sizes)))

;; --------------------------------------------------------------------------------------------

(define +dir (curry format "~a~a/"))

(define (process-commands s cwd entries)
  ;; Individual commands ----------------------------------------------------------------------
  (define (cd cwd dir-name)
    (if (string=? ".." dir-name)
        (regexp-replace #px"[a-z]+/$" cwd "")
        (+dir cwd dir-name)))

  (define (ls s cwd entries)
    (define (add-file s cwd entry)
      (let ([ obj (hash-ref s cwd) ])
        (hash-set s cwd (struct-copy dir obj
                                     [ files (set-add (dir-files obj) (first entry)) ]))))

    (define (add-dir s cwd entry)
      (let* ([ name (second entry)   ]
             [ path (+dir cwd name)  ]
             [ obj  (hash-ref s cwd) ]
             [ s    (if (hash-has-key? s path)
                        s
                        (hash-set s path (dir 0 (set) (set)))) ])
        (hash-set s cwd (struct-copy dir obj
                                     [ dirs (set-add (dir-dirs obj) name) ]))))

    (if (null? entries)
        (values s '())
        (let ([ entry (car entries) ])
          (match (car entry)
            [ (== "$")   (values s entries)                            ]
            [ (== "dir") (ls (add-dir s cwd entry)  cwd (cdr entries)) ]
            [ _          (ls (add-file s cwd entry) cwd (cdr entries)) ]))))
  ;; ------------------------------------------------------------------------------------------

  (if (null? entries)
      s
      (let ([ entry (car entries) ])
        (match (second entry)
          [ (== "ls")  (let-values ([ (s entries) (ls s cwd (cdr entries)) ])
                         (process-commands s cwd entries)) ]
          [ (== "cd")  (process-commands s (cd cwd (third entry)) (cdr entries)) ]))))

(define (compute-dir-sizes s path)
  (define (sub-dirs s names [sum 0])
    (if (null? names)
        (values s sum)
        (let*-values ([ (s n) (compute-dir-sizes s (+dir path (car names))) ])
            (sub-dirs s (cdr names) (+ n sum)))))

  (let*-values ([ (obj)         (hash-ref s path) ]
                [ (s sub-total) (sub-dirs s (set->list (dir-dirs obj)))  ]
                [ (total)       (+ sub-total
                                   (for/sum ([ size (in-set (dir-files obj)) ]) size)) ])
      (values (hash-set s path (struct-copy dir obj [ size total ])) total)))

(solve part1 in)
(solve part2 in)
