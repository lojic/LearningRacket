#lang racket

(require threading "../../advent/advent.rkt")

(struct scanner (id orientation origin beacons) #:transparent)

(define (part1 scanners)
  (~> (map scanner-beacons scanners)
      append*
      remove-duplicates
      length))

(define (part2 scanners)
  (~> (map scanner-origin scanners)
      largest-distance))

(define (solve part scanners)
  (let loop ([ matched   (list (first scanners)) ]
             [ unmatched (rest scanners)         ])
    (cond [ (null? unmatched) (part matched) ]
          [ else (let-values ([ (matched* unmatched*) (match-scanners matched unmatched) ])
                   (loop matched* unmatched*)) ])))

;; --------------------------------------------------------------------------------------------

(define (match-scanners matched unmatched)
  (let next-matched ([ matched* matched ])
    (let ([ src (first matched*) ])
      (let next-unmatched ([ unmatched* unmatched ])
        (if (null? unmatched*)
            (next-matched (rest matched*))
            (let ([ dst (match-scanner src (first unmatched*)) ])
              (if dst
                  (values (cons dst matched)
                          (filter-not (λ (s) (= (scanner-id dst) (scanner-id s)))
                                      unmatched))
                  (next-unmatched (cdr unmatched*)))))))))

(define (match-scanner s0 s1)
  (define (orient n lst) (map (orientation n) lst))

  (let ([ beacons0 (scanner-beacons s0) ]
        [ beacons1 (scanner-beacons s1) ])
    (let loop ([ orientation 0 ])
      (if (>= orientation 24)
          #f
          (let* ([ beacons (orient orientation beacons1) ]
                 [ tuples (~>> (for*/list ([ b0 (in-range (length beacons0)) ]
                                           [ b1 (in-range (length beacons))  ])
                                 (point-sub (list-ref beacons0 b0)
                                            (list-ref beacons b1)))
                               (group-by identity)
                               (map (λ (g) (cons (length g) g)))
                               (filter (λ (tuple) (>= (car tuple) 12)))) ])
            (if (null? tuples)
                (loop (add1 orientation))
                (let ([ s1-origin (cadar tuples) ])
                  (struct-copy scanner s1
                               [ orientation orientation ]
                               [ origin      s1-origin   ]
                               [ beacons
                                 (map (curry point-add s1-origin) beacons) ]))))))))

(define (largest-distance pts)
  (define (manhattan p1 p2)
    (+ (abs (- (point-x p1) (point-x p2)))
       (abs (- (point-y p1) (point-y p2)))
       (abs (- (point-z p1) (point-z p2)))))

  (let* ([ vec (list->vector pts)  ]
         [ len (vector-length vec) ])
    (for*/fold ([ largest 0 ])
               ([ i (in-range len) ]
                [ j (in-range len) ]
                #:when (not (= i j)))
      (let ([ distance (manhattan (vector-ref vec i) (vector-ref vec j)) ])
        (if (> distance largest)
            distance
            largest)))))

(define (parse file-name)
  (define (parse-scanner lines)
    (define pat #px"^--- scanner (\\d+) ---$")

    (define (parse-scanner-id s)
      (let ([ groups (regexp-match pat s) ])
        (string->number (second groups))))

    (if (null? lines)
        (cons #f '())
        (let ([ id (parse-scanner-id (first lines)) ])
          (let loop ([ lines (rest lines) ][ beacons '() ])
            (if (or (null? lines)
                    (regexp-match? pat (first lines)))
                (cons (scanner id 0 (point 0 0 0) beacons) lines)
                (let ([ lst (map string->number (string-split (first lines) ",")) ])
                  (loop (cdr lines) (cons (point (first lst)
                                                 (second lst)
                                                 (third lst)) beacons))))))))

  (let ([ lines (filter non-empty-string? (file->lines file-name)) ])
    (let loop ([ lines lines ][ scanners '() ])
      (match-let ([ (cons scanner lines) (parse-scanner lines) ])
        (if scanner
            (loop lines (cons scanner scanners))
            (reverse scanners))))))

(define orientations
  (vector-immutable
   identity
   (λ (pt) (point    (point-x pt)    (point-z pt)  (- (point-y pt))))
   (λ (pt) (point    (point-x pt) (- (point-y pt)) (- (point-z pt))))
   (λ (pt) (point    (point-x pt) (- (point-z pt))    (point-y pt)))
   (λ (pt) (point    (point-y pt)    (point-x pt)  (- (point-z pt))))
   (λ (pt) (point    (point-y pt)    (point-z pt)     (point-x pt)))
   (λ (pt) (point    (point-y pt) (- (point-x pt))    (point-z pt)))
   (λ (pt) (point    (point-y pt) (- (point-z pt)) (- (point-x pt))))
   (λ (pt) (point    (point-z pt)    (point-x pt)     (point-y pt)))
   (λ (pt) (point    (point-z pt)    (point-y pt)  (- (point-x pt))))
   (λ (pt) (point    (point-z pt) (- (point-x pt)) (- (point-y pt))))
   (λ (pt) (point    (point-z pt) (- (point-y pt))    (point-x pt)))
   (λ (pt) (point (- (point-x pt))    (point-y pt)  (- (point-z pt))))
   (λ (pt) (point (- (point-x pt))    (point-z pt)     (point-y pt)))
   (λ (pt) (point (- (point-x pt)) (- (point-y pt))    (point-z pt)))
   (λ (pt) (point (- (point-x pt)) (- (point-z pt)) (- (point-y pt))))
   (λ (pt) (point (- (point-y pt))    (point-x pt)     (point-z pt)))
   (λ (pt) (point (- (point-y pt))    (point-z pt)  (- (point-x pt))))
   (λ (pt) (point (- (point-y pt)) (- (point-x pt)) (- (point-z pt))))
   (λ (pt) (point (- (point-y pt)) (- (point-z pt))    (point-x pt)))
   (λ (pt) (point (- (point-z pt))    (point-x pt)  (- (point-y pt))))
   (λ (pt) (point (- (point-z pt))    (point-y pt)     (point-x pt)))
   (λ (pt) (point (- (point-z pt)) (- (point-x pt))    (point-y pt)))
   (λ (pt) (point (- (point-z pt)) (- (point-y pt)) (- (point-x pt))))))

(define (orientation n) (vector-ref orientations n))

(module+ test
  (require rackunit)
  (let ([ scanners (parse "day19.txt") ])
    (check-equal? (solve part1 scanners) 438)
    (check-equal? (solve part2 scanners) 11985)))
