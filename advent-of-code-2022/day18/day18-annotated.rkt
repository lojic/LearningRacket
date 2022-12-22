#lang racket
(require "../advent.rkt")

(define in (parse-aoc 18 numbers))

;; Define the bounding box of the search space
(define-values (xmin xmax ymin ymax zmin zmax)
  (let* ([ xs (map car   in) ][ xmin (sub1 (list-min xs)) ][ xmax (add1 (list-max xs)) ]
         [ ys (map cadr  in) ][ ymin (sub1 (list-min ys)) ][ ymax (add1 (list-max ys)) ]
         [ zs (map caddr in) ][ zmin (sub1 (list-min zs)) ][ zmax (add1 (list-max zs)) ])
    (values xmin xmax ymin ymax zmin zmax)))

;; Iterate over all the sides/faces of the cubes, and either
;; 1) add them to the set if they're not a member, or
;; 2) remove them from the set if they are a member.
(define (part1 in)
  (for/fold ([ sides (hash) ])
            ([ cube (in-list in) ])
    (for/fold ([ sides sides ])
              ([ side (in-list (cube-sides cube)) ])
      (if (hash-ref sides side #f)
          (hash-remove sides side)
          (hash-set sides side #t)))))

;; 1. Choose the minimum point of the bounding box as our starting position
;; 2. Create a set of all cube faces using Part 1 (which removes shared faces)
;; 3. Create a set of seen cubes (initially the input cubes)
(define (part2 in)
  (expand-steam (list xmin ymin zmin)                       ; Start position
                (part1 in)                                  ; All cube faces
                (make-immutable-hash (map (λ (cube)         ; Seen cubes
                                            (cons cube #t))
                                          in))))

;; Simulate expanding steam to find all exterior cube faces:
(define (expand-steam cube sides seen [ sum 0 ])
  (if (seen? cube seen)
      ; We've already seen this cube, so return the current values
      (values sum seen)
      ; Process the cube
      (let loop ([ moves (next-cubes cube seen)     ]  ; Generate all the valid next positions from this cube
                 [ sum   (+ sum (faces cube sides)) ]  ; Add exterior faces this cube touches
                 [ seen  (mark-seen cube seen)      ]) ; Mark it as seen
        (if (null? moves)
            ;; No more moves, return
            (values sum seen)
            ;; Process the next move:
            ;; * Recursively call expand-steam
            ;; * Update the count of faces and the seen set
            ;; * Loop for the next move
            (let ([ move (car moves) ])
              (let-values ([ (sum* seen*) (expand-steam move sides seen sum) ])
                (loop (cdr moves)
                      sum*
                      seen*)))))))

(define (seen? cube seen)     (hash-ref seen cube #f))
(define (mark-seen cube seen) (hash-set seen cube #t))

;; Return all the sides of the cube
(define (cube-sides cube)
  (match-let ([ (list x y z) cube ])
    (list (list       x       y       z  'z)
          (list       x       y (sub1 z) 'z)
          (list       x       y       z  'x)
          (list (sub1 x)      y       z  'x)
          (list       x       y       z  'y)
          (list       x (sub1 y)      z  'y))))

;; Return a list of valid "next moves" from the cube
(define (next-cubes cube seen)
  (define (valid? c)
    (match-let ([ (list x y z) c ])
      (not (or (< x xmin) (> x xmax)
               (< y ymin) (> y ymax)
               (< z zmin) (> z zmax)
               (hash-ref seen c #f)))))

  (match-let ([ (list x y z) cube ])
    (filter valid? (list (list (add1 x) y z)
                         (list (sub1 x) y z)
                         (list x (add1 y) z)
                         (list x (sub1 y) z)
                         (list x y (add1 z))
                         (list x y (sub1 z))))))

;; Count the exterior faces touching the cube
(define (faces cube sides)
  (for/sum ([ side (cube-sides cube) ])
    (if (hash-ref sides side #f) 1 0)))

(check-equal? (hash-count (part1 in)) 3390)
(check-equal? (let-values ([(sum _) (part2 in)]) sum) 2058)
