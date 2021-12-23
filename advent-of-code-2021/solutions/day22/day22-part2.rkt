#lang racket

(struct bb  (min max) #:transparent)
(struct cmd (fun b)   #:transparent)
(struct pt  (x y z)   #:transparent)

(define (solve file-name)
  (let* ([ hsh  (make-hash)       ]
         [ cmds (parse file-name) ])
    (hash-set! hsh (cmd-b (car cmds)) 1)

    (for ([ cmd (in-list (cdr cmds)) ])
      (process-cmd! cmd hsh))

    (for/sum ([ (key val) (in-hash hsh) ])
      (* val (bb-volume key)))))

(define (process-cmd! cmd hsh)
  (define overlapped (filter (λ (pair)
                               (bb-intersect? (cmd-b cmd) (car pair)))
                             (hash->list hsh)))

  ((cmd-fun cmd) (cmd-b cmd) overlapped hsh))

(define (add! b pairs hsh)
  (sub! b pairs hsh)
  (let ([ bval (add1 (hash-ref hsh b 0)) ])
    (hash-set! hsh b bval)))

(define (sub! b pairs hsh)
  (for ([ pair (in-list pairs) ])
    (let* ([ b*    (car pair)           ]
           [ b*val (cdr pair)           ]
           [ key   (bb-intersect b b*)  ]
           [ val   (hash-ref hsh key 0) ]
           [ val*  (+ (- b*val) val)    ])
      (if (= 0 val*)
          (hash-remove! hsh key)
          (hash-set! hsh key val*)))))

(define (bb-intersect? b1 b2)
  (and (>= (pt-x (bb-max b1)) (pt-x (bb-min b2)))
       (<= (pt-x (bb-min b1)) (pt-x (bb-max b2)))
       (>= (pt-y (bb-max b1)) (pt-y (bb-min b2)))
       (<= (pt-y (bb-min b1)) (pt-y (bb-max b2)))
       (>= (pt-z (bb-max b1)) (pt-z (bb-min b2)))
       (<= (pt-z (bb-min b1)) (pt-z (bb-max b2)))))

(define (bb-intersect b1 b2)
  (bb (pt (max (pt-x (bb-min b1)) (pt-x (bb-min b2)))
          (max (pt-y (bb-min b1)) (pt-y (bb-min b2)))
          (max (pt-z (bb-min b1)) (pt-z (bb-min b2))))
      (pt (min (pt-x (bb-max b1)) (pt-x (bb-max b2)))
          (min (pt-y (bb-max b1)) (pt-y (bb-max b2)))
          (min (pt-z (bb-max b1)) (pt-z (bb-max b2))))))

(define (bb-volume b)
  (* (add1 (- (pt-x (bb-max b)) (pt-x (bb-min b))))
     (add1 (- (pt-y (bb-max b)) (pt-y (bb-min b))))
     (add1 (- (pt-z (bb-max b)) (pt-z (bb-min b))))))

(define (parse file-name)
  (define pat #px"^(on|off) x=([-0-9]+)..([-0-9]+),y=([-0-9]+)..([-0-9]+),z=([-0-9]+)..([-0-9]+)$")

  (for/list ([ line (in-list (file->lines file-name)) ])
    (let* ([ groups (regexp-match pat line)            ]
           [ on?    (string=? "on" (second groups))    ])
      (match-let ([ (list xmin xmax ymin ymax zmin zmax)
                    (map string->number (cddr groups)) ])
        (let* ([ pmin (pt xmin ymin zmin)          ]
               [ pmax (pt xmax ymax zmax)          ])
          (cmd (if on? add! sub!) (bb pmin pmax)))))))

;; Tests --------------------------------------------------------------------------------------

(module+ test
  (require rackunit)

  (check-equal? (solve "day22.txt") 1225064738333321))
