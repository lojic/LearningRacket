#lang racket

;; This was a tough puzzle for me. My initial thought was to add &
;; subtract cubes by intersecting and splitting cubes into smaller
;; cubes. This seemed like a lot of work, so I checked a couple Slack
;; threads to see if I was missing something. Nothing came up beyond
;; what I had thought of already (which I came to learn is essentially
;; CSG), so I almost began the work of splitting cubes, etc.,
;;
;; I was just too lazy to do it though, so I had a thought about just
;; keeping a running list of intersections with positive and negative
;; values. So instead of just intersecting a new cube with existing
;; cubes, I also intersect with the previous intersections of cubes
;; and basically flip the sign.
;;
;; After fixing a combinatorial explosion issue by using a hash, it
;; worked perfectly and ran in ~ 105 ms. I'm still amazed the idea
;; worked! :)

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
  (hash-set! hsh b (add1 (hash-ref hsh b 0))))

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
  (check-equal? (time (solve "day22.txt")) 1225064738333321))
