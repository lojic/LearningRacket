#lang racket
(require "../advent.rkt")

(define-values (vget width height commands)
  (match-let ([ (list str path) (parse-aoc 22 #:sep "\n\n" #:print-sample #f) ])
    (let* ([ lines    (string-split str "\n")              ]
           [ width    (list-max (map string-length lines)) ]
           [ height   (length lines)                       ]
           [ lines    (map (λ (s)
                             (let ([ len (string-length s) ])
                               (if (< len width)
                                   (string-append s (make-string (- width len) #\space))
                                   s)))
                           lines) ]
           [ vec      (apply vector (string->list (apply string-append lines))) ]
           [ vget     (λ (c)
                        (vector-ref vec (+ (* (sub1 (imag-part c)) width)
                                           (sub1 (real-part c))))) ]
           [ commands (map atom (regexp-match* #px"(\\d+|[LR])" path)) ])
      (values vget width height commands))))

(define right 1)
(define left -1)
(define up   -i)
(define down +i)
(define start-tile 51+1i)
(define tile-size-1 49)

(define (solve next-pos)
  (define (dir-value dir)
    (match dir
      [ (== right) 0 ]
      [ (== down)  1 ]
      [ (== left)  2 ]
      [ (== up)    3 ]))

  (define (execute-command next-pos command pos dir)
    (define (execute-move next-pos pos dir n)
      (if (zero? n)
          (values pos dir)
          (let-values ([ (pos* dir*) (next-pos pos dir) ])
            (if (char=? #\# (vget pos*))
                (values pos dir)
                (execute-move next-pos pos* dir* (sub1 n))))))

    (if (number? command)
        (let-values ([ (pos* dir*) (execute-move next-pos pos dir command) ])
          (values pos* dir*))
        (values  pos (match command
                       [ "L" (match dir
                               [ (== right) up    ]
                               [ (== down)  right ]
                               [ (== left)  down  ]
                               [ (== up)    left  ]) ]
                       [ "R" (match dir
                               [ (== right) down  ]
                               [ (== down)  left  ]
                               [ (== left)  up    ]
                               [ (== up)    right ]) ]))))

  (let loop ([ pos      start-tile ]
             [ dir      right      ]
             [ commands commands   ])
    (if (null? commands)
        (+ (* 1000 (imag-part pos))
           (* 4 (real-part pos))
           (dir-value dir))
        (let-values ([ (pos* dir*) (execute-command next-pos (car commands) pos dir) ])
          (loop pos* dir* (cdr commands))))))

;; Part 1 specific code -----------------------------------------------------------------------

(define (part1-next-pos pos dir)
  (define (in-bounds? pos)
    (let ([ col (real-part pos) ]
          [ row (imag-part pos) ])
      (and (<= 1 col width)                     ; Horizontal ok
           (<= 1 row height)                    ; Vertical ok
           (not (char=? #\space (vget pos)))))) ; Not a space

  (define (most-tile-pos pos dir)
    (let loop ([ pos pos ])
      (if (in-bounds? pos)
          (loop (+ pos dir))
          (- pos dir)))) ; We went to far, backup

  (let ([ pos* (+ pos dir) ])
    (if (in-bounds? pos*)
        (values pos* dir)
        (values (most-tile-pos pos (- dir)) dir))))

;; Part 2 specific code -----------------------------------------------------------------------

(struct tile (key origin neighbors))

(define tile-b
  (tile 'b 51+1i   (hash right (cons 'a right) down (cons 'c down) left (cons 'e right) up (cons 'f right))))
(define tile-a
  (tile 'a 101+1i  (hash right (cons 'd left)  down (cons 'c left) left (cons 'b left)  up (cons 'f up))))
(define tile-c
  (tile 'c 51+51i  (hash right (cons 'a up)    down (cons 'd down) left (cons 'e down)  up (cons 'b up))))
(define tile-d
  (tile 'd 51+101i (hash right (cons 'a left)  down (cons 'f left) left (cons 'e left)  up (cons 'c up))))
(define tile-e
  (tile 'e 1+101i  (hash right (cons 'd right) down (cons 'f down) left (cons 'b right) up (cons 'c right))))
(define tile-f
  (tile 'f 1+151i  (hash right (cons 'd up)    down (cons 'a down) left (cons 'b down)  up (cons 'e up))))

(define tiles (hash 'a tile-a 'b tile-b 'c tile-c 'd tile-d 'e tile-e 'f tile-f))

(define (hop-tile a-tile pos dir)
  (define (transform src-origin src-pos src-dir dst-origin dst-dir)
    (define len tile-size-1)
    (values (cond [ (= src-dir dst-dir)
                    (let* ([ pos* (+ src-pos src-dir) ]
                           [ y    (imag-part pos*)    ])
                      (cond [ (< y 1)   (+ pos* -100+200i) ] ; Special case transition between A & F
                            [ (> y 200) (+ pos* 100-200i) ]
                            [ else      pos*           ])) ]
                  [ (and (= src-dir left) (= dst-dir right))
                    (+ dst-origin (- (+ src-origin (* len +i)) src-pos)) ]
                  [ (and (= src-dir left) (= dst-dir down))
                    (+ dst-origin (imag-part (- src-pos src-origin))) ]
                  [ (and (= src-dir right) (= dst-dir left))
                    (- (+ dst-origin (* len +i) len) (* +i (- (imag-part src-pos) (imag-part src-origin)))) ]
                  [ (and (= src-dir right) (= dst-dir up))
                    (+ (+ dst-origin (* len +i)) (real-part (- (imag-part src-pos) (imag-part src-origin)))) ]
                  [ (and (= src-dir down) (= dst-dir left))
                    (+ (+ dst-origin len) (* (- (real-part src-pos) (real-part src-origin)) +i)) ]
                  [ (and (= src-dir up) (= dst-dir right))
                    (+ dst-origin (* +i (- (real-part src-pos) (real-part src-origin)))) ])
            dst-dir))

  (match-let ([ (cons key dir*) (hash-ref (tile-neighbors a-tile) dir) ])
    (transform (tile-origin a-tile) pos dir (tile-origin (hash-ref tiles key)) dir*)))

(define (part2-next-pos pos dir)
  (define (leaving-tile? a-tile pos dir)
    (let* ([ origin (tile-origin a-tile) ]
           [ ox     (real-part origin)   ]
           [ oy     (imag-part origin)   ]
           [ x      (real-part pos)      ]
           [ y      (imag-part pos)      ]
           [ len    tile-size-1     ])
      (or (and (= dir right) (= x (+ ox len)))
          (and (= dir down)  (= y (+ oy len)))
          (and (= dir left)  (= x ox))
          (and (= dir up)    (= y oy)))))

  (define (pos->tile pos)
    (findf (λ (a-tile)
             (let* ([ orig (tile-origin a-tile) ]
                    [ x1   (real-part orig)     ]
                    [ y1   (imag-part orig)     ]
                    [ x2   (+ x1 tile-size-1)   ]
                    [ y2   (+ y1 tile-size-1)   ])
               (and (<= x1 (real-part pos) x2)
                    (<= y1 (imag-part pos) y2))))
           (hash-values tiles)))

  (let ([ a-tile (pos->tile pos) ])
    (let-values ([ (pos* dir*) (if (leaving-tile? a-tile pos dir)
                                   (hop-tile a-tile pos dir)
                                   (values (+ pos dir) dir)) ])
      (values pos* dir*))))

(time (check-equal? (solve part1-next-pos) 136054))
(time (check-equal? (solve part2-next-pos) 122153))
