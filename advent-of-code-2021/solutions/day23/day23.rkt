#lang racket

(require racket/performance-hint)

(struct pod  (energy col x y)  #:transparent #:mutable)
(struct move (pod sx sy dx dy) #:transparent #:mutable)

(define board         #f)
(define lowest-energy #f)
(define num-slots     #f)
(define pods          #f)

(define-inline (vget x y)      (vector-ref board (+ (* y 11) x)))
(define-inline (vset! x y val) (vector-set! board (+ (* y 11) x) val))

(define (part1!)
  (solve 2 (list (pod  10 4 2 1) (pod  100 6 4 1) (pod    1 2 6 1) (pod 10 4 8 1)
                 (pod 100 6 2 2) (pod 1000 8 4 2) (pod 1000 8 6 2) (pod  1 2 8 2))))

(define (part2!)
  (solve 4 (list (pod   10 4 2 1) (pod  100 6 4 1) (pod    1 2 6 1) (pod  10 4 8 1)
                 (pod 1000 8 2 2) (pod  100 6 4 2) (pod   10 4 6 2) (pod   1 2 8 2)
                 (pod 1000 8 2 3) (pod   10 4 4 3) (pod    1 2 6 3) (pod 100 6 8 3)
                 (pod  100 6 2 4) (pod 1000 8 4 4) (pod 1000 8 6 4) (pod   1 2 8 4))))

(define (solve slots pods)
  (reset-game! pods slots)
  (play!)
  lowest-energy)

(define (play! [ energy 0 ])
  (define-inline (goal-state?)
    (andmap (λ (p)
              (and (> (pod-y p) 0)
                   (= (pod-col p) (pod-x p))))
            pods))

  (cond [ (>= energy lowest-energy) (void)                      ]
        [ (goal-state?)             (set! lowest-energy energy) ]
        [ else (for ([ move (in-list (generate-moves)) ])
                 (make-move! move)
                 (play! (+ energy (move-energy move)))
                 (unmake-move! move)) ]))

(define-inline (generate-moves [ pods pods ])
  (cond [ (null? pods) '() ]
        [ else (append (generate-pod-moves (car pods))
                       (generate-moves (cdr pods))) ]))

(define-inline (generate-pod-moves pod)
  (cond [ (= 0 (pod-y pod))              (hall-to-room-moves pod) ]
        [ (stay-put? pod)                '()                      ]
        [ (= (pod-col pod) (pod-x pod))  (room-to-hall-moves pod) ]
        [ else
          (let ([ moves (room-to-room-moves pod) ])
            (if (null? moves)
                (room-to-hall-moves pod)
                moves)) ]))

(define-inline (hall-to-room-moves pod)
  (let ([ row (home-row pod) ]
        [ x   (pod-x pod)    ]
        [ y   (pod-y pod)    ]
        [ col (pod-col pod)  ])
    (if (and row
             (hall-clear? x col))
        (list (move pod x y col row))
        '())))

(define-inline (room-to-hall-moves pod)
  (define-inline (helper inc op lim x y)
    (let loop ([ x* (inc x) ][ result '() ])
      (cond [ (or (op x* lim) (vget x* 0))
              result ]
            [ (or (= x* 2) (= x* 4) (= x* 6) (= x* 8))
              (loop (inc x*) result) ]
            [ else
              (loop (inc x*) (cons (move pod x y x* 0) result)) ])))

  (let ([ x (pod-x pod) ]
        [ y (pod-y pod) ])
    (if (room-clear? x y)
        (append (helper sub1 < 0 x y)
                (helper add1 > 10 x y))
        '())))

(define-inline (room-to-room-moves pod)
  (let ([ x   (pod-x pod)    ]
        [ y   (pod-y pod)    ]
        [ col (pod-col pod)  ]
        [ row (home-row pod) ])
    (if (and row
             (room-clear? x y)
             (hall-clear? x col))
        (list (move pod x y col row))
        '())))

(define-inline (make-move! a-move)
  (match-let ([ (move pod sx sy dx dy) a-move ])
    (vset! sx sy #f)
    (vset! dx dy (pod-col pod))
    (set-pod-x! pod dx)
    (set-pod-y! pod dy)))

(define-inline (unmake-move! a-move)
  (match-let ([ (move pod sx sy dx dy) a-move ])
    (vset! dx dy #f)
    (vset! sx sy (pod-col pod))
    (set-pod-x! pod sx)
    (set-pod-y! pod sy)))

(define-inline (move-energy a-move)
  (match-let ([ (move pod sx sy dx dy) a-move ])
    (* (pod-energy pod)
       (+ sy (abs (- dx sx)) dy))))

(define-inline (stay-put? pod)
  (let ([ col (pod-col pod) ]
        [ x   (pod-x pod)   ])
    (and (= col x)
         (let loop ([ y* (add1 (pod-y pod)) ])
           (if (> y* num-slots)
               #t
               (let ([ col* (vget col y*) ])
                 (if (= col col*)
                     (loop (add1 y*))
                     #f)))))))

(define-inline (home-row pod)
  (let ([ col (pod-col pod) ])
    (let loop ([ y* num-slots ])
      (if (< y* 1)
          #f
          (let ([ col* (vget col y*) ])
            (if (not col*)
                y*
                (if (= col col*)
                    (loop (sub1 y*))
                    #f)))))))

(define-inline (hall-clear? x1 x2)
  (let ([ maxx (max x1 x2) ])
    (let loop ([ x (add1 (min x1 x2)) ])
      (cond [ (>= x maxx) #t ]
            [ (vget x 0)  #f ]
            [ else (loop (add1 x)) ]))))

(define-inline (room-clear? x y)
  (let loop ([ y* (sub1 y) ])
    (if (< y* 0)
        #t
        (if (vget x y*)
            #f
            (loop (sub1 y*))))))

(define (reset-game! pods* num-slots*)
  (set! lowest-energy 1000000)
  (set! board (make-vector (* 11 5) #f))
  (set! pods pods*)
  (set! num-slots num-slots*)
  (for ([ pod (in-list pods) ])
    (vset! (pod-x pod) (pod-y pod) (pod-col pod))))

;; Tests --------------------------------------------------------------------------------------

(module+ test
  (require rackunit)
  (check-equal? (time (part1!)) 14350)
  (check-equal? (time (part2!)) 49742))
