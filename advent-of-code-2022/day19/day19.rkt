#lang racket
(require "../advent.rkt")

(struct blueprint (id bots))
(struct robot     (type count ore-cost cly-cost obs-cost min max))
(struct state     (steps ore cly obs geo ore-bots cly-bots obs-bots geo-bots))

(define in (parse-aoc 19 numbers #:print-sample #f))

(define (part1 a-state)
  (for/sum ([ bp (map parse-blueprint in) ])
    (let ([ a-state (dfs bp a-state a-state) ])
      (* (blueprint-id bp)
         (state-geo a-state)))))

(define (part2 a-state)
  (list-prod
   (for/list ([ bp (take (map parse-blueprint in) 3) ])
     (let ([ a-state (dfs bp a-state a-state) ])
       (state-geo a-state)))))

(define (dfs bp a-state best)
  (if (zero? (state-steps a-state))
      a-state
      (for/fold ([ best best ])
                ([ move (generate-moves bp a-state) ])
        (if (prune? move best)
            best
            (let ([ state* (dfs bp move best) ])
              (if (is-better? state* best)
                  state*
                  best))))))

(define (build-bot a-state bot)
  (struct-copy state a-state
               [ ore-bots (+ (state-ore-bots a-state) (if (eq? 'ore (robot-type bot)) 1 0) ) ]
               [ cly-bots (+ (state-cly-bots a-state) (if (eq? 'cly (robot-type bot)) 1 0) ) ]
               [ obs-bots (+ (state-obs-bots a-state) (if (eq? 'obs (robot-type bot)) 1 0) ) ]
               [ geo-bots (+ (state-geo-bots a-state) (if (eq? 'geo (robot-type bot)) 1 0) ) ]
               [ ore      (- (state-ore a-state) (robot-ore-cost bot))                       ]
               [ cly      (- (state-cly a-state) (robot-cly-cost bot))                       ]
               [ obs      (- (state-obs a-state) (robot-obs-cost bot))                       ]))

(define (collect a-state [ steps 0 ])
  (struct-copy state a-state
               [ steps (- (state-steps a-state) steps)                  ]
               [ ore   (+ (state-ore a-state) (* steps (state-ore-bots a-state))) ]
               [ cly   (+ (state-cly a-state) (* steps (state-cly-bots a-state))) ]
               [ obs   (+ (state-obs a-state) (* steps (state-obs-bots a-state))) ]
               [ geo   (+ (state-geo a-state) (* steps (state-geo-bots a-state))) ]))

(define (generate-move a-state bot)
  (if (>= ((robot-count bot) a-state) (robot-max bot))
      #f ; We already have enough of this bot
      (let ([ ore-needed (- (robot-ore-cost bot) (state-ore a-state)) ]
            [ cly-needed (- (robot-cly-cost bot) (state-cly a-state)) ]
            [ obs-needed (- (robot-obs-cost bot) (state-obs a-state)) ]
            [ ore-bots   (state-ore-bots a-state)                     ]
            [ cly-bots   (state-cly-bots a-state)                     ]
            [ obs-bots   (state-obs-bots a-state)                     ])
        (if (or (and (positive? ore-needed) (zero? ore-bots))
                (and (positive? cly-needed) (zero? cly-bots))
                (and (positive? obs-needed) (zero? obs-bots)))
            #f ; Missing a bot for a needed resource
            (let ([ steps-needed (add1 (max (if (positive? ore-needed) (ceiling (/ ore-needed ore-bots)) 0)
                                            (if (positive? cly-needed) (ceiling (/ cly-needed cly-bots)) 0)
                                            (if (positive? obs-needed) (ceiling (/ obs-needed obs-bots)) 0))) ])
              (if (<= steps-needed (- (state-steps a-state) (robot-min bot)))
                  (build-bot (collect a-state steps-needed) bot)
                  #f)))))) ; Ran out of time

(define (generate-moves bp a-state)
  (let ([ moves (filter identity (map (λ (bot)
                                        (generate-move a-state bot))
                                      (blueprint-bots bp))) ])
    (if (null? moves)
        (list (collect a-state 1))
        moves)))

(define (initialize-state steps) (state steps 0 0 0 0 1 0 0 0))
(define (is-better? a b)         (> (state-geo a) (state-geo b)))

(define (parse-blueprint lst)
  (match-let ([ (list id ore-bot-ore-cost cly-bot-ore-cost obs-bot-ore-cost
                      obs-bot-cly-cost geo-bot-ore-cost geo-bot-obs-cost) lst ])
    (blueprint id
               (list (robot 'geo state-geo-bots geo-bot-ore-cost 0 geo-bot-obs-cost 1 MAX-INTEGER)
                     (robot 'obs state-obs-bots obs-bot-ore-cost obs-bot-cly-cost 0 3 geo-bot-obs-cost)
                     (robot 'cly state-cly-bots cly-bot-ore-cost 0 0 5 obs-bot-cly-cost)
                     (robot 'ore state-ore-bots ore-bot-ore-cost 0 0 3 (max cly-bot-ore-cost
                                                                            obs-bot-ore-cost
                                                                            geo-bot-ore-cost))))))

(define (prune? move best)
  (let* ([ steps    (state-steps move) ]
         [ possible (+ (state-geo move)
                       (* steps (state-geo-bots move))
                       (/ (* (sub1 steps) steps) 2)) ])
    (< possible (state-geo best))))

(time (check-equal? (part1 (initialize-state 24)) 1725))
(time (check-equal? (part2 (initialize-state 32)) 15510))
