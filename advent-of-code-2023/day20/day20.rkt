#lang racket
(require "../advent.rkt" data/queue)

(struct component   (dests))
(struct broadcaster component ())
(struct conjunction component (inputs))
(struct flip-flop   component (on))
(struct message     (from to pulse))

(define network
  (let ([ lines (parse-aoc 20 (λ (s) (string-split s #px"( -> )|(, )"))) ])
    (make-immutable-hash
     (map (match-lambda
            [ (list key dests ...)
              (cond [ (string-prefix? key "%") (cons (substring key 1) (flip-flop dests #f)) ]
                    [ (string=? key "broadcaster") (cons key (broadcaster dests)) ]
                    [ (string-prefix? key "&")
                      (let ([ key (substring key 1) ])
                        (cons key (conjunction dests (~> (filter (λ (line) (member key (cdr line))) lines)
                                                         (map (λ (line) (cons (substring (car line) 1) #f)) _)
                                                         (make-immutable-hash _))))) ]) ])

          lines))))

(define (process-message queue network from to pulse)
  (define (set-flip mod on?)
    (hash-set network to (struct-copy flip-flop mod [on on?])))

  (define (send-pulses mod from pulse)
    (for ([ dest (in-list (component-dests mod)) ])
      (enqueue! queue (message from dest pulse))))

  (let ([ mod (hash-ref network to #f) ])
    (cond [ (broadcaster? mod)
            (send-pulses mod to pulse)
            network ]

          [ (flip-flop? mod)
            (if (eq? pulse 'high)
                network ; ignore
                (cond [ (flip-flop-on mod)
                        (send-pulses mod to 'low)
                        (set-flip mod #f) ]
                      [ else
                        (send-pulses mod to 'high)
                        (set-flip mod #t) ])) ]

          [ (conjunction? mod)
            (let* ([ hsh    (hash-set (conjunction-inputs mod) from pulse) ]
                   [ pulse* (if (andmap (λ (pulse) (eq? pulse 'high)) (hash-values hsh))
                                'low
                                'high) ])
              (send-pulses mod to pulse*)
              (hash-set network to (struct-copy conjunction mod [ inputs hsh ]))) ]
          [ else network ])))

(define (button queue network)
  (define (helper queue network)
    (if (queue-empty? queue)
        network
        (match-let ([ (message from to pulse) (dequeue! queue) ])
          (when (and (string=? "rx" to)
                     (eq? pulse 'low))
            (printf "got it!\n"))
          (helper queue (process-message queue
                                        (hash-set network pulse (add1 (hash-ref network pulse 0)))
                                        from
                                        to
                                        pulse)))))

  (enqueue! queue (message #f "broadcaster" 'low))
  (helper queue network))

(define (part1)
  (let ([ network (iterate (curry button (make-queue)) network 1000) ])
    (* (hash-ref network 'low  0)
       (hash-ref network 'high 0))))

;; Tests --------------------------------------------------------------------------------------

(check-equal? (part1) 737679780)
