#lang racket

(define MAX-AGILITY  35)
(define MAX-HEALTH   35)
(define MAX-STRENGTH 35)

(struct orc-world  (player lom attack#))
(struct player     (health agility strength) #:mutable)

(struct monster ([health #:mutable]))
(struct brigand monster ())
(struct hydra   monster ())
(struct orc     monster (club))
(struct slime   monster (sliminess))

(define (player-update! setter selecter mx)
  (lambda (player delta)
    (setter player (interval+ (selector player) delta mx))))

(define player-agility+
  (player-update! set-player-agility! player-agility MAX-AGILITY))

(define player-health+
  (player-update! set-player-health! player-health MAX-HEALTH))

(define player-strength+
  (player-update! set-player-strength! player-strength MAX-STRENGTH))
