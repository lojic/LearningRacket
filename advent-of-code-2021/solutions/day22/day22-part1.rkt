#lang racket

(define (parse file-name)
  (define pat #px"^(on|off) x=([-0-9]+)..([-0-9]+),y=([-0-9]+)..([-0-9]+),z=([-0-9]+)..([-0-9]+)$")

  (for/list ([ line (in-list (file->lines file-name)) ])
    (let ([ groups (regexp-match pat line) ])
      (let ([ on?  (string=? "on" (second groups))    ]
            [ nums (map string->number (cddr groups)) ])
        (cons (if on? 1 0) nums)))))

(define (solve steps)
  (for*/sum ([ x (in-inclusive-range -50 50) ]
             [ y (in-inclusive-range -50 50) ]
             [ z (in-inclusive-range -50 50) ])
    (for/fold ([ cube 0 ])
              ([ step (in-list steps) ])
      (let ([ state (car step) ])
        (if (and (<= (second step) x (third step))
                 (<= (fourth step) y (fifth step))
                 (<= (sixth step) z (seventh step)))
            state
            cube)))))
      
(time (solve (parse "day22.txt")))
