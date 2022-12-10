#lang racket
(require "../advent.rkt")

(define (solve knots)
  (define (dir->num dir)
    (match dir
      [ "U"  0-1i ]
      [ "R"  1+0i ]
      [ "D"  0+1i ]
      [ "L" -1+0i ]))

  (define (step dir n snake positions)
    (let* ([ h (+ dir (car snake))         ]
           [ t (update-tail h (cdr snake)) ])
      (values (sub1 n)
              (cons h t)
              (set-add positions (last t)))))

  (define (update-tail h snake)
    (define (move-one h t)
      (let ([ delta (- h t) ])
        (if (< (magnitude delta) 2)
            t
            (+ t (clamp -1-i 1+i delta)))))

    (if (null? snake)
        '()
        (let ([ t (move-one h (car snake)) ])
          (cons t (update-tail t (cdr snake))))))

  (let next-command ([ in        (parse-aoc 9 atoms) ]
                     [ snake     (make-list knots 0) ]
                     [ positions (set 0)             ])
    (if (null? in)
        (set-count positions)
        (match-let ([ (list dir n) (car in) ])
          (let next-step ([ n         n         ]
                          [ snake     snake     ]
                          [ positions positions ])
            (if (= n 0)
                (next-command (cdr in) snake positions)
                (call-with-values (λ ()
                                    (step (dir->num dir) n snake positions))
                                  next-step)))))))

(solve 2)  ; Part 1
(solve 10) ; Part 2
