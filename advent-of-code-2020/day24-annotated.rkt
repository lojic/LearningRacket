#lang racket
(require threading)

;; Model the directions with complex numbers
;;     (NW )   (NE )              (-0.5+1i) (+0.5+1i)
;; ( W )   (REF)   ( E ) ==> (-1.0+0i)   (REF)   (+1.0+0i)
;;     (SW )   (SE )              (-0.5-1i) (+0.5-1i)
(define directions (hash "nw" (+ -0.5 +i)   "ne" (+ 0.5 +i)
                      "w" -1          #;(REF)            "e" 1
                         "sw" (+ -0.5 -i)   "se" (+ 0.5 -i)))

;; Returns a list of the form:
;; '( (-0.5+i 1 0.5-i -1 1 0.5+i ...) Line 1 of input
;;    (1 0.5-i -0.5+i 0.5+i -1 1 ...) Line 2 of input
;;    ... )                           etc.
(define (parse-input fname)
  ;; For each line in the input
  (for/list ([ line (in-list (file->lines fname)) ])
     ;; Split the line into a list of directions e.g. '("NW" "E" "SW" ...)
    (~> (regexp-match* #px"(e|se|sw|w|nw|ne)" line)
     ;; Convert each direction into the corresponding complex number
        (map (curry hash-ref directions) _))))

(define (part1 fname)
  ;; For each list of directions
  (for/fold ([ hsh (hash) ]) ([ dir (parse-input fname) ])
    ;; Compute the hash key (tile position) by simply summing all the complex numbers
    (let ([ key (apply + dir) ])
      (if (hash-ref hsh key #f)
          ;; If the tile is found, it's a black tile, so "flip" it by
          ;; removing it from the hash
          (hash-remove hsh key)
          ;; If the tile is not found, it's a white tile, so "flip" it
          ;; by adding it to the hash
          (hash-set hsh key #t)))))

;; For part2, we'll model the floor with a hash table containing black
;; tiles, and we'll assume all other tiles are white. For each
;; iteration, we simply check <all tiles> against the puzzle
;; conditions and either flip tiles from one color to the other, or
;; leave them as is. We compute <all tiles> as the union of the black
;; tiles and tiles adjacent to the black tiles.
(define (part2 fname num-days)
  ;; Produce a list of adjacent tile keys by adding each of the
  ;; directions (complex number) to the tile's key
  (define (adjacent-keys key)
    (map (curry + key)
         (hash-values directions)))

  ;; Given a list of keys, produce a list that contains each of those
  ;; keys with its adjacent keys
  (define (all-keys keys)
    (foldl (λ (key result)
             (append (cons key (adjacent-keys key)) result))
           '()
           keys))

  ;; Remove duplicatess from a list of keys by adding them to a set,
  ;; and then converting the set to a list
  (define (key-set keys)
    (set->list
     (foldl (λ (key s) (set-add s key))
            (set)
            keys)))

  ;; For any relevant key, determine if it should be black or
  ;; white. If it's black, functionally set the corresponding key in
  ;; the new hash
  (define (flip-em h0)
    (for/fold ([ h (hash) ])
              ([ key (key-set (all-keys (hash-keys h0))) ])
      (if (is-black? h0 key)
          (hash-set h key #t)
          h)))

  ;; Determine whether the tile corresponding to <key> should be
  ;; black, according to the game rules.
  (define (is-black? h0 key)
    (let ([ black (hash-ref h0 key #f) ]
          [ num-black (for/sum ([ key (adjacent-keys key) ]) (if (hash-ref h0 key #f) 1 0)) ])
      (cond [; If the tile is black and the number of adjacent black tiles is 0 or more than 2
             (and black (or (= num-black 0) (> num-black 2)))
             ; Indicate white
              #f ]
            [; If the tile is white and the number of adjacent black tiles is 2
             (and (not black) (= num-black 2))
             ; Indicate black
              #t ]
            ; Otherwise, leave the color as is
            [ else black ])))

  ;; Get the floor state from part 1, and initialize the day counter to zero
  (let loop ([ hsh (part1 fname) ][ day 0 ])
    (if (>= day num-days)
        ;; If we've computed the specified number of days, return the hash table
        hsh
        ;; Else, flip the appropriate tiles and recur
        (loop (flip-em hsh) (add1 day)))))

(module+ test (require rackunit)
  (check-equal? (hash-count (part1 "day24.txt")) 360)
  (check-equal? (time (hash-count (part2 "day24.txt" 100))) 3924))
