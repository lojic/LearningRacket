#lang racket

(struct node (parent value left right) #:transparent #:mutable)

(define (part1 nums)
   (for/fold ([ sum (first nums) ] #:result (magnitude sum))
             ([ num (rest nums) ])
     (add+reduce sum num)))

(define (part2 nums)
  (let* ([ combs  (combinations nums 2)              ]
         [ tuples (append combs (map reverse combs)) ])
    (for/fold ([ largest 0 ])
              ([ tuple tuples ])
      (let ([ sum (magnitude (add+reduce (first tuple) (second tuple))) ])
        (if (> sum largest) sum largest)))))

;; --------------------------------------------------------------------------------------------

(define (->list obj)
  (if (pair-node? obj)
      (format "[~a,~a]"
              (->list (node-left obj))
              (->list (node-right obj)))
      (~a (node-value obj))))

(define (add left right)
  (let* ([ obj   (node #f #f #f #f) ]
         [ left  (copy-node left)   ]
         [ right (copy-node right)  ])
    (set-node-parent! left obj)
    (set-node-parent! right obj)
    (set-node-left!  obj left)
    (set-node-right! obj right)
    obj))

(define (add+reduce left right)
  (reduce! (add left right)))

(define (copy-node obj)
  (parse-snail (->list obj)))

(define (create-node snail-number [ parent #f ])
  (if (number? snail-number)
      (node parent snail-number #f #f)
      (let ([ obj (node parent #f #f #f) ])
        (set-node-left!  obj (create-node (first snail-number)  obj))
        (set-node-right! obj (create-node (second snail-number) obj))
        obj)))

(define (explode! obj [level 0])
  (cond [ (regular-node? obj) #f ]
        [ (and (= level 4) (pair-of-regulars? obj))
          (explode-pair! obj)
          #t ]
        [ else (or (explode! (node-left obj)  (add1 level))
                   (explode! (node-right obj) (add1 level))) ]))

(define (explode-pair! obj)
  (let ([ left-regular  (previous-regular obj) ]
        [ right-regular (next-regular obj)     ])
    (when left-regular
      (set-node-value! left-regular (+ (node-value left-regular)
                                       (node-value (node-left obj)))))

    (when right-regular
      (set-node-value! right-regular (+ (node-value right-regular)
                                        (node-value (node-right obj)))))

    (set-node-value! obj 0)
    (set-node-left!  obj #f)
    (set-node-right! obj #f)))

(define (left-most pred? obj)
  (cond [ (not obj)           #f  ]
        [ (pred? obj)         obj ]
        [ (regular-node? obj) #f  ]
        [ else (or (left-most pred? (node-left  obj))
                   (left-most pred? (node-right obj))) ]))

(define (magnitude obj)
  (if (regular-node? obj)
      (node-value obj)
      (+ (* 3 (magnitude (node-left  obj)))
         (* 2 (magnitude (node-right obj))))))

(define (next-regular obj)
  (define (helper parent child)
    (if (not parent)
        #f
        (let ([ right (node-right parent) ])
          (if (and right
               (not (eq? right child)))
              (let ([ obj (left-most regular-node? right) ])
                (if obj
                    obj
                    (helper (node-parent parent) parent)))
              (helper (node-parent parent) parent)))))

  (helper (node-parent obj) obj))

(define (pair-node? obj)
  (and (node-left obj)
       (node-right obj)))

(define (pair-of-regulars? obj)
  (and (pair-node? obj)
       (regular-node? (node-left obj))
       (regular-node? (node-right obj))))

(define (parse file-name)
  (map parse-snail (file->lines file-name)))

(define (parse-snail s)
  (let ([ snail-number (read (open-input-string (string-replace s "," " "))) ])
    (create-node snail-number)))

(define (previous-regular obj)
  (define (helper parent child)
    (if (not parent)
        #f
        (let ([ left (node-left parent) ])
          (if (and left
               (not (eq? left child)))
              (let ([ obj (right-most regular-node? left) ])
                (if obj
                    obj
                    (helper (node-parent parent) parent)))
              (helper (node-parent parent) parent)))))

  (helper (node-parent obj) obj))

(define (reduce! obj)
  (cond [ (explode! obj) (reduce! obj) ]
        [ (split! obj)   (reduce! obj) ]
        [ else           obj           ]))

(define (regular-node? obj)
  (node-value obj))

(define (right-most pred? obj)
  (cond [ (not obj)           #f  ]
        [ (pred? obj)         obj ]
        [ (regular-node? obj) #f  ]
        [ else (or (right-most pred? (node-right obj))
                   (right-most pred? (node-left  obj))) ]))

(define (split! obj)
  (define (at-least-10? obj)
    (and (regular-node? obj)
         (>= (node-value obj) 10)))

  (let ([ big (left-most at-least-10? obj) ])
    (cond [ big
            (split-regular! big)
            #t ]
          [ else #f])))

(define (split-regular! obj)
  (let*-values ([ (quo rem) (quotient/remainder (node-value obj) 2) ])
    (set-node-value! obj #f)
    (set-node-left!  obj (node obj quo #f #f))
    (set-node-right! obj (node obj (+ quo rem) #f #f))))

;; Tests --------------------------------------------------------------------------------------

(module+ test
  (require rackunit)
  (let ([ nums (parse "day18.txt") ])
    (check-equal? (part1 nums) 3665)
    (check-equal? (part2 nums) 4775)))
