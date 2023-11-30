#lang racket

#;(provide (contract-out #:∃ queue
          [ list->queue         (-> list? queue)                         ]
          [ queue->list         (-> queue list?)                         ]
          [ queue-copy          (-> queue queue)                         ]
          [ queue-count         (-> queue exact-positive-integer?)       ]
          [ queue-eq?           (-> queue queue boolean?)                ]
          [ queue-equal?        (-> queue queue boolean?)                ]
          [ queue-findf         (-> procedure? queue queue)              ]
          [ queue-insert!       (-> queue any/c queue)                   ]
          [ queue-insert-queue! (-> queue queue queue)                   ]
          [ queue-last          (-> queue queue)                         ]
          [ queue-memv          (-> any/c queue (or/c queue #f))         ]
          [ queue-next          (-> queue queue)                         ]
          [ queue-previous      (-> queue queue)                         ]
          [ queue-remove-n!     (-> queue exact-positive-integer? queue) ]
          [ queue-val           (-> queue any)                           ]))

(provide list->queue         
         queue->list         
         queue-copy          
         queue-count         
         queue-eq?           
         queue-equal?        
         queue-findf         
         queue-insert!       
         queue-insert-queue! 
         queue-last          
         queue-memv          
         queue-next          
         queue-previous      
         queue-remove-n!     
         queue-val)

;; Implementation details hidden through the (#:∃ queue) contract
(struct node (val next prev) #:mutable #:transparent)


;; (queue-copy head) -> queue
;; head : queue
;;
;; Return a copy of the queue.
(define (queue-copy head)
  (list->queue (queue->list head)))

;; (queue-count head) -> exact-positive-integer?
;; head : queue
;;
;; Return the number of elements in the queue
(define (queue-count head)
  (let loop ([ obj head ][ count 1 ])
    (let ([ next (node-next obj) ])
      (if (eq? head next)
          count
          (loop next (add1 count))))))

;; (queue-insert! head val) -> queue
;; head : queue
;; val  : any/c
;;
;; Insert a new node containing val directly after head and return
;; head.
(define (queue-insert! head val)
  (queue-insert-queue! head (list->queue (list val)))
  head)

;; (queue-insert-queue! head nodes) -> queue
;; head  : queue
;; nodes : queue
;;
;; Insert nodes directly after head.
(define (queue-insert-queue! head q)
  (let ([ right (node-next head) ]
        [ end   (queue-last q)   ])

    (set-node-next! head q)
    (set-node-prev! q head)
    (set-node-next! end right)
    (set-node-prev! right end)

    head))

;; (queue-findf pred? head) -> queue
;; pred? : procedure?
;; head  : queue
;;
;; Search for a node satisfying pred?; if none found, return #f
(define (queue-findf pred? head)
  (define first head)

  (let loop ([ obj head ])
    (if (pred? (node-val obj))
      obj
      (let ([ next (node-next obj) ])
        (if (eq? first next)
            #f
            (loop next))))))

;; (queue-last head) -> queue
;; head : queue
;;
;; Return the last node of the queue, starting from head
(define (queue-last head)
  (node-prev head))

;; (list->queue lst) -> queue
;; lst : list?
;;
;; Construct a circular queue from a list
(define (list->queue lst)
  (define head (node (car lst) #f #f))
  (let loop ([ lst (cdr lst) ][ prev head ])
    (cond [ (null? lst)
            (set-node-prev! head prev)
            (set-node-next! prev head)
            head ]
          [ else (let ([ curr (node (car lst) #f prev) ])
                   (set-node-next! prev curr)
                   (loop (cdr lst) curr)) ])))

;; (queue-memv v head) -> (or/c queue #f)
;; v    : any/c
;; head : queue
;;
;; Search queue for a node containing a value eqv? to v. if found,
;; return the queue starting at that node; otherwise, return #f
(define (queue-memv v head)
  (let loop ([ obj head ])
    (if (eqv? v (node-val obj))
      obj
      (let ([ next (node-next obj) ])
        (if (eq? head next)
            #f
            (loop next))))))

;; (queue-next head) -> queue
;; head : queue
;;
;; Return the queue following head
(define (queue-next head)     (node-next head))

;; (queue-previous head) -> queue
;; head : queue
;;
;; Return the queue preceding head
(define (queue-previous head) (node-prev head))

;; (queue-eq? q1 q2) -> boolean?
;; q1 : queue
;; q2 : queue
;;
;; Indicate whether the two queues are the same queue.
(define (queue-eq? q1 q2)
  (eq? q1 q2))

;; (queue-equal? q1 q2) -> boolean?
;; q1 : queue
;; q2 : queue
;;
;; Indicate whether the two queues are equal.
(define (queue-equal? q1head q2head)
  (if (equal? (queue-val q1head) (queue-val q2head))
      (let loop ([ q1 (queue-next q1head) ]
                 [ q2 (queue-next q2head) ])
        (cond [ (eq? q1 q1head) (eq? q2 q2head) ]
              [ (eq? q2 q2head) #f ]
              [ else (if (equal? (queue-val q1) (queue-val q2))
                         (loop (queue-next q1) (queue-next q2))
                         #f) ]))
      #f))

;; (queue->list head) -> list?
(define (queue->list head)
  (let loop ([ obj (node-next head) ][ result (list (node-val head)) ])
    (if (eq? head obj)
        (reverse result)
        (loop (node-next obj) (cons (node-val obj) result)))))

;; (queue-remove-n! head n) -> queue
;; head : queue
;; n    : exact-positive-integer?
;;
;; Extracts a circular queue of n elements beginning at head and
;; returns it. The remaining elements are made into a circular queue -
;; it is presumed there exists a reference to one of the remaining
;; nodes. It is not permissible to remove all elements.
(define (queue-remove-n! head n)
  ;; Node to the left of head
  (define left (node-prev head))

  ;; Node to the right of the last node to remove
  (define right (let loop ([ obj head ][ n n ])
                  (if (> n 0)
                      (let ([ next (node-next obj) ])
                        (if (eq? next left)
                            (error "Cannot remove all nodes from queue")
                            (loop next (sub1 n))))
                      obj)))

  ;; Make the queue to be removed circular
  (set-node-prev! head (node-prev right))
  (set-node-next! (node-prev right) head)

  ;; Make the remaining nodes into a circular queue
  (set-node-next! left right)
  (set-node-prev! right left)

  ;; Return the extracted queue
  head)

;; (queue-val head) -> any
;; head : queue
;;
;; Return the value of the node at the head of the queue.
(define (queue-val head) (node-val head))

;; --------------------------------------------------------------------------------------------
;; Tests
;; --------------------------------------------------------------------------------------------

(module+ test
  (require rackunit)

  (define sample-queue (list->queue '(1 2 3 4 5 6 7 8 9 10)))

  ;; queue-copy -------------------------------------------------------------------------------------

  (let ([ other (queue-copy sample-queue) ])
    (check-equal? (queue-count other) 10)
    (check-equal? (queue-val other) 1)
    (check-equal? (queue-val (queue-last other)) 10))

  ;; queue-count ------------------------------------------------------------------------------------

  (check-equal? (queue-count sample-queue) 10)

  ;; queue-insert! ----------------------------------------------------------------------------------

  (let* ([ q     (queue-copy sample-queue) ]
         [ three (queue-next (queue-next q))     ])
    (queue-insert! three 100)
    (check-equal? (queue-count q) 11)
    (check-equal? (queue-val q) 1)
    (check-equal? (queue-val three) 3)
    (check-equal? (queue-val (queue-next three)) 100)
    (check-equal? (queue-val (queue-next (queue-next three))) 4))

  ;; queue-insert-queue! ----------------------------------------------------------------------------

  (let* ([ q         (queue-copy sample-queue)          ]
         [ last-node (queue-last q)                     ]
         [ nodes     (list->queue '("foo" "bar")) ])
    (queue-insert-queue! last-node nodes)
    (check-equal? (queue-count q) 12)
    (check-equal? (queue-val (queue-last q)) "bar"))

  ;; queue-findf ------------------------------------------------------------------------------------

  (let ([ pred? (λ (v) (> v 5)) ])
    (check-equal? (queue-val (queue-findf pred? sample-queue)) 6)
    (check-equal? (queue-val (queue-findf even? sample-queue)) 2))

  ;; queue-last -------------------------------------------------------------------------------------

  (check-equal? (queue-val (queue-last sample-queue)) 10)

  ;; list->queue ------------------------------------------------------------------------------

  (let ([ other (list->queue '(1 2 3 4 5 6 7 8 9 10)) ])
    (check-not-false (queue-equal? sample-queue other)))

  ;; queue-memv -------------------------------------------------------------------------------------

  (check-eq? (queue-memv 3 sample-queue) (queue-next (queue-next sample-queue)))

  ;; queue-next -------------------------------------------------------------------------------------

  (check-equal? (queue-val (queue-next sample-queue)) 2)

  ;; queue-previous ---------------------------------------------------------------------------------

  (check-equal? (queue-val (queue-previous sample-queue)) (queue-val (queue-last sample-queue)))

  ;; queue-equal? -----------------------------------------------------------------------------

  (let ([ q1 (list->queue '(1 3 5))                  ]
        [ q2 (list->queue (list (add1 0) (+ 1 2) 5)) ]
        [ q3 (list->queue '(1 2 3 4 5))              ])
    (check-not-false (queue-equal? q1 q2))
    (check-false (queue-equal? sample-queue q3)))

  ;; queue->list ------------------------------------------------------------------------------

  (check-equal? (queue->list sample-queue) '(1 2 3 4 5 6 7 8 9 10))

  ;; queue-remove-n! --------------------------------------------------------------------------------

  (let* ([ q1 (list->queue '(1 2 3 4 5)) ]
         [ q2 (queue-remove-n! (queue-next q1) 3)    ])
    (check-equal? (queue-count q1) 2)
    (check-equal? (queue-count q2) 3)
    (check-equal? (queue->list q1) '(1 5))
    (check-equal? (queue->list q2) '(2 3 4)))

  ;; queue-val ----------------------------------------------------------------------------------

  (check-equal? (queue-val sample-queue) 1)
  (check-equal? (queue-val (queue-last sample-queue)) 10)
  (check-equal? (queue-val (queue-next sample-queue)) 2)

  )
