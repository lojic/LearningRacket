#lang racket

(provide (struct-out node)
         insert!
         findf
         last-node
         make-queue
         memv
         queue->list
         remove-n!)

(struct node (val next prev) #:mutable #:transparent)

(define (insert! dest nodes)
  (define right (node-next dest))
  (define end (last-node nodes))
  (set-node-next! dest nodes)
  (set-node-prev! nodes dest)
  (set-node-next! end right)
  (set-node-prev! right end))

(define (findf proc obj)
  (define first obj)
  (let loop ([ obj obj ])
    (if (proc (node-val obj))
      obj
      (let ([ next (node-next obj) ])
        (if (eq? first next)
            #f
            (loop next))))))

(define (last-node obj)
  (define head obj)
  (let loop ([ obj obj ])
    (let ([ next (node-next obj) ])
      (if (or (not next) (eq? head next))
          obj
          (loop next)))))

(define (make-queue lst)
  (define head (node (car lst) #f #f))
  (let loop ([ lst (cdr lst) ][ prev head ])
    (cond [ (null? lst)
            (set-node-prev! head prev)
            (set-node-next! prev head)
            head ]
          [ else (let ([ curr (node (car lst) #f prev) ])
                   (set-node-next! prev curr)
                   (loop (cdr lst) curr)) ])))

(define (memv v obj)
  (define first obj)
  (let loop ([ obj obj ])
    (if (eqv? v (node-val obj))
      obj
      (let ([ next (node-next obj) ])
        (if (eq? first next)
            #f
            (loop next))))))

(define (queue->list obj)
  (define head obj)
  (let loop ([ obj (node-next obj) ][ result (list (node-val obj)) ])
    (if (or (not obj) (eq? head obj))
        (reverse result)
        (loop (node-next obj) (cons (node-val obj) result)))))

(define (remove-n! obj n)
  (define left (node-prev obj))
  (define right (let loop ([ obj obj ][ n n ])
                  (if (> n 0)
                      (loop (node-next obj) (sub1 n))
                      obj)))
  (set-node-prev! obj #f)
  (set-node-next! (node-next (node-next obj)) #f)
  (set-node-next! left right)
  (set-node-prev! right left)
  obj)

(module+ test
  (require rackunit)

  (let* ([ lst '(1 2 3 4 5 6 7) ]
         [ obj (make-queue lst) ])                  ;; make-queue
    (check-equal? (queue->list obj) lst)            ;; queue->list

    (let ([ last (last-node obj) ])                 ;; last-node
      (check-equal? (node-val last) 7))

    (let* ([ target (findf (curry equal? 4) obj) ]) ;; findf
      (remove-n! target 3)                          ;; remove-n!
      (check-equal? (queue->list target) '(4 5 6))
      (check-equal? (queue->list obj) '(1 2 3 7))
      (insert! (node-next obj) target)              ;; insert!
      (check-equal? (queue->list obj) '(1 2 4 5 6 3 7)))))
