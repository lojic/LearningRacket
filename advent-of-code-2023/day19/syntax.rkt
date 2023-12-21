#lang racket
(require (for-syntax racket/syntax))

(provide set< set>)

(define-syntax (set< stx)
  (syntax-case stx ()
    [(_ c)
     (with-syntax ([ state (format-id stx "state") ]
                   [ obj   (format-id stx "obj")   ]
                   [ c-max (format-id stx "~a-max" #`c) ]
                   [ c-min (format-id stx "~a-min" #`c) ]
                   [ state-c-max (format-id stx "state-~a-max" #`c) ]
                   [ state-c-min (format-id stx "state-~a-min" #`c) ]
                   [ n (format-id stx "n") ])
       #`(values (struct-copy state obj [ c-max (min (state-c-max obj) (sub1 n)) ])
                 (struct-copy state obj [ c-min (max (state-c-min obj) n) ])))]))

(define-syntax (set> stx)
  (syntax-case stx ()
    [(_ c)
     (with-syntax ([ state (format-id stx "state") ]
                   [ obj   (format-id stx "obj")   ]
                   [ c-max (format-id stx "~a-max" #`c) ]
                   [ c-min (format-id stx "~a-min" #`c) ]
                   [ state-c-max (format-id stx "state-~a-max" #`c) ]
                   [ state-c-min (format-id stx "state-~a-min" #`c) ]
                   [ n (format-id stx "n") ])
       #`(values (struct-copy state obj [ c-min (max (state-c-min obj) (add1 n)) ])
                 (struct-copy state obj [ c-max (min (state-c-max obj) n) ])))]))

