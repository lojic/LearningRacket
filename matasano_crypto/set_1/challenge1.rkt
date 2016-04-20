#lang racket

(require openssl/sha1)
(require net/base64)

(define (hex->base64 hex-str)
  (base64-encode (hex-string->bytes hex-str)))

;; ---------------------------------------------------------------------------------------------------
;; Tests
;; ---------------------------------------------------------------------------------------------------
(module+ test
  (require rackunit)
  (define hex-str "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d")
  (define base64-str #"SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t\r\n")
  (check-equal? base64-str (hex->base64 hex-str)))
