#lang racket
(require "../lojic.rkt")

; Heuristic for determining password strength:
; Possible Levels:
;    [ 'Too short', 'Weak', 'Fair', 'Good', 'Strong']
;
; In the logarithm expressions below, X refers to the size of
; the character set:
;   digits only = 10
;   lowercase only = 26
;   uppercase only = 26
;   punctuation only = 32
;   combinations can yield { 10, 26, 32, 36, 42, 52, 58, 62, 68, 84, 94 }
;
; 'Too short'
; ===========
; length < 6
;
; 'Weak'
; ======
; (password.gsub(/\s/, '') == user_name.gsub(/\s/, '')) OR
; (length < ( 10.613 / log(X) ) ) OR  e.g. 8 chars of lower
; (composed of exactly one dictionary word)
; (composed of a word + single non-alpha)
; (composed of a single non-alpha + word)
;
; 'Fair'
; ======
; (length < ( 12.871 / log(X) ) )  e.g. 8 chars of lower & upper
; (composed of a word + digits)
; (composed of digits + word)
;
; 'Good'
; ======
; (length < ( 16.303 / log(X) ) )  e.g. 8 chars of lower, upper & punctuation
; (composed of 2 words separated by a non-alpha)
; (composed of 2 words)
;
; 'Strong'
; ========
; not disqualified by above
;
; dict_fn should be a function that accepts a word and returns true
;         if the word is found in a dictionary
;
(define min-pswd-length 6)

(define (dummy-dict word)
  (or
   (string=? "giraffe" word)
   (string=? "airplane" word)
   (string=? "exterminate" word)))

(define (password-strength-level pswd user [dict-fn (λ (word) #f)])
  (if (< (string-length pswd) min-pswd-length) 
      "Too short"
      (let ([charset-size (compute-charset pswd)])
        (cond [ (sub-fair   pswd charset-size user dict-fn) "Weak"   ]
              [ (sub-good   pswd charset-size dict-fn)      "Fair"   ]
              [ (sub-strong pswd charset-size dict-fn)      "Good"   ]
              [ else                                        "Strong" ]))))

(define (starts-or-ends-with-word str dict-fn non-word)
  (let ([lower (string-downcase str)])
    (ormap (λ (regex) 
             (let ([result (regexp-match regex lower)])
               (and result (dict-fn (cadr result)))))
           (list (pregexp (string-append "^([a-z]+)" non-word "$"))
                 (pregexp (string-append "^" non-word "([a-z]+)$"))))))

(define (sub-fair pswd charset-size user dict-fn)
  (or 
   (string=? (string-replace pswd #px"\\s" "") (string-replace user #px"\\s" ""))
   (< (string-length pswd) (/ 10.613 (round (log10 charset-size))))
   (dict-fn pswd)
   (starts-or-ends-with-word pswd dict-fn "[^a-z]")))

(define (sub-good pswd charset-size dict-fn)
  (or
   (< (string-length pswd) (/ 12.871 (round (log10 charset-size))))
   (starts-or-ends-with-word pswd dict-fn "\\d+")))

(define (starts-and-ends-with-word str dict-fn)
  (let* ([lower (string-downcase str)]
         [result (regexp-match #px"^([a-z]+)[^a-z]([a-z]+)$" lower)])
    (and result (dict-fn (second result)) (dict-fn (third result)))))

(define (consists-of-2-words str dict-fn)
  (let ([lower (string-downcase str)]
        [length (string-length str)])
    (for/or ([i (range 1 (- length 1))])
      (and
       (dict-fn (substring lower 0 i))
       (dict-fn (substring lower i length))))))
  
(define (sub-strong pswd charset-size dict-fn)
  (or
   (< (string-length pswd) (/ 16.303 (round (log10 charset-size))))
   (starts-and-ends-with-word pswd dict-fn)
   (consists-of-2-words pswd dict-fn)))
 
(define (compute-charset str)
  (+
   (if (regexp-match #px"[0-9]" str) 10 0)
   (if (regexp-match #px"[a-z]" str) 26 0)
   (if (regexp-match #px"[A-Z]" str) 26 0)
   (if (regexp-match #px"[^0-9a-zA-Z]" str) 32 0)))
   
   
