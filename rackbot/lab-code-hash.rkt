#lang racket

(require sha)

;; given two bytes, return the list of bytes in between (inclusive)
(define (char-range start finish)
 (range (first (bytes->list start))
        (add1 (first (bytes->list finish)))))

(define regular-bytes
  (append
   (char-range #"a" #"z")
   #;(char-range #"A" #"Z")
   (char-range #"0" #"9")
   (bytes->list #".-")))

(define CHAR-BITS 5)

;; check it...
(unless (<= (expt 2 CHAR-BITS)
            (length regular-bytes))
  (error 'lab-code-hash
         "not enough bytes to choose from"))


;; codes will only use the first 6*5 = 30 bits of
;; the hash. This gives students a 1 in 2^30
;; chance of guessing one correctly.

(define CHARS 6)

(define SEKRIT
  (first
   (file->lines
    (build-path (find-system-path 'home-dir) ".ssh" "lab-code-secret"))))

(define (hash-user-lab id qtr labnum)
  
  (define sha-bytes
    (sha256
     (string->bytes/utf-8
      (format "user: ~v id, qtr: ~v, labnum: ~v, secret, ~v"
              id qtr labnum SEKRIT))))
  ;; as long as char-bits is <= 8 and CHARS is
  ;; <= 256, we can just take CHARS bytes from the
  ;; sha-256 and mask off the upper bits.
  (list->bytes
   (for/list ([bit-source (take (bytes->list sha-bytes)
                                CHARS)])
     (list-ref regular-bytes
               (bitwise-and (sub1 (expt 2 CHAR-BITS)) bit-source)))))

(hash-user-lab "clements" 2158 3)
(for/list ([lab 10])
  (hash-user-lab "clements" 2158 lab))