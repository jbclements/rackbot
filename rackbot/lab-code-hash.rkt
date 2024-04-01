#lang racket

(require sha)

(provide
 (contract-out
  [compute-hash (-> string? qtr-num? string? lab-num? bytes?)]))

;; a legal quarter number
(define (qtr-num? x)
  (and (exact-integer? x)
       (<= 1000 x)
       (= 0 (modulo x 2))
       (not (= 0 (modulo x 10)))))

;; a legal lab number. There's no reason for
;; the restriction, it's just intended to catch
;; accidental transposes of quarter-number and
;; lab-number
(define lab-num? (integer-in 1 20))

;; given two bytes, return the list of bytes in between (inclusive)
(define (char-range start finish)
 (range (first (bytes->list start))
        (add1 (first (bytes->list finish)))))


(define o-char (first (bytes->list #"o")))
(define regular-bytes
  (append
   (bytes->list #"023456789")
   (remove o-char (char-range #"a" #"z"))
   ))

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

;; given an id, a quarter, and a lab number, generate the secret code
;; that the student should enter:
(define (compute-hash id qtr course labnum) 
  (define sha-bytes
    (sha256
     (string->bytes/utf-8
      (format "user: ~v id, qtr: ~v, course: ~v, labnum: ~v, secret, ~v"
              id qtr labnum course SEKRIT))))
  ;; as long as char-bits is <= 8 and CHARS is
  ;; <= 256, we can just take CHARS bytes from the
  ;; sha-256 and mask off the upper bits.
  (list->bytes
   (for/list ([bit-source (take (bytes->list sha-bytes)
                                CHARS)])
     (list-ref regular-bytes
               (bitwise-and (sub1 (expt 2 CHAR-BITS)) bit-source)))))



