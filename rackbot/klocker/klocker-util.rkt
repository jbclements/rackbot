#lang typed/racket/base


(require molis-hai/random-bits
         molis-hai/char-model
         molis-hai/example-model
         racket/runtime-path
         racket/match
         file/md5
         (only-in racket/list take)
         (only-in racket/file get-preference))

(require/typed net/base64
               [base64-encode (Bytes -> Bytes)])

(require/typed sha
               [sha224 (Bytes -> Bytes)])

(define-runtime-path here ".")

(provide generate-session-key
         generate-training-str
         log-session-start!
         record-session-data!)

(define-logger data)

;; 64 bits should give a very low collision likelihood
;; over 100K trials. (about 1 in 55K)
(define SESSION-KEY-BYTES 8)

(define TESTING-STR-SALT #"m~4gB\332\367\6")

(define TESTING-STR-BITS 56)

;; SESSION KEY GENERATION

(: generate-session-key (-> String))
(define (generate-session-key)
  (bytes->string/utf-8
   (base64-encode (random-bytes SESSION-KEY-BYTES))))

;; PASSWORD GENERATION

;; generate the training string for a user (must always be the
;; same string...)
(: generate-training-str (String String -> String))
(define (generate-training-str uid pwd)
  (define bits (generate-password-bits uid pwd))
  (cond [(experimental-group? uid)
         ;; atotc-model strings always start with a space:
         (substring (generate-char-pwd atotc-model bits) 1)]
        [else
         (generate-char-pwd base-model bits)]))

;; generate the set of bits to be used in generating the training string
(: generate-password-bits (String String -> (Listof Boolean)))
(define (generate-password-bits uid pwd)
  (take (bytes->bits
          (bytes->list
           (sha224 (bytes-append (string->bytes/utf-8 uid)
                                 (string->bytes/utf-8 pwd)
                                 TESTING-STR-SALT))))
         TESTING-STR-BITS))

;; divide the class into experimental and control groups
;; based on a hash of their name (and some tweaking to get a good
;; split)

;; is this student in the experimental group?
(: experimental-group? (String -> Boolean))
(define (experimental-group? u)
  (define uid-string (string->bytes/utf-8 u))
  (define hashed-bytes (bytes->list (sha224 uid-string)))
  ;; fourth byte turns out to give a good split (36/38)
  (= (modulo (list-ref hashed-bytes 3) 2) 0))

;; LOGGING

(: log-session-start! (String String Integer -> Void))
(define (log-session-start! uid session-key user-timestamp)
  (define local-timestamp (inexact->exact
                           (floor (current-inexact-milliseconds))))
  (printf "log: ~s\n" `(session-start ,uid
                                      ,session-key
                                      ,user-timestamp
                                    ,local-timestamp)))

(: record-session-data! (String String (Listof Natural)
                                (Listof Natural)
                                (Listof String) -> Void))
(define (record-session-data! uid session-key ts ns ps)
  (let loop : Void
    ([ts ts]
     [ns ns]
     [ps ps])
    (cond [(and (null? ts) (null? ns) (null? ps))
           (void)]
          [(or (null? ts) (null? ns) (null? ps))
           (raise-argument-error 'record-session-data!
                                 "lists of same length"
                                 2 uid session-key
                                 (list ts ns ps))]
          [else (printf (format "log: ~s\n"
                                       (list 'session-data
                                             uid session-key
                                             (car ts)
                                             (car ns)
                                             (car ps))))
                (loop (cdr ts) (cdr ns) (cdr ps))])))