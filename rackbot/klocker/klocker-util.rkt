#lang typed/racket/base


(require molis-hai/random-bits
         molis-hai/char-model
         molis-hai/example-model
         racket/runtime-path
         ;racket/match
         ;file/md5
         (only-in racket/list take)
         (only-in racket/file
                  get-preference
                  file->bytes)
         "local-config.rkt")

(require/typed sha
               [sha224 (Bytes -> Bytes)])

(define-runtime-path here ".")

(provide generate-training-str
         log-session-start!
         record-session-data!)



(define TESTING-STR-SALT (file->bytes (build-path here "salt.txt")))


(define TESTING-STR-BITS 56)

;; PASSWORD GENERATION

;; generate the training string for a user (must always be the
;; same string...)
(: generate-training-str (String String -> String))
(define (generate-training-str uid pwd)
  (define bits (generate-password-bits uid pwd))
  (cond [(experimental-group? uid)
         ;; atotc-model strings always start with a space:
         (molis-hai-cleanup (generate-char-pwd atotc-model bits))]
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

;; remove the leading space, add an 'a' to the end if it ends
;; with a space.
(: molis-hai-cleanup (String -> String))
(define (molis-hai-cleanup str)
  (define str2 (substring str 1))
  (cond [(string=? (substring str2 (- (string-length str2) 1)) " ")
         (string-append str2 "a")]
        [else str2]))

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

;; log a session beginning.
(: log-session-start! (String String String -> Void))
(define (log-session-start! uid session-key training-str)
  (log-str! (format "~s" `(session-start ,uid
                                         ,session-key
                                         ,training-str))))

;; record keystroke data from a session.
(: record-session-data! (String (Listof Natural)
                                (Listof Natural)
                                (Listof String) -> Void))
(define (record-session-data! session-key ts ns ps)
  (let loop : Void
    ([ts ts]
     [ns ns]
     [ps ps])
    (cond [(and (null? ts) (null? ns) (null? ps))
           (void)]
          [(or (null? ts) (null? ns) (null? ps))
           (raise-argument-error 'record-session-data!
                                 "lists of same length"
                                 1 session-key
                                 (list ts ns ps))]
          [else (log-str! (format "~s"
                                  (list 'session-data
                                        session-key
                                        (car ts)
                                        (car ns)
                                        (car ps))))
                (loop (cdr ts) (cdr ns) (cdr ps))])))

;; low-level logging


(define log-port (open-output-file fifo-path #:exists 'truncate))

(: log-str! (String -> Void))
(define (log-str! str)
  (fprintf log-port "~a\n" str))

(module+ test
  (require typed/rackunit)
  (check-equal? (molis-hai-cleanup " abcd") "abcd")
  (check-equal? (molis-hai-cleanup " abcd ") "abcd a"))

;; flush every so often (in seconds)
(define FLUSH-INTERVAL 30)

(thread
 (λ ()
   (let loop ()
     (sleep FLUSH-INTERVAL)
     (flush-output log-port)
     (loop))))