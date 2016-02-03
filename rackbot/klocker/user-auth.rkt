#lang typed/racket/base

(require racket/runtime-path
         racket/match
         racket/file
         threading)

(require/typed file/md5
               [md5 (Bytes -> Bytes)])

(provide valid-userid?
         valid-password?
         user-consented?
         user-consents!)

(define-runtime-path here ".")

(define PASSWORD-FILE (build-path here "users.rktd"))
(define CONSENT-FILE (build-path here "consent.rktd"))

;; PASSWORDS

;; is this a legal userid?
(: valid-userid? (String -> Boolean))
(define (valid-userid? str)
  (match (regexp-match #px"^[0-9a-zA-Z_]*$" str)
    [#f #f]
    [other #t]))

(: get-user-pwd-hash (String -> (U False String)))
(define (get-user-pwd-hash username)
  (match (get-preference (string->symbol username) (lambda () #f) 'timestamp
                         PASSWORD-FILE)
    [(list-rest (? string? s) _1) s]
    [#f #f]
    [other (error 'get-user-pwd-hash
                  "internal error 2016-01-23")]))

;; check to see that this uid/pwd pair matches one in
;; users.rktd
(: valid-password? (String String -> Boolean))
(define (valid-password? uid pwd)
  (match (get-user-pwd-hash uid)
    [#f #f]
    [hashstr (~> pwd
                 string->bytes/utf-8
                 md5
                 bytes->string/latin-1
                 (equal? hashstr))]))

;; CONSENT

(: user-consented? (String -> Boolean))
(define (user-consented? username)
  (match (get-preference (string->symbol username) (lambda () #f) 'timestamp
                         CONSENT-FILE)
    [(? boolean? b) b]
    [other (error 'user-consented? "internal error 56b129f0")]))

(: user-consents! (String -> Void))
(define (user-consents! username)
  (thread
   (λ ()
     (let loop ((fail-count 0))
       (cond [(<= MAX-TRIES fail-count)
              (log-error (format "failed to write consented pref for user ~a"
                                 username))]
             [else
              (put-preferences (list (string->symbol username))
                               (list #t)
                               (λ (lockfile)
                                 (sleep (+ BASE-SLEEP
                                           (random SLEEP-RANGE))))
                               CONSENT-FILE)]))))
  (void))

;; try this many times before giving up:
(define MAX-TRIES 5)
;; wait at least this long before trying to write the pref again:
(define BASE-SLEEP 300)
;; adjust the sleep upward by a random # of secs in [0..SLEEP-RANGE]
;; to keep bouncing requests from continuing to bounce.
(define SLEEP-RANGE 60)


(module+ test
  (require typed/rackunit)
  
  (check-not-exn (λ () (get-user-pwd-hash "clements")))

  (check-true (valid-password? "guest" "40"))

  (check-true (user-consented? "consenter"))
  (check-false (user-consented? "non-consenter"))

  (check-true (valid-userid? "ath3224_3728hhho"))
  (check-false (valid-userid? "ath3224_3728h#hho")))



