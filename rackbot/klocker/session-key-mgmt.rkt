#lang typed/racket/base

(require racket/match
         threading
         molis-hai/random-bits)

(require/typed net/base64
               [base64-encode (Bytes -> Bytes)])

;; this module maintains a bidirectional mapping between
;; users and session-keys, for use in validating session keys and
;; ensuring that there aren't multiple sessions active for a single
;; user.

(provide maybe-start-user-session
         session-active?
         session-info
         generate-session-key
         valid-session-key?
         )

;; note that restarting the server will reset all tables.

(define-type Userid String)
(define-type SessionKey String)
(define-type TrainingStr String)
;; representing a time in UTC seconds:
(define-type Timestamp Integer)

;; maps the userid to the most recent session, and the time it began:
(define-type UserMap (HashTable Userid (List SessionKey Timestamp)))

;; maps a session id to a user and a training string:
(define-type SessionKeyMap (HashTable SessionKey (List Userid TrainingStr)))

;; 64 bits should give a very low collision likelihood
;; over 100K trials. (about 1 in 55K)
(define SESSION-KEY-BYTES 8)

;; these are base64-encoded, so they get longer:
(define MAX-SESSION-KEY-LENGTH 16)

;;;;;;;
;;
;;  SESSION KEY GENERATION
;;
;;;;;;;

;; generate a fresh session key
(: generate-session-key (-> String))
(define (generate-session-key)
  (~> (random-bytes SESSION-KEY-BYTES)
      base64-encode
      strip-trailing-equals
      bytes->string/utf-8))

;; is this a valid session key?
(: valid-session-key? (String -> Boolean))
(define (valid-session-key? str)
  (and (regexp-match #px"^[a-zA-Z0-9+/]+$" str)
       (< (string-length str)
          MAX-SESSION-KEY-LENGTH)
       #t))

;; strip the trailing =\r\n off of the base64-encoding
(: strip-trailing-equals (Bytes -> Bytes))
(define (strip-trailing-equals encoded)
  (cond [(equal? (subbytes encoded (- (bytes-length encoded) 3)) #"=\r\n")
         (subbytes encoded 0 (- (bytes-length encoded) 3))]
        [else (raise-argument-error 'strip-trailing-equals
                                    "byte-string ending with =\\r\\n"
                                    0 encoded)]))

;;;;;;;
;;
;;  KEY MANAGEMENT:
;;
;;;;;;;

;; mutable!
(: the-user-map UserMap)
(define the-user-map (make-hash))

;; mutable!
(: the-session-key-map SessionKeyMap)
(define the-session-key-map (make-hash))


;; user interaction state diagram.
;; init -> started
;; started -> started

;; race conditions possible here. using a semaphore
;; to block them.
(: maybe-start-user-session (Userid SessionKey TrainingStr -> Boolean))
(define (maybe-start-user-session userid session-key training-str)
  (semaphore-wait session-start-sema)
  (define maybe-session (hash-ref the-user-map userid #f))
  (match maybe-session
    [#f
     (hash-set! the-user-map userid (list session-key
                                          (current-seconds)))
     (hash-set! the-session-key-map session-key (list userid
                                                      training-str))
     (semaphore-post session-start-sema)
     #t]
    [(list old-session-key (? old-time? timestamp))
     (hash-remove! the-session-key-map old-session-key)
     (hash-set! the-user-map userid (list session-key (current-seconds)))
     (hash-set! the-session-key-map session-key (list userid
                                                      training-str))
     (semaphore-post session-start-sema)
     #t]
    [other
     (semaphore-post session-start-sema)
         #f]))

(define session-start-sema (make-semaphore 1))

(: old-time? (Timestamp -> Boolean))
(define (old-time? t)
  (< (+ t ENFORCED-TIME-GAP) (current-seconds)))

;; 19 hours:
(define ENFORCED-TIME-GAP (* 3600 18))

;; is this the most recent session associated with the user?
(: session-active? (String -> Boolean))
(define (session-active? session-key)
  (hash-has-key? the-session-key-map session-key))

;; return info for this session if it's active
(: session-info (SessionKey -> (U False (List Userid TrainingStr))))
(define (session-info session-key)
  (hash-ref the-session-key-map session-key #f))

(module+ test
  (require typed/rackunit)

  
  (check-true (valid-session-key? "7897+/3htasthAT"))
  (check-false (valid-session-key? "7897+/3htasthATH7897+/3htasthATH7897+/3htasthATH"))
  (check-false (valid-session-key? "7897+/3htasth>TH"))

  ;; can't test the time-based part... :(
  (check-equal? (maybe-start-user-session "zardoz" "3hnth3" "a") #t)
  (check-true (hash-has-key? the-user-map "zardoz"))
  (check-true (hash-has-key? the-session-key-map "3hnth3"))
  (check-false (hash-has-key? the-user-map "bapbap"))
  (check-false (hash-has-key? the-session-key-map "227"))
  (check-equal? (session-info "3hnth3") (list "zardoz" ))
  (check-equal? (maybe-start-user-session "zardoz" "287" "a") #f)
  (check-equal? (maybe-start-user-session "zardoz" "227" "a") #f)
  (check-equal? (maybe-start-user-session "bapbap" "227" "a") #t)
  (check-equal? (maybe-start-user-session "zardoz" "21437" "a") #f)
  (check-equal? (maybe-start-user-session "bapbap" "2223147" "a") #f)
  (check-true (hash-has-key? the-user-map "bapbap"))
  (check-true (hash-has-key? the-session-key-map "227"))
  

  (check-true (session-active? "3hnth3"))
  (check-false (session-active? "aaaaa"))

  ;; only run these tests after setting the time gap to 5 seconds...
  ;(check-false (maybe-start-user-session "zardoz" "21437"))
  ;(sleep 8)
  ;(check-true (maybe-start-user-session "zardoz" "21437"))
  ;(check-false (hash-has-key? the-session-key-map "3hnth3"))

  )

