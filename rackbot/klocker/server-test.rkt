#lang racket/base

(require net/url
         net/head
         rackunit
         rackunit/text-ui
         racket/date
         racket/block
         racket/contract
         racket/format
         json
         (only-in racket/list last empty? rest empty first)
         "web-funs.rkt")

(define-logger klocker)

(define HOST 
  "localhost"
  #;"king.brinckerhoff.org"
  )

(define PORT
  8027
  #;443)

(define PROTOCOL
  'http)

(define (gett endpoint)
  (remote-call/get HOST PORT endpoint #:protocol PROTOCOL))

(define (postt endpoint jsexpr)
  (remote-call/post HOST PORT endpoint
                    #:protocol PROTOCOL
                    jsexpr))

(define (postt/core endpoint bytes)
  (remote-call/post/core HOST PORT endpoint bytes #:protocol PROTOCOL))

(define (all-but-last l)
  (cond [(empty? l) (raise-argument-error 'all-but-last
                                          "nonempty list" 0 l)]
        [(empty? (rest l)) empty]
        [else (cons (first l) (all-but-last (rest l)))]))

(check-equal? (all-but-last '(3 4 5 6)) '(3 4 5))

;; ping check returns wrong result
;; no events wrong in both places

(define (string-or-null? s)
  (or (eq? s 'null)
      (string? s)))


(define (time-near-now? n)
  (and (exact-integer? n)
       (< (abs (- (current-seconds) n)) 100)))

(define ((port-containing str) port)
  (regexp-match (regexp-quote str) port))

(define ((eqto? a) b)
  (equal? a b))

(run-tests
(test-suite
 "klocker server"
 (block

  ;; bogus endpoint
  (test-case
   "404s"
   ;; simple 404:
   (check-match (remote-call/get/core HOST PORT "/blothints"
                                      #:protocol PROTOCOL)
                (list (regexp #"^HTTP/1.1 404")
                      _1
                      (? (port-containing "not found") _3))))

  
  (test-case
   "bad JSON"
   (check-match (postt/core "/start" #"bogodata")
                (list (regexp #px"^HTTP/1.1 400")
                      _1
                      _2)))

  (define (long-string? s)
    (and (string? s) (< 10 (string-length s))))


  (test-case
   "missing field"
   (check-match
    (postt/core "/start" #"userid=this+is+my+name")
    (list (regexp #px"^HTTP/1.1 400 wrong POST data shape")
          _2 _3)))

  (test-case
   "illegal userid"
   (check-match
    (postt/core "/start" #"userid=this+is+my+name&password=bogo")
    (list (regexp #px"^HTTP/1.1 400 Illegal Userid")
          _2 _3)))
  
  (test-case
   "unknown user"
   (check-match
    (postt/core "/start" #"userid=thisismyname&password=and+my+password")
    (list (regexp #px"^HTTP/1.1 403 Forbidden") _2 _3)))

  (test-case
   "successful login, no consent yet"
   (check-pred
    (λ (response)
      (regexp-match #px"INFORMED CONSENT" response))
    (postt "/start" #"userid=nonconsenter&password=40")))

  (test-case
   "successful post-consent"
   (check-pred
    (λ (response)
      (regexp-match #px"HERE IS YOUR PASSWORD" response))
    (postt "/record-consent" #"sessionkey=testsessionkey")))
  
  (test-case
   "successful login"
   (check-pred
    (λ (response)
      (regexp-match #px"HERE IS YOUR PASSWORD" response))
    (postt "/start" #"userid=consenter&password=40")))

  
  (test-case
   "session data"
   (check-match
    (postt "/record-data"
           (jsexpr->bytes
            (hash 'sessionkey "7728koh"
                  'data (list (hash 't 2340
                                    'n 2
                                    'p "a")
                              (hash 't 2640
                                    'n 2
                                    'p "ab")
                              (hash 't 3140
                                    'n 2
                                    'p "abc")
                              (hash 't 3362
                                    'n 2
                                    'p "ab")
                              (hash 't 3961
                                    'n 2
                                    'p "abq")))))
    "recorded"))
   
   
   

   
)))

