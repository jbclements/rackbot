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
  )

(define PORT
  8027)

(define (gett . args)
  (remote-call/get HOST PORT (apply klocker-url args)))

(define (postt endpoint jsexpr)
  (remote-call/post HOST PORT (klocker-url endpoint #f)
                    (jsexpr->bytes jsexpr)))

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
   (check-match (remote-call/get/core HOST PORT (klocker-url "blothints" #f))
                (list (regexp #"^HTTP/1.1 404")
                      _1
                      (? (port-containing "not found") _3))))

  
  (test-case
   "bad JSON"
   (check-match (remote-call/post/core HOST PORT (klocker-url "start" #f)
                                       #"bogodata")
                (list (regexp #px"^HTTP/1.1 400")
                      _1
                      _2)))

  (define (long-string? s)
    (and (string? s) (< 10 (string-length s))))


  (test-case
   "missing field"
   (check-match
    (remote-call/post/core HOST PORT (klocker-url "start" #f)
                           #"userid=this+is+my+name")
    (list (regexp #px"^HTTP/1.1 400 wrong POST data shape")
          _2 _3)))
  
  (test-case
   "unknown user"
   (check-match
    (remote-call/post/core HOST PORT (klocker-url "start" #f)
                           #"userid=this+is+my+name&password=and+my+password")
    (list (regexp #px"^HTTP/1.1 403 Forbidden") _2 _3)))

  (test-case
   "successful login"
   (check-pred
    (λ (response)
      (regexp-match #px"HERE IS YOUR PASSWORD" response))
    (remote-call/post HOST PORT (klocker-url "start" #f)
                      #"userid=guest&password=40")))

  
  (test-case
   "session data"
   (check-match
    (postt "record-data" (hash 'userid "clements"
                               'sessionkey "7728koh"
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
                                                 'p "abq"))))
    "recorded"))
  
  #;(check-match (postt "start" (hash 'userid "truncheon"
                                    'password "bootie"
                                    'timestamp 9287))
               (hash-table ('sessionkey (? long-string? _1))
                           ('trainingstr (? string? _2))))
   
   ;; latest event
   
   ;; near miss on the device name:
   #;(check-match (remote-call/get/core HOST PORT (klocker-url "latest-event"
                                                             `((device uhnoth))))
                (list (regexp #px"^HTTP/1.1 40[04]")
                      _1
                      (? (port-containing "uhnoth") _3)))
   

   
)))

