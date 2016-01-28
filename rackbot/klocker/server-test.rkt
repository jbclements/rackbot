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

  (test-equal? "ping" (gett "ping" #f) "alive")

  ;; bogus endpoint
  (test-case
   "404s"
   ;; simple 404:
   (check-match (remote-call/get/core HOST PORT (klocker-url "blothints" #f))
                (list (regexp #"^HTTP/1.1 404")
                      _1
                      (? (port-containing "blothints") _3))))

  
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
   "missing json field"
   (check-match
    (remote-call/post/core HOST PORT (klocker-url "start" #f)
                           (jsexpr->bytes
                            (hash 'userid "truncheon"
                                  'password "bootie")))
    (list (regexp #px"^HTTP/1.1 400 wrong JSON shape in POST")
          _2 _3)))
  
  (test-case
   "unknown user"
   (check-match
    (remote-call/post/core HOST PORT (klocker-url "start" #f)
                           (jsexpr->bytes
                            (hash 'userid "truncheon"
                                  'password "bootie"
                                  'timestamp 9287)))
    (list (regexp #px"^HTTP/1.1 403 Forbidden") _2 _3)))

  (test-case
   "successful login"
   (check-match
    (postt "start" (hash 'userid "clements"
                         'password "aoeuidht"
                         'timestamp 9287))
    (hash-table ('sessionkey (? long-string? _1))
                           ('trainingstr (? string? _2)))))

  
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
   
#;(
   (test-case
    "timestamp"
    (check-match (gett "timestamp" #f)
                 (hash-table ('timestamp (? time-near-now? n)))))

   ;; this is kind of meaningless for now...
   ;; ... need a new endpoint to list all measurements & all devices





   (test-equal? "no-latest-event"
                (gett "latest-event" '((measurement "temperature")
                                       (device "testing_empty")))
                "no events")


   ;; new style of electric devices... anything goes!
   (test-case
    "illegal electrical device name"
    (check-match
     (remote-call/get/core HOST PORT (klocker-url "latest-event" '((measurement "electric_power")
                                                                 (device "device with spaces"))))
     (list (regexp #px"^HTTP/1.1 404")
           _1
           (? (port-containing "device with spaces") _3))))

   (test-equal?
    "made-up device"
    (gett "latest-event" '((measurement "electric_power")
                           (device "bogus_device")))
    "no events")
   


   (test-case
    "events-in-empty-range"
    (define ts (current-seconds))
    (check-equal?
     (gett "events-in-range"
           `((measurement "temperature")
             (device "bedroom")
             (start ,ts)
             (end ,ts)))
     '()))
   ;; may be going away...
   (test-case
    "too long range for events-in-range"  

    (define ts1 (find-seconds 0 0 0 1 1 2014))
    (define ts2 (find-seconds 0 0 0 1 1 2015))
    ;; more than a day of data:
    (check-match (remote-call/get/core
                  HOST PORT
                  (klocker-url "events-in-range"
                             `((measurement "temperature")
                               (device "bedroom")
                               (start ,ts1)
                               (end ,ts2))))
                 (list #"HTTP/1.1 400 range too long"
                       _2
                       _3)))

   (test-case
    "count-events-in-range bad args"
    (check-match (remote-call/get/core
                  HOST PORT
                  (klocker-url "count-events-in-range" #f))
                 (list #"HTTP/1.1 404 wrong query fields" _2 _3))
    
    (check-match (remote-call/get/core
                  HOST PORT
                  (klocker-url "count-events-in-range" '((device foo)
                                                       (start 0)
                                                       (end 1))))
                 (list #"HTTP/1.1 404 wrong query fields"  _2 _3)))

   
   ;; RECORDING READINGS

   (test-case
    "record-reading-404"
    (check-match (remote-call/post/core
                  HOST PORT
                  (klocker-url "record-reading" `((device uhnoth)))
                  #"1234")
                 (list (regexp #px#"^HTTP/1.1 404")
                       _1
                       (? (port-containing "uhnoth") _3))))

        


   (test-case
    "record-reading-bad-json"
    
    (check-match (remote-call/post/core
                  HOST PORT
                  (klocker-url "record-reading" '((device s-temp-lr)))
                  #"abcd")
                 (list #"HTTP/1.1 400 bad JSON in POST"
                       _2
                       _3)))
   
   (test-case
    "record-reading"
    (check-equal? (remote-call/post
                   HOST PORT
                   (klocker-url "record-reading" '((device s-temp-testing-blackhole)))
                   #"{\"status\":7772387,\"secret\":\"$a8Es#crB469\"}")

                  "okay"))

   ;; try with electrical ones
   (test-case
    "record-reading"
    (check-equal? (remote-call/post
                   HOST PORT
                   (klocker-url "record-reading" '((measurement "electric_power")
                                                 (device "krazbo_magnipod")))
                   #"{\"status\":7111387,\"secret\":\"$a8Es#crB469\"}")

                  "okay"))
   

   ;; test sending with urlencoding
   (test-case
    "record-reading with form-urlencoded"
    (check-equal?
     (remote-call/post
      HOST PORT
      (klocker-url "record-reading" '((device s-temp-testing-blackhole)))
      #"status=261&secret=$a8Es#crB469"
      #:content-type #"application/x-www-form-urlencoded")
     "okay"))

   (test-case
    "record-reading with form-urlencoded negative #"
    (check-equal?
     (remote-call/post
      HOST PORT
      (klocker-url "record-reading" '((device s-temp-testing-blackhole)))
      #"status=-261&secret=$a8Es#crB469"
      #:content-type #"application/x-www-form-urlencoded")
     "okay"))
   
   (test-case
    "record-reading with form-urlencoded bad number"
    (check-match
     (remote-call/post/core
      HOST PORT
      (klocker-url "record-reading" '((device s-temp-testing-blackhole)))
      #"status=26eee1&secret=$a8Es#crB469"
      #:content-type #"application/x-www-form-urlencoded")
     (list (regexp #"^HTTP/1.1 400 ")
           _2 _3)))

   (test-case
    "record-reading with form-urlencoded bad alist"
    (check-match
     (remote-call/post/core
      HOST PORT
      (klocker-url "record-reading" '((device s-temp-testing-blackhole)))
      #"status=21&=secret=$a8Es#crB469"
      #:content-type #"application/x-www-form-urlencoded")
     (list (regexp #"^HTTP/1.1 400 ")
           _2 _3)))

   ;; WEATHER FORECAST
   (test-case
    "weather forecast"
    (check-match
     (gett "latest-forecast" '())
     (hash-table ('timestamp (? number? n))
                 ('forecast (hash-table ('latitude _1)
                                        ('longitude _2)
                                        ('currently _3)
                                        ('minutely _4)
                                        (_5 _6)
                                        ...)))))

   (test-case
    "insights"
    (check-pred
     (and/c
      (listof (hash/c symbol? (or/c string? number?) #:immutable #t))
      ;; should never be empty:
      pair?)
     (gett "latest-insights" '())))

   ;; INTERVAL MEANS

   
   (test-case
     "interval means/firsts"
     (define ts (find-seconds 23 14 17 4 9 2015))
     (check-pred
      (flat-contract-predicate
       (listof (hash/c (symbols 't 'r) (or/c number? "none") #:immutable #t)))
      (gett "mean-by-interval" `((measurement "humidity")
                                 (device "outside")
                                 (start ,ts)
                                 (end ,(+ ts 100))
                                 (interval 30))))
     (check-pred
      (flat-contract-predicate
       (listof (hash/c (symbols 't 'r) (or/c number? "none") #:immutable #t)))
      (gett "first-by-interval" `((measurement "humidity")
                                 (device "outside")
                                 (start ,ts)
                                 (end ,(+ ts 100))
                                 (interval 30))))
     (check-pred
      (flat-contract-predicate
       (listof (hash/c (symbols 't 'r) (or/c number? "none") #:immutable #t)))
      (gett "last-by-interval" `((measurement "humidity")
                                 (device "outside")
                                 (start ,ts)
                                 (end ,(+ ts 100))
                                 (interval 30)))))

   (test-case
    "last in interval"
    (define ts (find-seconds 23 14 17 4 9 2015))
    (check-pred
     (flat-contract-predicate
      (or/c number? "no events"))
     (gett "interval-last-event" `((measurement "humidity")
                                   (device "outside")
                                   (start ,(- ts (* 3600 2)))
                                   (end ,ts)))))

   (test-case
    "first in interval"
    (define ts (find-seconds 23 14 17 4 9 2015))
    (check-pred
     (flat-contract-predicate
      (or/c "no events"
            (hash/c (symbols 't 'r) number? #:immutable #t)))
     (gett "interval-first-event" `((measurement "humidity")
                                   (device "outside")
                                   (start ,(- ts (* 3600 2)))
                                   (end ,ts)))))

   (test-case
    "long-term means"
    (define ts (find-seconds 23 14 17 4 9 2015))
    (check-pred
     (flat-contract-predicate
      (listof (hash/c (symbols 't 'r) (or/c number? "none") #:immutable #t)))
     (gett "mean-by-interval" `((measurement "humidity")
                                (device "outside")
                                (start ,(- ts (* 86400 2)))
                                (end ,ts)
                                (interval 3600))))))

   
)))

