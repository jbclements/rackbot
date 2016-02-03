#lang racket/base

;; ISSUES:
;; - stress test anticipated bandwidth... 1.5Meg at once?
;; - password length... long enough boxes?
;; - align printed password and box
;; - 'this popup is too annoying' message
;; - consent form
;; - can't hit return where the interface tells you to (for new users)
;; - allow users to create their own accounts
;; - user repeat lockout
;; - hash user names in log?
;; - remove run from repo (for local config)
;; - spaces at end of passwords!
;; - make /etc/service for server itself.
;; - session-start must include training string
;; - use 'label' attribute
;; - on hash table reset, allow all sessions? Or reset session?

;; expected endpoints:

;; POST: /start,  "userid=...&password=..."
;; result: a web page

;; POST: /record-consent, "sessionkey=..."
;; result: a web page

;; POST: /record-data, {"sessionkey":...,
;;                      "data":[{t:...,n:...,p:...},...]}
;; where sessionkey is a string, and data is a sequence
;; of structures containing a t field with milliseconds since the
;; beginning of the session, an n field indicating which password
;; the trainee is working on, and a p field containing the current
;; contents of the field.
;; result: "recorded"

(require racket/match
         web-server/servlet
         web-server/servlet-env
         web-server/templates
         net/uri-codec
         json
         racket/date
         xml
         racket/runtime-path
         "user-auth.rkt"
         "klocker-util.rkt"
         "local-config.rkt"
         "session-key-mgmt.rkt")

(define START-ENDPOINT "start")
(define DATALOG-ENDPOINT "record-data")
(define CONSENTED-ENDPOINT "record-consent")
(define SERVLET-REGEXP  #px"^/(start|record-data|record-consent)$")

(define datalog-url (string-append server-stem "/" DATALOG-ENDPOINT))
(define start-url (string-append server-stem "/" START-ENDPOINT))
(define consented-url (string-append server-stem "/" CONSENTED-ENDPOINT))

(define-runtime-path here ".")

;; copied from sodec-server:
(define-logger client)

;; handle a request. "front door" of the server
(define (start req)
  (with-handlers ;; log all errors...
      ([(lambda (exn) #t)
        (lambda (exn)
          
          (log-error (exn-message exn))
          (fail-response 500 #"server exception"
                 "Server-side exception. Check logs for more detail."))])
  (match req
    [(struct request
       (method
        uri
        headers/raw
        bindings/raw-promise
        post-data/raw
        host-ip
        host-port
        client-ip))
     (match method
       [#"GET"
        (404-response
         #"unknown server path"
         (format "GET url ~v doesn't match known pattern" (url->string uri)))]
       [#"POST"
        (with-handlers ([(lambda (exn)
                           (and (exn:fail? exn)
                                (regexp-match #px"bytes->jsexpr" (exn-message exn))))
                         (lambda (exn)
                           (fail-response
                            400
                            #"bad JSON in POST"
                            (format "expected POST bytes parseable as JSON, got: ~e"
                                    post-data/raw)))])
          (match (url-path uri)
            [(list (struct path/param (endpoint (list))))
             (cond
               ;; start a new session
               [(string=? endpoint START-ENDPOINT)
                (handle-session-start (form-urlencoded->alist (bytes->string/utf-8
                                                               post-data/raw)))]
               ;; user has consented
               [(string=? endpoint CONSENTED-ENDPOINT)
                (handle-consented (form-urlencoded->alist (bytes->string/utf-8
                                                           post-data/raw)))]
               ;; add data for a session
               [(string=? endpoint DATALOG-ENDPOINT)
                (handle-session-data (bytes->jsexpr post-data/raw))]
               [else
                (404-response
                 #"unknown server path"
                 (format "POST url ~v doesn't match known pattern" (url->string uri)))])]
            [other
             (404-response
              #"unknown server path"
              (format "POST url ~v doesn't match known pattern"
                      (url->string uri)))]))]
       [other
        (404-response
         (format "unexpected method"))])])))

;; handle a click on "I agree"
(define (handle-consented post-alist)
  (match post-alist
    [(list
      (cons 'sessionkey (? string? session-key)))
     (cond [(not (valid-session-key? session-key))
            (fail-response
             400
             #"Illegal Session Key"
             "session keys must be valid")]
           [else (match (session-info session-key)
                   [(list user training-str)
                    (main-page session-key
                               (escape-quotes training-str)
                               datalog-url)]
                   [#f
                    (fail-response
                     403
                     #"Forbidden"
                     (format "this session key does not appear to be active: ~v"
                             session-key))])])]
    [other (fail-response
            400
            #"wrong POST data shape"
            "wrong fields in consented POST data")]))

;; output a consent form page with userid, session-key, training-str, and consent-url embedded
(define (consent-page userid session-key training-str agree-url)
  (response/html
   (include-template "consent-template.html")))


;; handle a session-start
(define (handle-session-start post-alist)
  (match post-alist
    [(list-no-order
      (cons 'userid (? string? userid))
      (cons 'password (? string? password)))
     (cond [(not (valid-userid? userid))
            (fail-response
             400
             #"Illegal Userid"
             "user ids can contain only numbers, characters, and underscore")]
           [(valid-password? userid password)
            (define session-key (generate-session-key))
            (define training-str (generate-training-str userid password))
            (match (maybe-start-user-session userid session-key training-str)
              [#t
               (log-session-start! userid session-key training-str)
               (cond [(user-consented? userid)
                      (main-page session-key training-str datalog-url)]
                     [else
                      (consent-page userid session-key
                                    (form-urlencoded-encode training-str)
                                    consented-url)])]
              [#f
               (fail-response
                403
                #"Forbidden"
                "It appears that it's too soon for you to practice your password again.")])]
           [else
            (fail-response
             403
             #"Forbidden"
             "user id and/or password incorrect")])]
    [other (fail-response
            400
            #"wrong POST data shape"
            "wrong fields in session-start POST data")]))

;; output a main html page with userid session-key and training-str embedded
(define (main-page session-key training-str record-data-url)
  (response/html
   (include-template "mainpage-template.html")))

;; for embedding in JavaScript, double-quotes must be quoted:
(define (escape-quotes str)
  (regexp-replace* #px"\"" str "\\\\\""))

;; given session data, record it and signal completion
(define (handle-session-data post-jsexpr)
  (match-record-data-jsexpr
   post-jsexpr
   (λ (session-key t n p)
     (record-session-data! session-key t n p)
     (response/json "recorded"))
   (λ ()
     (fail-response
            400
            #"wrong JSON shape in POST"
            "wrong JSON shape in session-data post"))))

;; given jsexpr, match it and call either success or fail continuation
;; this abstraction is a bit awkward, but I want to be able to test
;; it independently
(define (match-record-data-jsexpr jsexpr success-kont fail-kont)
  (match jsexpr
    [(hash-table ('sessionkey (? string? session-key))
                 ('data (list (hash-table ['t (? nat? t)]
                                          ['n (? nat? n)]
                                          ['p (? string? p)])
                              ...)))
     (success-kont session-key t n p)]
    [other (fail-kont)]))

(define nat? exact-nonnegative-integer?)

;; issue a 404 response:
(define (404-response header-msg body-msg)
  (fail-response 404 header-msg body-msg))

;; issue a failure response
(define (fail-response code header-msg body-msg)
  (log-client-error
   (format "[~a] [~a] ~a"
           (date->string (seconds->date (current-seconds)) #t)
           code
           body-msg))
  (response/full
   code
   header-msg
   (current-seconds) TEXT/HTML-MIME-TYPE
   (list)
   (list
    (string->bytes/utf-8
     (xexpr->string
      `(html (body (p ,body-msg))))))))

;; a successful json response
;; jsexpr -> response
(define (response/json jsexpr)
  (response/full
   200 #"Okay"
   (current-seconds)
   #"application/json"
   (list)
   (list (jsexpr->bytes jsexpr))))

(define (response/html html)
  (response/output (λ (port)
                     (write-string html port)
                     (void))))

(module+ main
  (with-handlers ([(λ (exn) #t)
                   (λ (exn)
                     (log-error (exn-message exn))
                     (raise exn))])
    (serve/servlet start
                   #:port listen-port
                   #:listen-ip #f
                   #:servlet-regexp SERVLET-REGEXP
                   #:launch-browser? #f
                   #:extra-files-paths (list (build-path here "htdocs"))
                   #:log-file (build-path here "server.log"))))

(module+ test
  (require rackunit)
  

  (check-equal? (escape-quotes "ab'\"cd") "ab'\\\"cd")
  (define test-jsexpr
    (bytes->jsexpr
     #"{\"sessionkey\":\"A6MCslStdKk\",\"data\":
[{\"t\":123,\"n\":1,\"p\":\"D\"},{\"t\":456,\"n\":1,\"p\":\"De\"}]}" 
     ))
  (check-equal?
   (match-record-data-jsexpr
    test-jsexpr
    (λ (session-key t n p)
      (list session-key t n p))
    (λ ()
      'fail))
   (list "A6MCslStdKk" '(123 456) '(1 1) '("D" "De"))))



