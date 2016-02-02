#lang racket/base

;; ISSUES:
;; - stress test anticipated bandwidth... 1.5Meg at once?
;; - password length... long enough boxes?
;; - align printed password and box
;; - 'this popup is too annoying' message
;; - consent form
;; - user repeat lockout
;; - hash user names in log?
;; - configuration management! https/http, localhost/king, log file locations
;; - spaces at end of passwords!

;; expected endpoints:

;; POST: /start,  "userid=...&password=..."
;; where the value associated with timestamp is... ?
;; result: a web page

;; POST: /record-data, {"userid":...,"sessionkey":...,
;;                      "data":[{t:...,n:...,p:...},...]}
;; where userid and sessionkey are strings, and data is a sequence
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
         "local-config.rkt")

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
            ;; start a new session
            [(list (struct path/param ("start" (list))))
             (handle-session-start (form-urlencoded->alist (bytes->string/utf-8
                                                            post-data/raw)))]
            ;; add data for a session
            [(list (struct path/param ("record-data" (list))))
             (handle-session-data (bytes->jsexpr post-data/raw))]
            [other
             (404-response
              #"unknown server path"
              (format "POST url ~v doesn't match known pattern" (url->string uri)))]))]
       [other
        (404-response
         (format "unexpected method"))])])))

;; handle a session-start
(define (handle-session-start post-alist)
  (with-handlers ([(lambda (exn)
                     (and (exn:fail? exn)
                          (regexp-match #px"no user named"
                                        (exn-message exn))))
                   (lambda (exn)
                     (fail-response
                      403
                      #"Forbidden"
                      (exn-message exn)))])
    (match post-alist
      [(list-no-order
        (cons 'userid (? string? userid))
        (cons 'password (? string? pwd)))
       (cond [(valid-password? userid pwd)
              (define session-key (generate-session-key))
              (define training-str (generate-training-str userid pwd))
              (log-session-start! userid session-key)
              (main-page userid session-key training-str
                         record-data-url)]
             [else
              (fail-response
               403
               #"Forbidden"
               "user id and password incorrect")])]
      [other (fail-response
              400
              #"wrong POST data shape"
              "wrong fields in session-start POST data")])))


;; output a main html page with uid session-key and training-str embedded
(define (main-page userid session-key training-str record-data-url)
  (response/html
   (include-template "mainpage.html")))

;; given session data, record it and signal completion
(define (handle-session-data post-jsexpr)
  (match-record-data-jsexpr
   post-jsexpr
   (λ (uid session-key t n p)
     (record-session-data! uid session-key t n p)
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
    [(hash-table ('userid (? string? uid))
                 ('sessionkey (? string? session-key))
                 ('data (list (hash-table ['t (? nat? t)]
                                          ['n (? nat? n)]
                                          ['p (? string? p)])
                              ...)))
     (success-kont uid session-key t n p)]
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
   (list #;(make-header #"Access-Control-Allow-Origin"
                      #"*"))
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
   ;; CORS header necessary for testing only:
   (list #;(make-header #"Access-Control-Allow-Origin"
                      #"*"))
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
                   #:servlet-regexp #px"^/(start|record-data)$"
                   #:launch-browser? #f
                   #:extra-files-paths (list (build-path here "htdocs"))
                   #:log-file (build-path here "server.log"))))

(module+ test
  (require rackunit)
  (define test-jsexpr
    (bytes->jsexpr
     #"{\"userid\":\"guest\",\"sessionkey\":\"A6MCslStdKk\",\"data\":
[{\"t\":123,\"n\":1,\"p\":\"D\"},{\"t\":456,\"n\":1,\"p\":\"De\"}]}" 
     ))
  (check-equal?
   (match-record-data-jsexpr
    test-jsexpr
    (λ (uid session-key t n p)
      (list uid session-key t n p))
    (λ ()
      'fail))
   (list "guest" "A6MCslStdKk" '(123 456) '(1 1) '("D" "De"))))



