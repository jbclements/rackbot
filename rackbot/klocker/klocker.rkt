#lang racket/base

;; ISSUES:
;; - disable CORS when deployed
;; - stress test anticipated bandwidth... 1.5Meg at once?
;; - strip =\r\n from end of session keys.

;; expected endpoints:

;; GET: /mhk/ping
;; result: "alive"

;; POST: /mhk/start, {"userid":...,"password":...,"timestamp":...}
;; where the value associated with timestamp is... ?
;; result: {"sessionkey":...,"trainingstr":...}

;; POST: /mhk/record-data, {"userid":...,"sessionkey":...,
;;                          "data":[{t:...,n:...,p:...},...]}
;; where userid and sessionkey are strings, and data is a sequence
;; of structures containing a t field with milliseconds since the
;; beginning of the session, an n field indicating which password
;; the trainee is working on, and a p field containing the current
;; contents of the field.
;; result: "recorded"

(require racket/match
         web-server/servlet
         web-server/servlet-env
         net/uri-codec
         json
         racket/date
         xml
         "user-auth.rkt"
         "klocker-util.rkt"
         racket/file
         racket/runtime-path)

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
          (define post-jsexpr (bytes->jsexpr post-data/raw))
          (match (url-path uri)
            ;; start a new session
            [(list (struct path/param ("start" (list))))
             (handle-session-start post-jsexpr)]
            ;; add data for a session
            [(list (struct path/param ("record-data" (list))))
             (handle-session-data post-jsexpr)]
            [other
             (404-response
              #"unknown server path"
              (format "POST url ~v doesn't match known pattern" (url->string uri)))]))]
       [other
        (404-response
         (format "unexpected method"))])])))

;; handle a session-start
(define (handle-session-start post-jsexpr)
  (with-handlers ([(lambda (exn)
                     (and (exn:fail? exn)
                          (regexp-match #px"no user named"
                                        (exn-message exn))))
                   (lambda (exn)
                     (fail-response
                      403
                      #"Forbidden"
                      (exn-message exn)))])
    (match post-jsexpr
      [(hash-table ('timestamp (? number? timestamp))
                   ('userid (? string? uid))
                   ('password (? string? pwd)))
       (cond [(valid-password? uid pwd)
              (define session-key (generate-session-key))
              (define training-str (generate-training-str uid pwd))
              (log-session-start! uid session-key timestamp)
              (response/json (hash 'sessionkey session-key
                                   'trainingstr training-str))]
             [else
              (fail-response
               403
               #"Forbidden"
               "user id and password incorrect")])]
      [other (fail-response
              400
              #"wrong JSON shape in POST"
              "wrong JSON shape in session-start post")])))

;; given session data, record it and signal completion
(define (handle-session-data post-jsexpr)
  (match post-jsexpr
    [(hash-table ('userid (? string? uid))
                 ('sessionkey (? string? session-key))
                 ('data (list (hash-table ['t (? nat? t)]
                                          ['n (? nat? n)]
                                          ['p (? string? p)])
                              ...)))
     (record-session-data! uid session-key t n p)
     (response/json "recorded")]
    [other (fail-response
            400
            #"wrong JSON shape in POST"
            "wrong JSON shape in session-data post")]))

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
   ;; need CORS headers even on fails to allow them to be read...
   (list (make-header #"Access-Control-Allow-Origin"
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
   (list (make-header #"Access-Control-Allow-Origin"
                      #"*"))
   (list (jsexpr->bytes jsexpr))))

(define cert-dir (string->path "/Users/clements/brinckerhoff.org-certs/"))
(define cert-path (build-path cert-dir "brinckerhoff.org.pem"))
(define key-path (build-path cert-dir "brinckerhoff.org.key"))

(module+ main
  (with-handlers ([(λ (exn) #t)
                   (λ (exn)
                     (log-error (exn-message exn))
                     (raise exn))])
    (serve/servlet start
                   #:port 8027
                   #:listen-ip #f
                   #:servlet-regexp #px"^/(start|record-data)$"
                   ;;#:ssl-cert cert-path
                   ;;#:ssl-key key-path
                   #:launch-browser? #f
                   #:extra-files-paths (list (build-path here "htdocs"))
                   #:log-file (build-path here "server.log"))))



