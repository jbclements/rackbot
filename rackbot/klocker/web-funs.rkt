#lang racket/base

;; web functions used more than once

(require net/url
         net/head
         net/http-client
         net/uri-codec
         json
         racket/match
         racket/contract
         (only-in racket/list add-between))

(provide (contract-out
          [remote-call/get (->* (string? number? string?)
                                (#:protocol protocol?)
                                jsexpr?)]
          [remote-call/get/core (->* (string? number? string?)
                                     (#:protocol protocol?)
                                     (list/c bytes? (listof bytes?) input-port?))]
          [remote-call/post
           (->* (string? number? string? bytes?)
                (#:content-type bytes? #:protocol protocol?)
                (or/c bytes? jsexpr?))]
          [remote-call/post/core
           (->* (string? number? string? bytes?)
                (#:content-type bytes? #:protocol protocol?)
                (list/c bytes? (listof bytes?) input-port?))]
          [klocker-url
           (-> string? (or/c query? #f) string?)])
         parse-results)

(define protocol? (symbols 'http 'https))

(define-logger klocker)

;; represents elements of a "GET" query
(define query? (listof (list/c symbol? (or/c number? symbol? string?))))

;; map query associations to a string
(define (query->string assoc)
  (alist->form-urlencoded
   (for/list ([pr (in-list assoc)])
     (match pr
       [(list a (? number? n)) (cons a (number->string n))]
       [(list a (? symbol? n)) (cons a (symbol->string n))]
       [(list a (? string? s)) (cons a s)]))))

;; a string with only alphanum and hyphens OR an exact integer OR a symbol
;; whose corresponding string is alphanum & hyphens
(define (clean-string? str)
  (or (and (string? str)
           (regexp-match #px"^[A-Za-z0-9-]*$" str)
           #t)
      (exact-integer? str)
      (and (symbol? str)
           (clean-string? (symbol->string str)))))


;; formulate a request URL
(define (klocker-url endpoint query)
  (string-append "/" endpoint
                 (cond [query
                        (string-append QUERY-START (query->string query))]
                       [else ""])))

(define QUERY-START "?")

;; given a host, port, and URI, make a GET request and return the
;; result as a jsexpr
(define (remote-call/get host port uri #:protocol [protocol 'https])
  (apply parse-results (remote-call/get/core host port uri
                                             #:protocol protocol)))

;; given a URL, make a POST request and wait for a succesful response, returning a jsexpr
(define (remote-call/post host port uri post-bytes
                          #:content-type [content-type #"application/json"]
                          #:protocol [protocol 'https])
  (apply parse-results (remote-call/post/core host port uri post-bytes
                                                #:content-type content-type
                                                #:protocol protocol)))



;; given a URL, make a GET request and wait for a response
(define (remote-call/get/core host port uri
                              #:protocol [protocol 'https])
  (log-klocker-debug "remote-call/get/core: args: ~a"
             (list host port uri))
  (call-with-values (lambda () (http-sendrecv host uri
                                              #:port port
                                              #:ssl? (match protocol
                                                       ['https 'secure]
                                                       ['http #f])))
                    list))


(define (remote-call/post/core host port uri post-bytes
                               #:content-type [content-type #"application/json"]
                               #:protocol [protocol 'https])
  (log-klocker-debug "remote-call/post/core: args: ~a" 
                   (list host port uri post-bytes))
  (call-with-values
   (lambda () (http-sendrecv host uri #:port port #:method 'POST
                             #:headers (list (bytes-append #"Content-Type: "content-type))
                             #:data post-bytes
                             #:ssl? (match protocol
                                      ['https 'secure]
                                      ['http #f])))
   list))

;; given a list of header strings, find the one with the given name and return
;; its associated right-hand-side
(define (find-field name headers)
  (cond [(null? headers) (error 'find-field "field ~a not found in headers" name)]
        [else (match (car headers)
                [(regexp (bytes-append #"^"(regexp-quote name)
                                       #":[ \t]+(.*)$")
                         (list dc rhs))
                 rhs]
                [other (find-field name (cdr headers))])]))


;; fetch the content on the response-port
(define (fetch-body response-port)
  (define reply (car (regexp-match #px".*" response-port)))
  (close-input-port response-port)
  (log-klocker-debug (format "reply-bytes : ~v\n" reply))
  reply)

;; ensure that the return code is 200 and then parse the body either
;; as html or as a jsexpr
(define (parse-results first-line headers response-port)
  (define response-code (extract-response-code first-line))
  (cond [(= response-code 200)
         (define mime-type (find-field #"Content-Type" headers))
         (match mime-type
           [(regexp #px#"^application/json(;.*)?$")
            (bytes->jsexpr (fetch-body response-port))]
           [(regexp #px#"^text/html(;.*)?$")
            (fetch-body response-port)]
           [other
            (error 'results->jsexpr
                  (format "expected mime type application/json or text/html, got ~e"
                          mime-type))])]
        [else
         (error 'results->jsexpr
                "response code: expected 200, got: ~v\nwith message: ~v\nand body: ~v" 
                response-code
                first-line
                (regexp-match #px".*" response-port))]))

;; extract the response code from a http response line:
(define (extract-response-code first-line)
  (match (regexp-match #px"^HTTP/[^ ]* ([0-9]+)" first-line)
    [(list dc2 response-code-string)
     (string->number (bytes->string/utf-8 response-code-string))]
    [other
     (raise-argument-error 'response-code "HTTP response line matching standard pattern"
                           0 first-line)]))

;; given an input port, return the response code, the first line, the rest of the
;; headers, and the port for the body
(define (response-port->results response-port)
  (define header-string (purify-port response-port))
  (match (regexp-match #px"^([^\n]*)\n(.*)" header-string)
    [(list dc first-line headers)
     (match (regexp-match #px"^HTTP/[^ ]* ([0-9]+)" first-line)
       [(list dc2 response-code-string)
        (define reply-code (string->number response-code-string))
        (list reply-code first-line headers response-port)]
       [other
        (error 'remote-call/get/core
               "couldn't extract response code from first response line ~e"
               first-line)])]
    [other (error 'remote-call/get
                  (format "expected response with at least one header line, got ~e"
                          header-string))]))


(module+ test
  (require rackunit)

  (check-equal? (find-field #"Content-Type"
                            '(#"Argybargy: 23"
                              #"Content-Type: application/json"
                              #"Troubador: 9"))
                #"application/json")
  (check-equal? (clean-string? "hth-t987") #t)
  (check-equal? (clean-string? "hth-t98.7") #f)
  
  (check-equal? (query->string '((device "s-temp-bed")
                                 (start 273)
                                 (end "29")))
                "device=s-temp-bed&start=273&end=29")

  (check-equal? (alist->form-urlencoded
                 '((device . "s-temp-bed")
                   (start . "273")
                   (end . "29")))
                "device=s-temp-bed&start=273&end=29")

  
  (check-equal? (klocker-url "latest-event"
                           `((device s-temp-bed)))
                "/latest-event?device=s-temp-bed")
  (check-equal? (klocker-url "latest-event" #f)
                (string-append "/latest-event"))

  
  (check-match (remote-call/get/core "example.com" 80 "/")
               (list-rest #"HTTP/1.1 200 OK" rest))
  
  
  )