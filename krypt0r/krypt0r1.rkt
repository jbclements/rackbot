#lang racket/base

(require web-server/formlets
         web-server/servlet-env
         web-server/servlet/web
         web-server/http/xexpr
         racket/match
         racket/file)

(define LISTEN-PORT 8027)

(define server-extra-files-path
  (let ()
    (define pref-path (build-path (find-system-path 'pref-dir) "rackbot-prefs"))
    (and (file-exists? pref-path)
         (hash-ref (file->value pref-path)
                   'server-extra-files-path
                   #f))))
;; a small servlet that allows students to enter their lab numbers


;; put stars on either side of a string
(define (stars str)
  (string-append "*" str "*"))

;; swap first and last chars of a string
(define (swap-chars str)
  (cond [(< (string-length str) 2)
         (error 'swap-chars "expected a string of length 2 or longer")]
        [else
         (define l (string-length str))
         (string-append (substring str (sub1 l) l) (substring str 1 (sub1 l)) (substring str 0 1))]))

;; put the first letter at the end, add "ay"
(define (pig-latin str)
  (cond [(< (string-length str) 2)
         (error 'swap-chars "expected a string of length 2 or longer")]
        [else
         (define l (string-length str))
         (string-append (substring str 1 l) (substring str 0 1) "ay")]))

(define encryptors
  (list (list "A" stars)
        (list "B" swap-chars)
        (list "C" pig-latin)))




;; accept the bindings passed by the user for the web request.
(define (handle-bindings strs encryptor-strs)
  (match* (strs encryptor-strs)
    [((list str) (list encryptor-str))
     (define encryptor (cadr (assoc encryptor-str encryptors)))
     (with-handlers ([exn:fail?
                      (λ (exn)
                        `(div
                          (p ,(format "An error occurred: ~a" (exn-message exn)))
                          (p "(Maybe go back and try again?)"))
                        )])
       (apply handle-bindings
              (send/formlet (followup-formlet str (encryptor str) encryptor-str))))]
    [(other other2)
     `(p "Internal Server Error 22732nth2412: ~a ~a" strs encryptor-strs)
     ]
  ))


(module+ test
  (require rackunit)

  (check-equal? (swap-chars "abcd") "dbca")

  (check-equal? (pig-latin "satin") "atinsay")
  )


;; the formlet that accepts a date
(define initial-formlet
  (formlet* `(div
              (p "Choose an encryptor: ",{(select-input (map car encryptors) ) . =>* . encryptor})
              (p "enter a string to be encoded: " ,{input-string . =>* . str})
              (p ,{(submit "go") . =>* . dc})
              )
            (list str encryptor)))

(define (followup-formlet input-str encrypted-str encryptor)
  (formlet* `(div
              (p ,(format "Chosen encryptor: ~a" encryptor))
              (p ,(format "Given string: ~a" input-str))
              (p ,(format "Encrypted string: ~a" encrypted-str) )
              (p "Choose an encryptor: ",{(select-input (map car encryptors) #:selected? (λ (s) (equal? s encryptor))) . =>* . encryptor})
              (p "enter another string to be encoded: " ,{input-string . =>* . str})
              (p ,{(submit "go") . =>* . dc})
              )
            (list str encryptor)))

;; start the interaction
(define (start req)
  (send/suspend
   (lambda (url)
   (response/xexpr
    `(html
      (body
       ,@(apply
          handle-bindings
          (send/formlet initial-formlet))))))))


(module+ main
  (with-handlers ([(λ (exn) #t)
                   (λ (exn)
                     (log-error (exn-message exn))
                     (raise exn))])
    (serve/servlet start
                   #:servlet-regexp #px""
                   #:port LISTEN-PORT
                   #:listen-ip #f
                   #:launch-browser? #f
                   #:extra-files-paths
                   (cond [server-extra-files-path (list server-extra-files-path)]
                         [else '()]))))



