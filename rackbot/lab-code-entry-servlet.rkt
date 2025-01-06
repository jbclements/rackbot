#lang racket/base

(require web-server/formlets
         web-server/servlet-env
         web-server/servlet/web
         web-server/http/xexpr
         racket/runtime-path
         racket/date
         racket/match
         racket/list
         racket/string
         racket/file
         "lab-code-hash.rkt")

(define THIS-QTR 2252)
(define LISTEN-PORT 8026)
(define LAB-SLOTS 9)

(define server-extra-files-path
  (let ()
    (define pref-path (build-path (find-system-path 'pref-dir) "rackbot-prefs"))
    (and (file-exists? pref-path)
         (hash-ref (file->value pref-path)
                   'server-extra-files-path
                   #f))))

;; a small servlet that allows students to enter their lab numbers

(define (log-labcode-info fmt-string . args)
  (apply printf fmt-string args)
  (newline)
  (flush-output))

;; log a "successful" line
(define (log-successes login data entered-codes qtr)
  (log-labcode-info "successes: ~s ~s ~s ~s" login data entered-codes qtr))

;; log the failures
(define (log-failures login failure-data entered-codes)
  (log-labcode-info "failures: ~s ~s ~s" login failure-data entered-codes))

;(define-type SuccessResult (Pair 'success (Pair String (Listof Any))))
;(define-predicate success-result? SuccessResult)
(define (success-result? r) (eq? (car r) 'success))
#;(define-type MatchResult
  (U SuccessResult
     (Pair 'fail (Listof Any))))

;; accept the bindings passed by the user for the web request.
;; actually doesn't return...
(define (handle-bindings logins pairs)
  (match logins
    [(list "")
     (no-login-page)]
    [(list login)
     (define nonempties
       (filter (lambda (pr)
                 (not (regexp-match #px"^\\s*$" (cadr pr))))
               pairs))
     (when (null? nonempties)
       "all entries were blank")
     (define results
       (for/list ([pair (in-list nonempties)])
         (match-define (list labnum entered-code) pair)
         (define downcased-code (string-downcase (string-trim entered-code)))
         (list labnum
               (equal?
                downcased-code
                (bytes->string/utf-8
                 (compute-hash (string-downcase (string-trim login))
                               THIS-QTR
                               labnum)))
               downcased-code)))
     (define-values (successes failures)
       (partition (lambda (result) (cadr result)) results))
     (define success-labs (map car successes))
     (define failure-labs (map car failures))
     (log-successes login success-labs (map third successes) THIS-QTR)
     (log-failures login failure-labs (map third failures))
     (success-page success-labs
                   failure-labs)]
    [other
     "Interval Server Error 22732nth2412..."]
  ))


(module+ test
  (require rackunit)
  (define clements-lab-3-code (bytes->string/utf-8
                               (compute-hash "clements" THIS-QTR 3)))
  (check-equal? (handle-bindings '("clements")
                                 `((1 "zzzz")
                                   (3 ,clements-lab-3-code)
                                   ;; this one should fail:
                                   (4 ,clements-lab-3-code)))
                (success-page '(3)
                              '(1 4)))

  (check-equal? (handle-bindings '("")
                                 '((1 "zzzz")
                                   (3 "w46vi3")
                                   (4 "abchtd")))
                (no-login-page)))

;; the formlet that accepts a completion number
(define (lab-formlet n)
  (formlet 
   (div 
    ,(format "lab ~a completion code: " n) ,{input-string . => . sekrit})
   (list n sekrit)))

;; the formlet that accepts a date
(define whole-lab-formlet
  (formlet* `(div
              (p "login id: " ,{input-string . =>* . login})
              ,@(for/list ([i (in-range 1 LAB-SLOTS)])
                  `(p ,{(lab-formlet i) . =>* . codes}))
              (p ,{(submit #"go!") . =>* . foo})
              )
            (list login codes)))

;; start the interaction
(define (start req)
  (send/suspend
   (lambda (url)
   (response/xexpr
    `(html
      (body
       ,@(apply
          handle-bindings
          (send/formlet whole-lab-formlet))))))))

;; display a success page (incl. successes and failures)
(define (success-page successes failures)
  (cond [(= (length failures) 0)
         `((h2 "INCREDIBLE VICTORY")
           (p ,(format "Lab(s) ~a logged as complete." successes)))]
        [else
  `((h3 "Results:")
    (ul (p ,(format "~a" (length successes))" success(es) on lab #s: " ,(format "~a" successes))
        (p ,(format "~a" (length failures))" failure(s) on lab #s: " ,(format "~a" failures)))
    (p "If you see successes, these should now have been logged.")
    (p "If you see failures, don't panic. Perhaps you entered "
       "the number wrong, or perhaps I gave it to you wrong. "
       "Find me and we'll figure it out."))]))

(define (no-login-page)
  `((p "It looks like you left the `login` box blank. Go back "
       "and try again.")))




(module+ main
  ;; start the servlet on port 8026
  (with-handlers ([(λ (exn) #t)
                   (λ (exn)
                     (log-error (exn-message exn))
                     (raise exn))])
    (serve/servlet start
                   #:port LISTEN-PORT
                   #:listen-ip #f
                   #:launch-browser? #f
                   #:extra-files-paths
                   (cond [server-extra-files-path (list server-extra-files-path)]
                         [else '()]))))



