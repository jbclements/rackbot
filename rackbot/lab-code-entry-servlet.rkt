#lang racket/base

(require web-server/formlets
         web-server/servlet-env
         web-server/servlet/web
         web-server/http/xexpr
         racket/runtime-path
         racket/date
         racket/match
         racket/list
         "lab-code-hash.rkt")

(define THIS-QTR 2158)

;; a small servlet that allows students to enter their lab numbers

(define (log-labcode-info fmt-string . args)
  (apply printf fmt-string args)
  (newline)
  (flush-output))

;; log a "successful" line
(define (log-successes login data)
  (log-labcode-info "successes: ~s ~s" login data))

;; log the failures
(define (log-failures login failure-data)
  (log-labcode-info "failures: ~s ~s" login failure-data))

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
    [(list login)
     (define nonempties
       (filter (lambda (pr)
                 (not (regexp-match #px"^\\s*$" (cadr pr))))
               pairs))
     (when (null? nonempties)
       "all entries were blank")
     (define results
       (for/list ([pair (in-list nonempties)])
         (list (car pair)
               (equal?
                (string-downcase (cadr pair))
                (bytes->string/utf-8
                 (compute-hash (string-downcase login)
                               THIS-QTR
                               (car pair)))))))
     (define-values (successes failures)
       (partition (lambda (result) (cadr result)) results))
     (define success-labs (map car successes))
     (define failure-labs (map car failures))
     (log-successes login success-labs)
     (log-failures login failure-labs)
     (success-page success-labs
                   failure-labs)]
    [other
     "Interval Server Error 22732nth2412..."]
  ))

(module+ test
  (require rackunit)
  (check-equal? (handle-bindings '("clements")
                                 '((1 "zzzz")
                                   (3 "w46vi3")
                                   (4 "abchtd")))
                (success-page '(3)
                              '(1 4))))

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
              ,@(for/list ([i (in-range 1 20)])
                  `(p ,{(lab-formlet i) . =>* . codes}))
              (p ,{(submit #"go!") . =>* . foo})
              )
            (list login codes)))

;; start the interaction
(define (start req)
  (send/finish
   (response/xexpr
    `(html
      (body
       ,@(apply
          handle-bindings
          (send/formlet whole-lab-formlet)))))))

;; display a success page (incl. successes and failures)
(define (success-page successes failures)
  `((p "Successes: " ,(format "~a" successes))
    (p "If you see successes, these should now have been logged.")
    (p "Failures (don't recognize these numbers, in wrong slot, "
       "or belong to someone else): " ,(format "~a" failures))
    (p "If you see failures, don't panic. Perhaps you entered "
       "the number wrong, or perhaps I gave it to you wrong. "
       "Find me and we'll figure it out.")))




(module+ main
  ;; start the servlet on port 8026
  (with-handlers ([(λ (exn) #t)
                   (λ (exn)
                     (log-error (exn-message exn))
                     (raise exn))])
    (serve/servlet start
                   #:port 8026
                   #:listen-ip #f
                   #:launch-browser? #f))  )



