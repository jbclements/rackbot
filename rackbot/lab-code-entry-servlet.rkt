#lang racket

(require web-server/formlets
         web-server/servlet-env
         web-server/servlet/web
         web-server/http/xexpr
         racket/runtime-path
         racket/date
         "lab-code-hash.rkt")

;; a small servlet that allows students to enter their lab numbers

(define-logger labcode)

;; log a "successful" line
(define (log-successes data)
  (log-labcode-info "successes: ~s" data))

;; log the failures
(define (log-failures failure-data)
  (log-labcode-info "failures" failure-data))

;(define-type SuccessResult (Pair 'success (Pair String (Listof Any))))
;(define-predicate success-result? SuccessResult)
(define (success-result? r) (eq? (first r) 'success))
#;(define-type MatchResult
  (U SuccessResult
     (Pair 'fail (Listof Any))))

;; given a slot on the page and an entered string, return a match result for it
#;(define (try-number slot-num input-str)
  (match (regexp-match #px"^\\s*([[:digit:]]+)\\s*$" input-str)
        [#f (error-page (format "expected numeric string, got ~s"
                                input-str))]
        [(list whole num-part)
         (define num-part-num
           (cond [(string? num-part)
                  (define result (string->number num-part))
                  (cond [(number? result)
                         result]
                        [else 
                         (error 'handle-bindings
                                 "secret number wasn't a number")])]
                 [else (error 'handle-bindings "secret number... hmmm....")]))
         (define matches
           (filter (lambda (lcn)
                     (= num-part-num (first (second lcn))))
                   lab-completion-numbers))
         (define match-results
           (for/list
             ([r matches])
             (match r
               [(list labnum (cons dc (cons id rest)))
                (cond [(= labnum slot-num)
                       `(success ,id ,labnum ,num-part)]
                      [else
                       `(fail ,num-part wrong-lab ,id ,slot-num ,labnum)])]
               [else (error-page "internal error, format of classnum file")])))
         (cond [(empty? match-results)
                `(fail ,num-part not-found)]
               [else
                (match (filter (lambda (x)
                                 (eq? (first x) 'success))
                               match-results)
                  ;; no successes, return the first fail
                  [(list) (first match-results)]
                  ;; one success, return it:
                  [(list a) a]
                  [other (error 'handle-bindings
                                "internal error, multiple success cases!")])])]))

;; accept the bindings passed by the user for the web request.
;; actually doesn't return...
(define (handle-bindings login pairs)
  (define nonempties
    (filter (lambda (pr)
              (not (regexp-match "^\\s*$" 
                                 (second pr))))
            pairs))
  (when (empty? nonempties)
    (error-page "all entries were blank"))
  (error-page "unimplemented...")
  #;((define matches
    (for/list ([n nonempties])
      (try-number (first n) (second n))))
  (define successes (filter success-result? matches))
  (define failures (filter (lambda (m)
                             (not (success-result? m)))
                           matches))
  (cond [(empty? successes)
         (log-failures empty failures)
         (success-page successes  failures)]
        [else
         (define most-common-id (most-common 
                                 (map second successes)))
         (define-values  (my-successes other-successes)
           (partition (lambda (x)
                        (equal? (success-second x) most-common-id)) successes))
         (log-successes my-successes)
         (log-failures other-successes failures)
         (define concealed-failures
           (map (lambda (x)
                  `(fail ,(fourth x) wrong-person x)) other-successes))
         (success-page my-successes (append concealed-failures failures))])))

(define success-second second)

;; most-common: return the most common element of a list, pick
;; one of the most common if no one is most common.
(define (most-common l)
  (define tabulated
    (for/fold ([r (hash)])
      ([elt l])
      (hash-set r elt (add1 (hash-ref r elt (lambda () 0))))))
  (define results-list (hash->list tabulated))
  (car 
   (first
    (sort results-list > #:key cdr))))

(module+ test
  (require rackunit)
  (check-equal? (most-common `(a c b c d d c)) 'c))


;; the formlet that accepts a completion number
(define (lab-formlet n)
  (formlet 
   (div 
    ,(format "lab ~a completion code: " n) ,{input-string . => . sekrit})
   (list n sekrit)))

;; the formlet that accepts a date
(define whole-lab-formlet
  (formlet* `(div
              (p "login id: " ,{(text-input) . =>* . login})
              ,@(for/list ([i (in-range 1 20)])
                  `(p ,{(lab-formlet i) . =>* . codes}))
              (p ,{(submit #"go!") . =>* . foo})
              )
            (list login codes)))

;; start the interaction
(define (start req)
  (apply
   handle-bindings
   (send/formlet whole-lab-formlet)))

;; signal an error
(define (error-page message)
  (send/suspend
   (lambda (k-url)
     (response/xexpr
      `(html (body
              ,message))))))

;; display a success page (incl. successes and failures)
(define (success-page successes failures)
  (define displayed-failures
    (map (lambda (x) (take x 2)) failures))
  (send/suspend
   (lambda (k-url)
     (response/xexpr 
      `(html 
        (body
         (p "Successes: " ,(format "~a" successes))
         (p "If you see successes, these should now have been logged.")
         (p "Failures (don't recognize these numbers, in wrong slot, "
            "or belong to someone else): " ,(format "~a" displayed-failures))
         (p "If you see failures, don't panic. Perhaps you entered "
            "the number wrong, or perhaps I gave it to you wrong. "
            "Find me and we'll figure it out.")))))))


;; start the servlet on port 8026
(serve/servlet start
               #:port 8026
               #:listen-ip #f
               #:launch-browser? #f)



