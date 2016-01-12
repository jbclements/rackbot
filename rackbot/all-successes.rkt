#lang typed/racket

(provide all-successes)

(require "tai64n.rkt")
(require/typed srfi/19
               [#:opaque Time time?]
               [time-second (Time -> Integer)]
               [time-type (Time -> Time-Symbol)])

(define-type Time-Symbol (U 'time-duration
                            'time-monotonic
                            'time-process
                            'time-tai
                            'time-thread
                            'time-utc))

(define LOG-PATH "/var/log/rackbot/current")

(define-type User String)
(define-type Labnum Natural)
(define-type Info (List User (Listof Labnum)))
(define-type Infos (Listof Info))

;; return all of the recorded successes in the default file
(: all-successes (-> Infos))
(define (all-successes)
  (path->successes LOG-PATH))

;; return all successes since a specified TAI second
;; ... it may seem crazy to require a TAI second rather
;; than just a srfi/19 time, but srfi/19 provides no
;; means to compare times of different kinds, so they're
;; both going to have to be TAI times, so I might as well
;; just drill down and require the TAI second.
;; ... In fact, if you just pass UTC seconds you won't be too
;; far wrong (about 12 seconds, IIRC).
(: successes-since (Natural -> Infos))
(define (successes-since t)
  (path->successes LOG-PATH
                   (λ (i) (<= t (time-seconds i)))))

;; return all of the recorded successes in a given file
;; that satisfy the time-pred
(: path->successes (Path-String Time-Pred -> Infos))
(define (path->successes path time-pred)
  (define lines (file->lines path))
  (define successes (lines->successes lines time-pred))
  (group-successes successes))

;; group successes by user
(: group-successes ((Listof String) -> Infos))
(define (group-successes lines)
  (: successes (Listof Info))
  (define successes
    (filter Info? (map process-line lines)))
  (define grouped (group-by (ann first (Info -> String)) successes))
  (for/list ([group (in-list grouped)])
    (list (caar group)
          (sort
           (remove-duplicates (apply append
                                     (map (ann cadr (Info -> (Listof Labnum)))
                                          group)))
           <))))

;; given a list of lines, return the Infos of the successes whose timestamps
;; satisfy the given predicate
(: lines->successes ((Listof String) -> (Listof Info)))
(define (lines->successes lines pred)
  (for/list ([l lines]
             #:when (keep? l pred))
    (parse l)))



;; given a line, return false if it doesn't match the success
;; regexp, or a list containing two strings otherwise
(: process-line (String -> (U False (List User (Listof Labnum)))))
(define (process-line l) 
  (match (regexp-match SUCCESS-REGEXP l)
    [#f #f]
    [(list _1 (? string? id) (? string? labs))
     (: nums (Listof Natural))
     (define nums
       (match (read (open-input-string labs))
         [(? ListOfNats? lon) lon]))
     (list id nums)]
    [other
     (error 'parse-successes
            "internal error, regexp didn't produce expected result")]))

(define-predicate ListOfStrs? (Listof String))
(define-predicate ListOfNats? (Listof Natural))
(define-predicate Info? Info)



(define SUCCESS-REGEXP
  #px"^@[0-9a-f]+ successes: \"([^\"]+)\" (.*)$")


(module+ test
  (require typed/rackunit)

  (check-equal?
   (group-successes
    (list
     "@873972 successes: \"football\" (9 3)"
     "@873972 successes: \"soccer\" (2)"
     "@873972 successes: \"\" (9 3 23)"
     "@873972 successes: \"football\" (10 3)"
     "@873972 successes: \"larry\" ()"))
   '(("football" (3 9 10))
     ("soccer" (2))
     ("larry" ()))))