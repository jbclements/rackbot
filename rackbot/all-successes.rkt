#lang typed/racket

(provide all-successes
         successes-since)

(require "tai64n.rkt")
(require/typed srfi/19
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
(define-type NotSuccess 'not-success)
(define-type Success (List User Time
                           (Listof Labnum)))
(define-type LogLine (U NotSuccess Success))
(define-type Info (List User (Listof Labnum)))
(define-type Infos (Listof Info))

;; return all of the recorded successes in the default file
(: all-successes (-> Infos))
(define (all-successes)
  (path->successes LOG-PATH (λ (i) #t)))

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
                   (λ (i) (<= t (time-second (second i))))))

;; return all of the recorded successes in a given file
;; that satisfy the time-pred
(: path->successes (Path-String (Success -> Boolean) -> Infos))
(define (path->successes path time-pred)
  (define lines (map parse-line (file->lines path)))
  (: lines2 (Listof Success))
  (define lines2 (filter Success? lines))
  (define lines3 (filter time-pred lines2))
  (group-successes lines3))

;; group successes by user
(: group-successes ((Listof Success) -> (Listof Info)))
(define (group-successes successes)
  (define grouped (group-by (ann first (Success -> String)) successes))
  (for/list ([group (in-list grouped)])
    (list (caar group)
          (sort
           (remove-duplicates (apply append
                                     (map (ann caddr (Success -> (Listof Labnum)))
                                          group)))
           <))))


;; given a line, return false if it doesn't match the success
;; regexp, or a list containing two strings otherwise
(: parse-line (String -> LogLine))
(define (parse-line l)
  (match (regexp-match SUCCESS-REGEXP l)
    [#f 'not-success]
    [(list _1 (? string? timestamp) (? string? id) (? string? labs))
     (define ts (parse-tai64n timestamp))
     (: nums (Listof Labnum))
     (define nums
       (match (read (open-input-string labs))
         [(? ListOfNats? lon) lon]))
     (ann (list id ts (ann nums (Listof Labnum))) Success)]
    [other
     (error 'parse-successes
            "internal error, regexp didn't produce expected result")]))

(define-predicate ListOfStrs? (Listof String))
(define-predicate ListOfNats? (Listof Natural))
(define-predicate Info? Info)
(define-predicate Success? Success)



(define SUCCESS-REGEXP
  #px"^@([0-9a-f]+) successes: \"([^\"]+)\" (.*)$")


(module+ test
  (require typed/rackunit)

  (define test-tai64n "4000000037c219bf2ef02e94")
  (define test-time (parse-tai64n test-tai64n))
  (check-equal?
   (parse-line "@4000000037c219bf2ef02e94 successes: \"football\" (9 3)")
   (list "football" test-time (list 9 3)))

  (check-equal?
   (parse-line "@4000000037c219bf2ef02e94 suses: \"football\" (9 3)")
   'not-success)
  
  (check-equal?
   (group-successes
    (list
     (list "football" test-time '(9 3))
     (list "soccer" test-time '(2))
     (list "football" test-time '(10 3))
     (list "larry" test-time '())))
   '(("football" (3 9 10))
     ("soccer" (2))
     ("larry" ()))))