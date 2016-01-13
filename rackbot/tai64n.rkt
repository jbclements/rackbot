#lang typed/racket

(require/typed srfi/19
               [#:opaque Time time?]
               [make-time
                (Time-Symbol Integer Integer -> Time)])

(define-type Time-Symbol (U 'time-duration
                            'time-monotonic
                            'time-process
                            'time-tai
                            'time-thread
                            'time-utc))

(provide parse-tai64n
         Time)

;; parses a tai64n time, following
;; http://cr.yp.to/libtai/tai64.html#tai64n

(define-predicate natural? Natural)

;; given a tai64n string, produce a srfi 19 'time-tai structure
(: parse-tai64n (String -> Time))
(define (parse-tai64n in)
  (unless (regexp-match #px"^[0-9a-f]{24}$" in)
    (raise-argument-error
     'parse-tai64n
     "string of lowercase hexadecimal digits of length 24"
     0 in))
  in
  (match (list (string->number (substring in 0 16) 16)
               (string->number (substring in 16 24) 16))
    [(list (? natural? sec)
           (? natural? nsec))
     (define second (- sec (expt 2 62)))
     (make-time 'time-tai nsec second)]
    [other
     (error 'parse-tai64n
            "internal error, should be unreachable 560424ef")]))

(module+ test
  (require typed/rackunit)
  (require/typed srfi/19
               [#:opaque Date date?]
               [make-date
                (Integer Integer Integer Integer
                         Integer Integer Integer Integer -> Date)]
               [date->time-tai (Date -> Time)])
  
  (check-exn #px"hexadecimal digits"
             (lambda ()
               (parse-tai64n "abcdefghijklmn")))
  
  (check-exn #px"hexadecimal digits of length 24"
             (lambda ()
               (parse-tai64n "0000")))

  ;; from TAI64N docs:
  ;; "4000000037c219bf2ef02e94" --> 1999-08-23 21:03:43.787492500
  
  (check-equal? (parse-tai64n "4000000037c219bf2ef02e94")
                (date->time-tai (make-date 787492500 43 3 4 24 8 1999 0))))
