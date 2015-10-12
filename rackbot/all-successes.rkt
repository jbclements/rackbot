#lang racket

(provide (contract-out
          [all-successes
           (-> (listof (list/c string? (listof nat?))))]))

(define nat? exact-nonnegative-integer?)

(define LOG-PATH "/var/log/rackbot/current")

;; group successes by user
(define (group-successes lines)
  (define successes
    (filter (λ(l) (regexp-match SUCCESS-REGEXP l))
            lines))
  (define grouped
    (group-by
     first
     (for/list ([s (in-list successes)])
       (match (regexp-match SUCCESS-REGEXP s)
         [(list _1 id labs)
          (list id (read (open-input-string labs)))]
         [other
          (error 'parse-successes "aoentuhant")]))))
  (for/list ([group (in-list grouped)])
    (list (caar group)
          (sort
           (remove-duplicates (apply append (map cadr group)))
           <))))

(define SUCCESS-REGEXP
  #px"^@[0-9a-f]+ successes: \"([^\"]+)\" (.*)$")

(define (path->successes path)
  (define lines (file->lines path))
  (group-successes lines))

(define (all-successes)
  (path->successes LOG-PATH))

(module+ test
  (require rackunit)

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