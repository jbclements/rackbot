#lang racket

(provide (contract-out
          [all-successes
           (-> (listof (list/c string? (listof nat?))))]))

(define nat? exact-nonnegative-integer?)

(define LOG-PATH "/var/log/rackbot/current")

;; group successes by user
(define (group-successes successes)
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
          (apply append (map cadr group)))))

(define SUCCESS-REGEXP
  #px"^@[0-9a-f]+ successes: \"([^\"]+)\" (.*)$")

(define (path->successes path)
  (define lines (file->lines path))
  
  (define successes
    (filter (λ(l) (regexp-match SUCCESS-REGEXP l))
            lines))
  
  (group-successes successes))



(define (all-successes)
  (path->successes LOG-PATH))