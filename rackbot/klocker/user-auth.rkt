#lang typed/racket

(require racket/runtime-path)

(require/typed file/md5
               [md5 (Bytes -> Bytes)])

(provide valid-password?)

(define-runtime-path here ".")

(: get-user-pwd-hash (String -> String))
(define (get-user-pwd-hash username)
  (match (get-preference (string->symbol username) (lambda () #f) 'timestamp
                         (build-path here "users.rktd"))
    [(list-rest (? string? s) _1) s]
    [#f (error 'get-user-pwd-hash (format "no user named ~v" username))]
    [other (error 'get-user-pwd-hash
                  "internal error 2016-01-23")]))

;; check to see that this uid/pwd pair matches one in
;; users.rktd
(: valid-password? (String String -> Boolean))
(define (valid-password? uid pwd)
  (equal?
   (bytes->string/latin-1 (md5 (string->bytes/utf-8 pwd)))
   (get-user-pwd-hash uid)))


(module+ test
  (require typed/rackunit)
  (check-not-exn (λ () (get-user-pwd-hash "clements"))))



