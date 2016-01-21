#lang setup/infotab

(define collection 'multi)

(define deps
  (list "base"
        "sha"
        "web-server-lib"
        "srfi-lite-lib"
        "typed-racket-lib"))

(define build-deps
  (list "rackunit-lib"
        "typed-racket-more"))

