#lang racket/base

(require web-server/formlets
         web-server/servlet-env
         web-server/servlet/web
         web-server/http/xexpr
         racket/match
         racket/file
         (only-in racket/list shuffle range))


(define sample-text
  "miasma
mantelpiece
largesse
antediluvian
whisker pole
“Jimmy,” he said pleadingly, “I hankers fer somethin’ tuh take my mind offen the cares an’ troubles of my exactin’ duties. I craves some hard likker, the kind that falls with a clatter an’ lands with a bang. I’m plumb meloncolic, that’s what. Don’t keep me in expense no longer.”
The grinning bartender slid a bottle and glass across the battered “mahogany” and leaned his elbows upon the bar as he watched the fat deputy pour himself a drink.
“I’ll betcha business is rushin’,” he agreed facetiously. “What all yuh been pesterin’ around at this bright mornin’?”
“Collectin’ tacks.”")

(define lines (regexp-split #px"\n" sample-text))


(define LISTEN-PORT 8027)

(define server-extra-files-path
  (let ()
    (define pref-path (build-path (find-system-path 'pref-dir) "rackbot-prefs"))
    (and (file-exists? pref-path)
         (hash-ref (file->value pref-path)
                   'server-extra-files-path
                   #f))))
;; a small servlet that allows students to enter their lab numbers


;; put stars on either side of a string
(define (stars str)
  (string-append "*" str "*"))

;; swap first and last chars of a string
(define (swap-chars str)
  (cond [(< (string-length str) 2)
         (error 'swap-chars "expected a string of length 2 or longer")]
        [else
         (define l (string-length str))
         (string-append (substring str (sub1 l) l) (substring str 1 (sub1 l)) (substring str 0 1))]))

;; put the first letter at the end, add "ay"
(define (pig-latin str)
  (cond [(< (string-length str) 2)
         (error 'swap-chars "expected a string of length 2 or longer")]
        [else
         (define l (string-length str))
         (string-append (substring str 1 l) (substring str 0 1) "ay")]))

;; wrap the string in its length
(define (len-wrap str)
  (define len-str (number->string (string-length str)))
  (string-append len-str str len-str))

(define (middle-to-end str)
  (define l (string-length str))
  (define half-l (floor (/ l 2)))
  (cond [(= l 0)
         (error 'swap-chars "expected a string of length 2 or longer")]
        [else
         (string-append (substring str 0 half-l)
                        (substring str (add1 half-l) l)
                        (substring str half-l (add1 half-l)))]))



;; swap vowel pairs
(define (vowel-swap str)
  (list->string
   (map (λ (ch)
          (match ch
            [#\a #\e]
            [#\e #\i]
            [#\i #\o]
            [#\o #\u]
            [#\u #\a]
            [other other]))
        (string->list str))))

(define (string-reverse s)
  (list->string (reverse (string->list s))))

(define (flip-back str)
  (define l (string-length str))
  (define half-l (floor (/ l 2)))
  (string-append (string-reverse (substring str half-l l))
                        (substring str 0 half-l)))

(define (fencepost str)
  (define len (string-length str))
  (define l-even? (even? len))
  (list->string
   (for/list ([i (in-range len)])
     (cond [(even? i)
            (string-ref str i)]
           [else
            (cond [l-even?
                   (string-ref str (- len i))]
                  [else
                   (string-ref str (sub1 (- len i)))])]))))

(define (scytale-4 str)
  (define len (string-length str))
  (string-append
   (number->string len)
   "*"
   (list->string
    (apply
     append
     (for/list ([offset (in-range 4)])
       (for/list ([i (in-range (ceiling (/ len 4)))])
         (define idx (+ (* 4 i) offset))
         (cond [(>= idx len) #\-]
               [else
                (string-ref str idx)])))))))

(random-seed 9870)
(define substitution-map
  ;; just in case the RNG is different on different machines...
  #;(map (λ (x) (integer->char (+ x (char->integer #\a)))) (shuffle (range 26)))
  '(#\h
    #\f
    #\e
    #\u
    #\q
    #\j
    #\p
    #\s
    #\i
    #\z
    #\t
    #\b
    #\w
    #\v
    #\d
    #\n
    #\l
    #\x
    #\r
    #\o
    #\c
    #\y
    #\m
    #\k
    #\g
    #\a))
(define a-ch-int (char->integer #\a))
(define z-ch-int (char->integer #\z))
(define (substitution-cipher str)
  (list->string
   (for/list ([ch str])
     (define ch-int (char->integer ch))
     (cond [(<= a-ch-int ch-int z-ch-int)
            (list-ref substitution-map (- ch-int a-ch-int))]
           [else ch]))))

(define reverse-map
  (map list substitution-map (map (λ (i) (integer->char (+ a-ch-int i))) (range 0 26))))

(define (un-substitution-cipher str)
  (list->string
   (for/list ([ch str])
     (define ch-int (char->integer ch))
     (cond [(<= a-ch-int ch-int z-ch-int)
            (cadr (assoc ch reverse-map))]
           [else ch]))))



substitution-map


(define encryptors
  (list (list "A" stars 0)
        (list "B" swap-chars 1)
        (list "C" pig-latin 2)
        (list "D" middle-to-end 3)
        (list "LA" flip-back 4)
        (list "LB" vowel-swap 5)
        (list "LC" fencepost 6)
        (list "XA" scytale-4 7)
        (list "XB" substitution-cipher 8)
        ))




;; accept the bindings passed by the user for the web request.
(define (handle-bindings strs encryptor-strs)
  (match* (strs encryptor-strs)
    [((list str) (list encryptor-str))
     (define encryptor (cadr (assoc encryptor-str encryptors)))
     (define encryptor-idx (caddr (assoc encryptor-str encryptors)))
     (with-handlers ([exn:fail?
                      (λ (exn)
                        `(div
                          (p ,(format "An error occurred: ~a" (exn-message exn)))
                          (p "(Maybe go back and try again?)"))
                        )])
       (apply handle-bindings
              (send/formlet (followup-formlet str (encryptor str) encryptor-str
                                              (encryptor
                                               (list-ref lines encryptor-idx))))))]
    [(other other2)
     `(p "Internal Server Error 22732nth2412: ~a ~a" strs encryptor-strs)
     ]
  ))


(module+ test
  (require rackunit)

  (check-equal? (swap-chars "abcd") "dbca")

  (check-equal? (pig-latin "satin") "atinsay")

  (check-equal? (len-wrap "") "00")
  (check-equal? (len-wrap "abc") "3abc3")
  (check-equal? (len-wrap "i am a frog")  "11i am a frog11")

  (check-equal? (vowel-swap "abecedarium over")
                "ebicideroam uvir")
  (check-equal? (middle-to-end "abc") "acb")
  (check-equal? (middle-to-end "abcde") "abdec")
  (check-equal? (middle-to-end "a") "a")

  (check-equal? (flip-back "abcde") "edcab")
  (check-equal? (flip-back "repulsive") "evislrepu")
  (check-equal? (flip-back "") "")

  (check-equal? (fencepost "abcdefgh") "ahcfedgb")
  (check-equal? (fencepost "abcdefg") "afcdebg")

  (check-equal? (scytale-4 "i am a frog tomorrow")
                "20*i rtr aoora gmomf ow")
  (check-equal? (scytale-4 "i am a frog tomorrowx")
                "21*i rtrx aoor-a gmo-mf ow-")

  (check-equal? 
   (un-substitution-cipher (substitution-cipher "well, I went down to the river today"))
   "well, I went down to the river today")

  (check-equal?
   (un-substitution-cipher "Rqyqvpq ir h uirs osho ir fqro rqxyqu mios nhxrbqg hvu whgdvvhirq.")
   "Revenge is a dish that is best served with parsley and mayonnaise."))


(substitution-cipher "Revenge is a dish that is best served with parsley and mayonnaise.")

;; the formlet that accepts a date
(define initial-formlet
  (formlet* `(div
              (p "Choose an encryptor: ",{(select-input (map car encryptors) ) . =>* . encryptor})
              (p "enter a string to be encoded: " ,{input-string . =>* . str})
              (p ,{(submit "go") . =>* . dc})
              )
            (list str encryptor)))

(define (followup-formlet input-str encrypted-str encryptor encrypted-challenge)
  (formlet* `(div
              (p ,(format "Chosen encryptor: ~a" encryptor))
              (p "Given string: " (pre  ,input-str))
              (p "Encrypted string: " (pre ,encrypted-str))
              (p "Challenge string to be decrypted: " (pre ,encrypted-challenge))
              (p "Choose an encryptor: ",{(select-input (map car encryptors) #:selected? (λ (s) (equal? s encryptor))) . =>* . encryptor})
              (p "enter another string to be encoded: " ,{input-string . =>* . str})
              (p ,{(submit "go") . =>* . dc})
              )
            (list str encryptor)))

;; start the interaction
(define (start req)
  (send/suspend
   (lambda (url)
   (response/xexpr
    `(html
      (body
       ,@(apply
          handle-bindings
          (send/formlet initial-formlet))))))))


(module+ main
  (with-handlers ([(λ (exn) #t)
                   (λ (exn)
                     (log-error (exn-message exn))
                     (raise exn))])
    (serve/servlet start
                   #:servlet-regexp #px""
                   #:port LISTEN-PORT
                   #:listen-ip #f
                   #:launch-browser? #f
                   #:extra-files-paths
                   (cond [server-extra-files-path (list server-extra-files-path)]
                         [else '()]))))



