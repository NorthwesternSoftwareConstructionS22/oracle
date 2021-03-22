#lang at-exp racket

(provide request!
         try-read-json
         travis-request!
         current-travis-token
         GET POST)

(require net/url
         json
         racket/runtime-path
         "../common/util.rkt")


(define-values {GET POST} (values 'get 'post))

(define (request! url headers [read-output port->string]
                  #:method [method GET]
                  #:data [post-data #f])
  (call/input-url (string->url url)
                  (λ (url)
                    (define req-f
                      (match method
                        [(== GET)  get-pure-port]
                        [(== POST) (λ (url headers)
                                     (post-pure-port url post-data headers))]))
                    (req-f url headers))
                  read-output))

(define/contract current-travis-api-base-url
  (parameter/c #rx"^https://.+[^/]$")
  (make-parameter "https://api.travis-ci.com"))
(define-runtime-path token-file "../../../../.travis-token.gpg")
(define current-travis-token
  (make-parameter (and (file-exists? token-file)
                       (system/string @~a{ gpg --batch -dq @token-file}))))

(define (try-read-json in)
  (define in-str (port->string in))
  (with-handlers ([exn:fail:read? (λ _ in-str)])
    (call-with-input-string in-str read-json)))

(define (travis-request! request-url
                         #:output-type [output-type "application/json"]
                         #:auth-token [token (current-travis-token)]
                         #:base-url [base-url (current-travis-api-base-url)]
                         #:extra-headers [extra-headers '()]
                         #:method [method GET]
                         #:data [post-data #f]
                         #:read-response [read-response try-read-json]
                         #:user-agent [user-agent "software-construction-admin"])
  (request!
   (if (string-prefix? request-url "http")
       request-url
       @~a{@|base-url|@(if (string-prefix? request-url "/") "" "/")@request-url})
   (list* "Travis-API-Version: 3"
          @~a{Accept: @output-type}
          @~a{Authorization: token @token}
          @~a{User-Agent: @user-agent}
          extra-headers)
   read-response
   #:method method
   #:data post-data))
