#lang at-exp racket

(provide request!
         try-read-json
         github-request!
         current-github-token
         GET POST)

(require net/url
         json
         racket/runtime-path
         "../common/util.rkt")


(define-values {GET POST} (values 'get 'post))

(define (request! url
                  headers
                  [read-output (λ (code in) (port->string in))]
                  #:method [method GET]
                  #:data [post-data #f])

  (define (read-output/parse-status in)
    (define header (purify-port in))
    (define code (match header
                   [(regexp @pregexp{HTTP/[^ ]* +(\d+)} (list _ code))
                    (string->number code)]
                   [else -1]))
    (read-output code in))
  (call/input-url (string->url url)
                  (λ (url)
                    (define req-f
                      (match method
                        [(== GET)  get-impure-port]
                        [(== POST) (λ (url headers)
                                     (post-impure-port url post-data headers))]))
                    (req-f url headers))
                  read-output/parse-status))

(define/contract current-github-api-base-url
  (parameter/c #rx"^https://.+[^/]$")
  (make-parameter "https://api.github.com"))
(define-runtime-path token-file "../../../../.gh-token.gpg")
(define current-github-token
  (make-parameter (and (file-exists? token-file)
                       (system/string @~a{ gpg --batch -dq @token-file}))))

(define (try-read-json code in)
  (define in-str (port->string in))
  (with-handlers ([exn:fail:read? (λ _ in-str)])
    (call-with-input-string in-str read-json)))

(define (github-request! request-url
                         #:output-type [output-type "application/vnd.github.v3+json"]
                         #:auth-token [token (current-github-token)]
                         #:base-url [base-url (current-github-api-base-url)]
                         #:extra-headers [extra-headers '()]
                         #:method [method GET]
                         #:data [post-data #f]
                         #:read-response [read-response try-read-json]
                         #:user-agent [user-agent "software-construction-admin"])
  (request!
   (if (string-prefix? request-url "http")
       request-url
       @~a{@|base-url|@(if (string-prefix? request-url "/") "" "/")@request-url})
   (list* "Github-API-Version: 3"
          @~a{Accept: @output-type}
          @~a{Authorization: token @token}
          @~a{User-Agent: @user-agent}
          extra-headers)
   read-response
   #:method method
   #:data post-data))
