#lang at-exp racket

(provide (all-defined-out))

(require json
         syntax/parse/define
         "logger.rkt")

(define bad-json
  (let ()
    (struct bad-json ())
    (bad-json)))

(define (read-json/safe port)
  (with-handlers ([exn:fail:read? (λ (e)
                                    (log-fest-error (exn-message e))
                                    bad-json)])
    (match (read-json port)
      [(? eof-object?) bad-json]
      [json json])))

(define (valid-json? port)
  (not (eq? (read-json/safe port) bad-json)))

(define (valid-json-file? path)
  (log-fest-debug
            @~a{Checking json validity for file @(pretty-path path)})
  (call-with-input-file path
    valid-json?
    #:mode 'text))

(define (jsexpr=? a b)
  (equal? a b))

(define (build-path-string . args)
  (path->string (apply build-path args)))
(define (simple-form-path-string p)
  (path->string (simple-form-path p)))
(define (find-relative-path-string base path)
  (path->string (find-relative-path (simple-form-path base)
                                    (simple-form-path path))))

(define (basename p #:with-directory? [dir? #f])
  (define-values {dir-path name-path}
    (match (explode-path (simple-form-path p))
      [(list name)
       (values (current-directory)
               name)]
      [(list parent-path-parts ... name)
       (values (apply build-path parent-path-parts) name)]))
  (define dir-str (path->string dir-path))
  (define name-str (path->string name-path))
  (if dir?
      (values dir-str name-str)
      name-str))

(define/contract (call-with-extended-environment env-vars thunk)
  ((hash/c string-environment-variable-name? string-no-nuls?)
   (-> any)
   . -> .
   any)

  (parameterize ([current-environment-variables
                  (environment-variables-copy (current-environment-variables))])
    (for ([(k v) (in-hash env-vars)])
      (putenv k v))
    (thunk)))

(define/contract (round-up x)
  (number? . -> . integer?)
  (truncate (ceiling x)))

(define path-to-existant-directory?
  (and/c path-string? directory-exists?))
(define path-to-existant-file?
  (and/c path-string? file-exists?))

(define (pretty-path path)
  (path->string
   (find-relative-path (simple-form-path ".")
                       (simple-form-path path))))

(define (system/string cmd)
  (call-with-output-string
   (λ (out)
     (parameterize ([current-output-port out]
                    [current-error-port out])
       (system cmd)))))

(define-simple-macro (for/hash/fold for-clauses
                                    {~optional {~seq #:init initial-hash}
                                               #:defaults ([initial-hash #'(hash)])}
                                    #:combine combine
                                    #:default default
                                    body ...)
  (for/fold ([result-hash initial-hash])
            for-clauses
    (define-values {key value} (let () body ...))
    (hash-update result-hash
                 key
                 (λ (accumulator) (combine value accumulator))
                 default)))

(define (try-decode-bytes->string bytes)
  (with-handlers ([exn:fail? (λ _ (~s bytes))])
    (bytes->string/utf-8 bytes)))

(define/contract (matches-any? los some-str)
  ((listof string?) string? . -> . boolean?)

  (for/or ([s (in-list los)])
    (string-contains? s some-str)))

(define (user-prompt!* msg raw-options [none-result 'none])
  (define options (map (compose1 string-downcase ~a) raw-options))
  (display @~a{@msg [@(string-upcase (first options))/@(string-join (rest options) "/")]: })
  (flush-output)
  (define answer (string-trim (string-downcase (read-line))))
  (or (and (string=? answer "") (first raw-options))
      (for/first ([raw-option (in-list raw-options)]
                  [option-str (in-list options)]
                  #:when (string-prefix? answer option-str))
        raw-option)
      none-result))

(define (user-prompt! msg)
  (match (user-prompt!* msg '(y n))
    ['y #t]
    [else #f]))

(define system-temp-dir (find-system-path 'temp-dir))
;; Call f with a new temp directory.
;; Delete the directory after f returns.
;; Result is whatever f produces.
(define (call-with-temp-directory f
                                  #:name-seed [seed "sc"]
                                  #:name [name (~a seed (current-milliseconds))])
  (define temp-dir (build-path system-temp-dir name))
  (log-sc-debug @~a{Making temp dir at @temp-dir})
  (dynamic-wind
    (thunk (make-directory temp-dir))
    (thunk (f temp-dir))
    (thunk (delete-directory/files temp-dir))))

(define/contract (clean-directory! dir preserve)
  (path-to-existant-directory? (listof string?) . -> . any)

  (for ([f (in-list (directory-list dir))]
        #:unless (matches-any? preserve (path->string f)))
    (define f-path (build-path dir f))
    (displayln @~a{Deleting @f-path})
    (if (file-exists? f-path)
        (delete-file f-path)
        (delete-directory/files f-path))))
