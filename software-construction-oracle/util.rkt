#lang at-exp racket

(provide (all-defined-out))

(require json
         "logger.rkt")

(define bad-json
  (let ()
    (struct bad ())
    (bad)))

(define (read-json/safe port)
  (with-handlers ([exn:fail:read? (const bad-json)])
    (read-json port)))

(define (valid-json? port)
  (not (eq? (read-json/safe port) bad-json)))

(define (valid-json-file? path)
  (log-fest debug
            @~a{Checking json validity for file @(pretty-path path)})
  (call-with-input-file path
    valid-json?
    #:mode 'text))

(define (jsexpr=? a b)
  (equal? a b)
  #;(or (equal? a b)
      (match* {a b}
        [{(? list?) (? list?)}
         (unordered-list=? a b jsexpr=?)]
        [{(? hash-eq?) (? hash-eq?)}
         (and (set=? (hash-keys a) (hash-keys b))
              (for/and ([(k v/a) (in-hash a)])
                (jsexpr=? v/a (hash-ref b k))))])))

;; (define (unordered-list=? a b [=? equal?])
;;   (and (= (length a)
;;           (length b))
;;        (for/and ([v/a (in-list a)])
;;          (member v/a b =?))))

;; (module+ test
;;   (require rackunit)
;;   (check-true (unordered-list=? '()
;;                                 '()))
;;   (check-true (unordered-list=? '(1)
;;                                 '(1)))
;;   (check-true (unordered-list=? '(1 2 3)
;;                                 '(2 3 1)))

;;   (check-true (jsexpr=? "a" "a"))
;;   (check-true (jsexpr=? #t #t))
;;   (check-true (jsexpr=? 1 1))
;;   (check-true (jsexpr=? 100.2 100.2))
;;   (check-true (jsexpr=? 'null 'null))
;;   (check-false (jsexpr=? #t "#t"))

;;   (check-true (jsexpr=? '(1 ))))



(define (build-path-string . args)
  (path->string (apply build-path args)))
(define (simple-form-path-string p)
  (path->string (simple-form-path p)))
(define (find-relative-path-string base path)
  (path->string (find-relative-path (simple-form-path base)
                                    (simple-form-path path))))

(define (basename p #:with-directory? [dir? #f])
  (define-values {dir name _2} (split-path p))
  (define name-str (path->string name))
  (define dir-str (path->string dir))
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
