#lang at-exp racket

(provide check-test-pass
         valid-json?
         valid-json-file?
         for-each-test-in
         clone-repo)

(require json
         "test-fest-data.rkt"
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
  (call-with-input-file path
    valid-json?
    #:mode 'text))

(define racket-exe (find-executable-path "racket"))
(define git-exe (find-executable-path "git"))

(define (check-test-pass exe-path input-file output-file
                         #:run-with-racket? [run-with-racket? #f])
  (define expected-output
    (call-with-input-file output-file
      read-json/safe
      #:mode 'text))
  (define in-port (open-input-file input-file))
  (match-define (list stdout #f pid #f ctl)
    (apply process*/ports
           #f in-port 'stdout
           (if run-with-racket?
               (list racket-exe exe-path)
               (list exe-path))))
  (ctl 'wait)
  (define exe-output (read-json/safe stdout))
  (when (eq? exe-output bad-json)
    (log-fest warning @~a{@exe-path produces invalid json!}))
  (close-input-port stdout)
  (log-fest
   debug
   @~a{@|exe-path|: expect: @~v[expected-output], actual: @~v[exe-output]})
  (jsexpr=? expected-output exe-output))

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


(define (for-each-test-in dir-path
                          do
                          #:check-json-validity [check-json-validity #t]
                          #:check-other-validity [check-other-validity #f])
  (for* ([test-input/path (in-directory dir-path)]
         [test-input (in-value (path->string test-input/path))]
         #:when (test-input-file? test-input)
         [test-output (in-value (test-input-file->output-file test-input))])
    (cond [(not (file-exists? test-output))
           (log-fest info @~a{Skipping @test-input, missing output file.})]
          [(and check-json-validity
                (not (valid-json-file? test-input)))
           (log-fest info @~a{Skipping @test-input, invalid json input.})]
          [(and check-json-validity
                (not (valid-json-file? test-output)))
           (log-fest info @~a{Skipping @test-input, invalid json output.})]
          [(and check-other-validity
                (not (check-other-validity test-input test-output)))
           (log-fest info @~a{Skipping @test-input, fails validity test.})]
          [else
           (do test-input test-output)])))

(define (clone-repo repo-name into)
  (define repo-url (repo-name->url repo-name))
  (log-fest debug @~a{Cloning @repo-name ...})
  (system @~a{git clone "@repo-url" @into > /dev/null 2>&1})
  (log-fest debug @~a{Done.}))
