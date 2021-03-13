#lang at-exp racket

(provide valid-tests
         valid-tests/passing-oracle
         test-failures-for
         log-test-failure-comparison?)

(require json
         "util.rkt"
         "logger.rkt"
         "process.rkt"
         "tests.rkt")

;; The CI kills any job running longer than 115 min
(define absolute-max-timeout-seconds (* 115 60))

(define oracle-timeout-seconds (* 1 60))
(define (oracle->student-timeout secs) (* 100 secs))


;; Values being compared may be large, so they can be suppressed to prevent
;; filling up the log when other `info`-level information is desired
(define log-test-failure-comparison? (make-parameter #t))

(define/contract (run-exe-on-input exe-path
                                   input-json-file
                                   [timeout-seconds absolute-max-timeout-seconds])
  ({path-to-existant-file? path-to-existant-file?}
   {natural?}
   . ->* .
   (or/c bytes? #f))

  (define stdin (open-input-file input-json-file))
  (define-values {proc stdout stderr}
    ;; ll: Can't use pipe here because that's not a `file-stream-port?`
    (launch-process! exe-path
                     #:stdin stdin
                     #:stdout #f
                     #:stderr #f
                     #:limit-stdout? #t
                     #:limit-stderr? #t))

  (close-input-port stdin) ;; Make sure process gets eof

  (define terminated? (sync/timeout timeout-seconds proc))

  (subprocess-kill proc #t) ;; Ensure the process is dead

  (log-fest-debug @~a{@(pretty-path exe-path) done.})
  (log-fest-debug @~a{Closing exe ports})

  (log-fest-debug @~a{Reading exe output})
  (define stderr-bytes (port->bytes stderr))
  (define stderr-empty? (equal? stderr-bytes #""))
  (define stdout-bytes
    (cond [(not stderr-empty?)
           (log-fest-error
            @~a{
                @(pretty-path exe-path) produced output on stderr:
                ------------------------------
                @(try-decode-bytes->string stderr-bytes)
                ------------------------------
                })
           #f]
          [terminated?
           (port->bytes stdout)]
          [else
           (log-fest-error
            @~a{@(pretty-path exe-path) timed out (@|timeout-seconds|s)})
           #f]))
  (close-input-port stdout)
  (close-input-port stderr)
  stdout-bytes)

(define/contract (exe-passes-test? exe-path oracle-path t)
  (path-to-existant-file?
   path-to-existant-file?
   test/c
   . -> .
   boolean?)

  (define input-file (test-input-file t))

  (log-fest-debug @~a{Running the oracle on test @(basename input-file) ...})
  (match-define-values {(list oracle-output-bytes) _ oracle-time-ms _}
    (time-apply run-exe-on-input (list oracle-path input-file oracle-timeout-seconds)))

  (log-fest-debug @~a{Running @(pretty-path exe-path) on test @(basename input-file) ...})
  (define exe-timeout-seconds (ceiling (oracle->student-timeout (/ oracle-time-ms 1000))))
  (define exe-output-bytes (run-exe-on-input exe-path input-file exe-timeout-seconds))
  (log-fest-debug @~a{
                      The output of @(pretty-path exe-path) was:
                      ------------------------------
                      @(try-decode-bytes->string exe-output-bytes)
                      ------------------------------
                      })

  (define oracle-output-json
    (if (not oracle-output-bytes)
        bad-json
        (call-with-input-bytes oracle-output-bytes read-json/safe)))
  (define exe-output-json
    (if (not exe-output-bytes)
        bad-json
        (call-with-input-bytes exe-output-bytes read-json/safe)))

  (cond [(equal? oracle-output-json bad-json)
         (log-fest-error "The oracle seems to be confused. Giving up on this test.")
         #f]
        [(not exe-output-bytes)
         (log-fest-error @~a{
                             @(pretty-path exe-path) fails test @(basename input-file) @;
                             because something went wrong while running it.
                             })
         #f]
        [(equal? exe-output-json bad-json)
         (log-fest-error @~a{
                             @(pretty-path exe-path) fails test @(basename input-file) @;
                             because it produced invalid json.
                             The output is below. @;
                             @(if (= (bytes-length exe-output-bytes) process-stdout-bytes-limit)
                                  @~a{
                                      It hit the @process-stdout-bytes-limit @;
                                      bytes size limit, which might be why it was invalid.
                                      }
                                  "")
                             ------------------------------
                             @(try-decode-bytes->string exe-output-bytes)
                             ------------------------------
                             })
         #f]
        [(not (jsexpr=? exe-output-json oracle-output-json))
         (log-fest-error @~a{
                             @(pretty-path exe-path) fails test @(basename input-file) @;
                             because it produced the wrong result.
                             It produced this:
                             ------------------------------
                             @(with-output-to-string (thunk (write-json exe-output-json)))
                             ------------------------------
                             })
         (when (log-test-failure-comparison?)
           (log-fest-error @~a{
                               The expected json for this test is:
                               ------------------------------
                               @(with-output-to-string (thunk (write-json oracle-output-json)))
                               ------------------------------
                               })
           (log-test-failure-comparison? #f))
         #f]
        [else #t]))

(define/contract (valid-tests test-directory
                              check-validity
                              #:check-json-validity? [check-json-validity? #t]
                              #:test-timeout [test-timeout absolute-max-timeout-seconds]
                              #:max-count [max-count +inf.0])
  (->* {path-to-existant-directory?
        (path-to-existant-file? path-to-existant-file? . -> . boolean?)}
       {#:check-json-validity? boolean?
        #:test-timeout natural?
        #:max-count natural?}
       (listof test/c))

  (define all-tests (directory->tests test-directory))
  (define valid
    (filter (match-lambda [(test input-file output-file)
                           (cond [(not (file-exists? output-file))
                                  (log-fest-info
                                   @~a{Skip @input-file, missing output file.})
                                  #f]
                                 [(and check-json-validity?
                                       (not (valid-json-file? input-file)))
                                  (log-fest-info
                                   @~a{Skip @input-file, invalid json input.})
                                  #f]
                                 [(and check-json-validity?
                                       (not (valid-json-file? output-file)))
                                  (log-fest-info
                                   @~a{Skip @input-file, invalid json output.})
                                  #f]
                                 [(not (check-validity input-file output-file))
                                  (log-fest-info
                                   @~a{Skip @input-file, fails validity test.})
                                  #f]
                                 [else #t])])
            all-tests))
  (take valid (min max-count (length valid))))

(define/contract (valid-tests/passing-oracle test-directory
                                             oracle-path
                                             #:check-json-validity?
                                             [check-json-validity? #t])
  (->* {path-to-existant-directory?
        path-to-existant-file?}
       {#:check-json-validity? boolean?}
       (listof test/c))

  (valid-tests test-directory
               #:check-json-validity? #t
               (λ (in out) ;; Now we can assume the input and output both have valid json
                 (define oracle-output (run-exe-on-input oracle-path in))
                 (define oracle-output-json (call-with-input-bytes oracle-output read-json/safe))
                 (define expected-output-json (call-with-input-file out read-json/safe))
                 (jsexpr=? oracle-output-json expected-output-json))))

(define/contract (test-failures-for exe-path oracle-path tests-by-group)
  (path-to-existant-file? path-to-existant-file? test-set/c . -> . test-set/c)

  (define (passes-test? t)
    (exe-passes-test? exe-path oracle-path t))
  (for*/hash ([group (in-list (sort (hash-keys tests-by-group) string<?))]
              [tests (in-value (sort (hash-ref tests-by-group group)
                                     string<?
                                     #:key (compose1 ~a test-input-file)))]
              [failed-tests (in-value (filter-not passes-test? tests))]
              #:unless (empty? failed-tests))
    (values group failed-tests)))
