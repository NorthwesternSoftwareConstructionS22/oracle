#lang at-exp racket

(provide valid-tests
         valid-tests/passing-oracle
         test-failures-for
         log-test-failure-comparison?)

(require json
         "util.rkt"
         "test-fest-data.rkt"
         "logger.rkt"
         "process.rkt"
         "tests.rkt")

;; Travis kills any job running longer than 115 min
(define absolute-max-timeout-seconds (* 115 60))

(define oracle-timeout-seconds (* 1 60))
(define (oracle->student-timeout secs) (* 100 secs))


;; Values being compared may be large, so they can be suppressed to prevent
;; filling up the log when other `info`-level information is desired
(define log-test-failure-comparison? (make-parameter #t))

(define stderr-bytes-limit (* 16 1024))

(define/contract (run-exe-on-input exe-path
                                   input-json-file
                                   [timeout-seconds absolute-max-timeout-seconds])
  ({path-to-existant-file? path-to-existant-file?}
   {natural?}
   . ->* .
   (or/c bytes? #f))

  (define stdin (open-input-bytes (file->bytes input-json-file)))
  (define-values {stderr-read stderr-write} (make-pipe stderr-bytes-limit))
  (define-values {proc stdout}
    (launch-process! exe-path
                     #:stdin stdin
                     #:stderr stderr-write
                     #:limit-stdout? #t))
  (define terminated? (wait/keep-ci-alive proc timeout-seconds))

  (subprocess-kill proc #t) ;; Ensure the process is dead

  (log-fest-debug @~a{@(pretty-path exe-path) done.})
  (log-fest-debug @~a{Closing exe ports})
  (close-output-port stderr-write)

  (log-fest-debug @~a{Reading exe output})
  (define stderr-bytes (port->bytes stderr-read))
  (define stdout-bytes
    (cond [(not (equal? stderr-bytes #""))
           (log-fest-error
            @~a{
                @(pretty-path exe-path) produced output on stderr, so it fails this test.
                The stderr output was:
                ------------------------------
                @(try-decode-bytes->string stderr-bytes)
                ------------------------------
                })
           #f]
          [terminated?
           (define stdout-bytes (port->bytes stdout))
           (log-fest-debug @~a{
                               The stdout output was:
                               ------------------------------
                               @(try-decode-bytes->string stdout-bytes)
                               ------------------------------
                               })
           stdout-bytes]
          [else
           (log-fest-error
            @~a{@(pretty-path exe-path) timed out (@|timeout-seconds|s)})
           #f]))
  (close-output-port stdout)
  stdout-bytes)

(define/contract (exe-passes-test? exe-path oracle-path t)
  (path-to-existant-file?
   path-to-existant-file?
   test/c
   . -> .
   boolean?)

  (define input-file (test-input-file t))

  (log-fest-debug @~a{Running the oracle on test @(pretty-path input-file) ...})
  (match-define-values {(list oracle-output-bytes) _ oracle-time-ms _}
    (time-apply run-exe-on-input (list oracle-path input-file oracle-timeout-seconds)))

  (log-fest-debug @~a{Running @(pretty-path exe-path) on test @(pretty-path input-file) ...})
  (define exe-timeout-seconds (oracle->student-timeout (/ oracle-time-ms 1000)))
  (define exe-output-bytes (run-exe-on-input exe-path input-file exe-timeout-seconds))

  (define exe-output-json (and exe-output-bytes
                               (call-with-input-bytes exe-output-bytes read-json/safe)))
  (define oracle-output-json (and oracle-output-bytes
                               (call-with-input-bytes oracle-output-bytes read-json/safe)))

  (cond [(equal? oracle-output-json bad-json)
         (log-fest-error "The oracle seems to be confused. Giving up now.")
         #f]
        [(not exe-output-bytes)
         #f]
        [(equal? exe-output-json bad-json)
         (log-fest-error @~a{
                             @(pretty-path exe-path) produced invalid json. The output is below. @;
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
        [(jsexpr=? exe-output-json oracle-output-json)
         (log-fest-error @~a{
                             @(pretty-path exe-path) fails @(pretty-path input-file)
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

(define/contract (test-failures-for exe-path oracle-path peer-tests)
  (path-to-existant-file? test-set/c . -> . test-set/c)

  (define (passes-test? t)
    (exe-passes-test? exe-path oracle-path t))
  (for*/hash ([group (in-list (sort (hash-keys peer-tests) string<?))]
              [tests (in-value (hash-ref peer-tests group))]
              [failed-tests (in-value (filter-not passes-test? tests))]
              #:unless (empty? failed-tests))
    (values group failed-tests)))
