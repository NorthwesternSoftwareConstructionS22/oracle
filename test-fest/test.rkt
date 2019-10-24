#lang at-exp racket

(provide valid-tests
         valid-tests/passing-oracle
         test-failures-for
         log-test-failure-comparison?)

(require json
         "util.rkt"
         "test-fest-data.rkt"
         "logger.rkt")

(define racket-exe (find-executable-path "racket"))

(define/contract (subprocess/racket-bytecode subprocess-args path)
  (list? path-to-existant-file? . -> . any)

  (call-with-extended-environment
   (hash "PLT_COMPILED_FILE_CHECK" "exists"
         "PLTCOMPILEDROOTS" "compiled/@(version):")
   (thunk (apply subprocess (append subprocess-args
                                    (list racket-exe path))))))

;; Values being compared may be large, so they can be suppressed to prevent
;; filling up the log when other `info`-level information is desired
(define log-test-failure-comparison? (make-parameter #t))

(define/contract (exe-passes-test? exe-path
                                   t
                                   #:run-with-racket? [run-with-racket? #f])
  ({path-to-existant-file? test/c}
   {#:run-with-racket? boolean?}
   . ->* .
   boolean?)

  (define-values {exe-dir _3} (basename exe-path #:with-directory? #t))
  (match-define (test input-file output-file timeout-seconds) t)
  (define expected-output
    (call-with-input-file output-file
      read-json/safe
      #:mode 'text))
  (define in-port (open-input-file input-file))
  (define-values {proc stdout _1 _2}
    (parameterize ([current-directory exe-dir])
      (if run-with-racket?
          (subprocess/racket-bytecode (list #f in-port 'stdout)
                                      exe-path)
          (subprocess
           #f in-port 'stdout
           exe-path))))
  (define terminated? (wait/keep-ci-alive proc timeout-seconds))
  (unless terminated?
    (log-fest warning
              @~a{@(pretty-path exe-path) timed out (@|timeout-seconds|s)})
    (subprocess-kill proc #t))
  (define-values {exe-output-str exe-output-json}
    (cond [terminated?
           (define output (port->string stdout))
           (values output (call-with-input-string output read-json/safe))]
          [else (values "<timed out>" bad-json)]))
  (when (eq? exe-output-json bad-json)
    (log-fest warning @~a{@(pretty-path exe-path) produces invalid json!})
    (log-fest warning @~a{Output was: @~v[exe-output-str]}))
  (close-input-port stdout)
  (close-input-port in-port)
  (define pass? (jsexpr=? expected-output exe-output-json))
  (unless pass?
    (log-fest info
              @~a{@(pretty-path exe-path) fails @(pretty-path input-file)})
    (when (log-test-failure-comparison?)
      (log-fest
       info
       @~a{    expected: @~v[expected-output], actual: @~v[exe-output-json]})))
  pass?)

;; Travis CI kills any job that has no output for 10 minutes; prevent that.
(define (wait/keep-ci-alive proc timeout-seconds)
  (define waiting-period
    (min timeout-seconds ci-output-timeout-seconds))
  (define rounds-to-wait
    (round-up (/ timeout-seconds waiting-period)))
  (log-fest debug
            @~a{Waiting for @rounds-to-wait rounds of @|waiting-period|s})
  (for/or ([i (in-range rounds-to-wait)])
    (displayln ".")
    (sync/timeout waiting-period proc)))

(define/contract (valid-tests repo-path
                              assign-number
                              check-validity
                              #:check-json-validity? [check-json-validity? #t]
                              #:test-timeout
                              [test-timeout (const
                                             absolute-max-timeout-seconds)])
  (->* {path-to-existant-directory?
        assign-number?
        (path-to-existant-file? path-to-existant-file? . -> . boolean?)}
       {#:check-json-validity? boolean?
        #:test-timeout (-> natural?)}
       (listof test/c))

  (define (file->test-input path)
    (define path-str (path->string path))
    (and (test-input-file? path-str)
         path-str))
  (define repo-tests-path
    (build-path-string repo-path
                       (assign-number->dir-path assign-number)))
  (cond
    [(directory-exists? repo-tests-path)
     (define valid
       (for*/list ([file-path (in-directory repo-tests-path)]
                   [test-input (in-value (file->test-input file-path))]
                   #:when test-input
                   [test-output (in-value
                                 (test-input-file->output-file test-input))]
                   #:unless
                   (cond [(not (file-exists? test-output))
                          (log-fest info
                                    @~a{Skip @test-input, missing output file.})
                          #t]
                         [(and check-json-validity?
                               (not (valid-json-file? test-input)))
                          (log-fest info
                                    @~a{Skip @test-input, invalid json input.})
                          #t]
                         [(and check-json-validity?
                               (not (valid-json-file? test-output)))
                          (log-fest info
                                    @~a{Skip @test-input, invalid json output.})
                          #t]
                         [(not (check-validity test-input test-output))
                          (log-fest info
                                    @~a{Skip @test-input, fails validity test.})
                          #t]
                         [else #f]))
         (test (simple-form-path-string test-input)
               (simple-form-path-string test-output)
               (test-timeout))))
     (take valid (min max-number-tests (length valid)))]
    [else
     (log-fest
      info
      @~a{Unable to find @(pretty-path repo-path) tests at @repo-tests-path})
     empty]))

(define/contract (valid-tests/passing-oracle repo-path
                                             assign-number
                                             oracle-repo-path
                                             #:check-json-validity?
                                             [check-json-validity? #t])
  (->* {path-to-existant-directory? assign-number? path-to-existant-directory?}
       {#:check-json-validity? boolean?}
       (listof test/c))

  (define oracle-path
    (simple-form-path-string (find-oracle-file oracle-repo-path
                                               assign-number)))

  (define timeout-box (box #f))
  (valid-tests repo-path
               assign-number
               (λ (in out)
                 (define-values {results _2 ms _3}
                   (time-apply
                    (thunk (exe-passes-test? oracle-path
                                             (test in out
                                                   absolute-max-timeout-seconds)
                                             #:run-with-racket? #t))
                    empty))
                 (define student-timeout
                   (oracle->student-timeout (/ ms 1000)))
                 (define seconds (round-up student-timeout))
                 (log-fest debug
                           @~a{Determined timeout: @|seconds|s (oracle: @|ms|ms)})
                 (set-box! timeout-box seconds)
                 (first results))
               #:test-timeout (thunk (unbox timeout-box))))

(define/contract (test-failures-for exe-path peer-tests)
  (path-to-existant-file? test-set/c . -> . test-set/c)

  (define (passes-test? t)
    (exe-passes-test? exe-path t))
  (for*/hash ([(group tests) (in-hash peer-tests)]
              [failed-tests (in-value (filter-not passes-test? tests))]
              #:unless (empty? failed-tests))
    (values group failed-tests)))
