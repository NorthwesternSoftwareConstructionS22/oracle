#lang at-exp racket

(provide valid-tests
         valid-tests/passing-oracle
         test-failures-for)

(require json
         "util.rkt"
         "test-fest-data.rkt"
         "logger.rkt")

(define/contract (exe-passes-test? exe-path t)
  (path-to-existant-file? test/c . -> . boolean?)

  (match-define (test input-file output-file timeout-minutes) t)
  (define expected-output
    (call-with-input-file output-file
      read-json/safe
      #:mode 'text))
  (define in-port (open-input-file input-file))
  (define-values {proc stdout _1 _2}
    (subprocess #f in-port 'stdout
                exe-path))
  (unless (wait/keep-ci-alive proc timeout-minutes)
    (log-fest warning
              @~a{@(pretty-path exe-path) timeout (@timeout-minutes min)})
    (subprocess-kill proc #t))
  (define exe-output (read-json/safe stdout))
  (when (eq? exe-output bad-json)
    (log-fest warning @~a{@(pretty-path exe-path) produces invalid json!}))
  (close-input-port stdout)
  (close-input-port in-port)
  (log-fest
   debug
   @~a{@(pretty-path exe-path): expect: @~v[expected-output], actual: @~v[exe-output]})
  (jsexpr=? expected-output exe-output))

;; Travis CI kills any job that has no output for 10 minutes; prevent that.
(define (wait/keep-ci-alive proc timeout-minutes)
  (define safe-waiting-period (sub1 ci-output-timeout-minutes))
  (define rounds-to-wait
    (add1 (quotient timeout-minutes safe-waiting-period)))
  (for/or ([i (in-range rounds-to-wait)])
    (match (sync/timeout (* safe-waiting-period 60) proc)
      [#f (displayln ".") #f]
      [success success])))

(define/contract (valid-tests repo-path
                              assign-number
                              check-validity
                              #:check-json-validity? [check-json-validity? #t])
  (->* {path-to-existant-directory?
        assign-number?
        (path-to-existant-file? path-to-existant-file? . -> . boolean?)}
       {#:check-json-validity? boolean?}
       (listof test/c))

  (define (file->test-input path)
    (define path-str (path->string path))
    (and (test-input-file? path-str)
         path-str))
  (define repo-tests-path
    (build-path-string repo-path
                       (car assign-number)
                       (assign-number->string assign-number)))
  (cond
    [(directory-exists? repo-tests-path)
     (for*/list ([file-path (in-directory repo-path)]
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
       (test test-input test-output absolute-max-timeout-minutes))]
    [else
     (log-fest warning
               @~a{Unable to find @repo-path tests at @repo-tests-path})
     empty]))

(define oracle-exe-name
  "main.rkt")
(define/contract (valid-tests/passing-oracle repo-path
                                             assign-number
                                             oracle-repo-path
                                             #:check-json-validity?
                                             [check-json-validity? #t])
  (->* {path-to-existant-directory? assign-number? path-to-existant-directory?}
       {#:check-json-validity? boolean?}
       (listof test/c))

  (define oracle-path
    (build-path-string oracle-repo-path
                       "distribute"
                       (car assign-number)
                       (assign-number->string assign-number)
                       oracle-exe-name))
  (valid-tests repo-path
               assign-number
               (λ (in out)
                 (exe-passes-test? oracle-path
                                   (test in out
                                         absolute-max-timeout-minutes)))))

(define/contract (test-failures-for exe-path peer-tests)
  (path-to-existant-file? test-set/c . -> . test-set/c)

  (define passes-test?
    (match-lambda
      [(test in out oracle-time)
       (exe-passes-test? exe-path
                         (test in
                               out
                               (oracle->student-timeout oracle-time)))]))
  (for*/hash ([(group tests) (in-hash peer-tests)]
              [failed-tests (in-value (filter-not passes-test? tests))]
              #:unless (empty? failed-tests))
    (values group failed-tests)))
