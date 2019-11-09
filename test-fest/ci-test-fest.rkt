#lang at-exp racket

(require racket/cmdline
         racket/runtime-path
         "test-fest-data.rkt"
         "util.rkt"
         "git.rkt"
         "test.rkt"
         "logger.rkt"
         "test-cache.rkt")

(define test-repos-dir "./peer-tests")

(define racket (find-executable-path "racket"))

(define-runtime-path oracle-repo "..")

(define (clone+validate-tests-from-repos! assign-number)
  (log-fest info @~a{Cloning test repos into @test-repos-dir ...})
  (define student-test-repos/active
    (map group->test-repo-name
         (assign->active-groups assign-number)))
  (define test-repo-paths (clone-repos-into! test-repos-dir
                                             student-test-repos/active))
  (define test-repo-paths+oracle
    (hash-set test-repo-paths
              "oracle"
              (build-path-string oracle-repo "tests")))
  (log-fest info @~a{Done. Checking test validitity ...})
  (for/hash ([(repo-name repo-path) (in-hash test-repo-paths+oracle)])
    (values repo-name
            (valid-tests/passing-oracle repo-path
                                        assign-number
                                        oracle-repo
                                        #:check-json-validity? #t))))

(define (clone+parse-pre-validated-tests! assign-number)
  (log-fest info @~a{Using pre-validated tests})
  (define valid-tests-repo-path (clone-repo! "valid-tests"))
  (unless valid-tests-repo-path
    (eprintf "Error: unable to clone pre-validated tests! Aborting~n")
    (exit 1))
  (define valid-tests-info-path
    (find-cached-test-info-path valid-tests-repo-path
                                assign-number))
  (define valid-tests
    (deserialize-tests valid-tests-repo-path
                       (call-with-input-file valid-tests-info-path
                         read)))
  (define/match (test->team-repo-name t)
    [{(test (regexp #rx"oracle/./.../input[0-9]+$") _ _)}
     "oracle"]
    [{(test (regexp #rx"(team[^-]+)-tests/./.../input[0-9]+$"
                    (list _ name))
            _
            _)}
     @~a{@|name|-tests}])
  (define tests+names (map (λ (t) (list (test->team-repo-name t) t))
                           valid-tests))
  (for/hash ([group (in-list (group-by first tests+names))])
    (values (first (first group))
            (map second group))))

(module+ main
  (define assign-major-number-box (box "0"))
  (define assign-minor-number-box (box "0"))
  (define test-exe-path (box "echo"))
  (define team-box (box "dummy-team"))

  (command-line
   #:once-each
   [("-M" "--Major")
    assign-number*
    "Assignment major number. E.g. for 5.2 this is 5."
    (set-box! assign-major-number-box assign-number*)]
   [("-m" "--minor")
    assign-number*
    "Assignment minor number. E.g. for 5.2 this is 2."
    (set-box! assign-minor-number-box assign-number*)]
   [("-t" "--test-exe")
    student-path*
    "Absolute path to executable to test."
    (set-box! test-exe-path student-path*)]
   [("-n" "--team-name")
    team
    "Team name to report test validity results for."
    (set-box! team-box team)])

  (define assign-number (cons (unbox assign-major-number-box)
                              (unbox assign-minor-number-box)))

  (delete-directory/files test-repos-dir
                          #:must-exist? #f)
  (make-directory test-repos-dir)
  (define get-valid-tests
    (if (use-pre-validated-tests?)
        clone+parse-pre-validated-tests!
        clone+validate-tests-from-repos!))
  (define valid-peer-tests (get-valid-tests assign-number))
  (define path-to-test-exe (unbox test-exe-path))
  (log-fest
   info
   @~a{Done. Running @(pretty-path path-to-test-exe) against valid tests ...})
  (define failed-peer-tests
    (test-failures-for path-to-test-exe
                       valid-peer-tests))
  (log-fest info @~a{Done.})


  (define valid-tests-by-team
    (length (hash-ref valid-peer-tests
                      (group->test-repo-name (unbox team-box))
                      empty)))
  (define enough-valid-tests? (>= valid-tests-by-team 5))

  (define total-test-count (test-set-count-tests valid-peer-tests))
  (define failed-count (test-set-count-tests failed-peer-tests))
  (define failed? (not (zero? failed-count)))
  (displayln
   @~a{


       =======================================================
       Test fest summary for assignment @|assign-number|: @(if failed?
                                                               "FAIL"
                                                               "OK")
       Submitted @valid-tests-by-team / 5 valid tests
       Failed @failed-count / @total-test-count peer tests
       =======================================================
       })
  (exit
   (cond [failed?
          (displayln @~a{
                         Failed tests:
                         @(pretty-format
                           (for/hash ([(group tests) (in-hash failed-peer-tests)])
                             (values group
                                     (map (λ (t) (pretty-path (test-in t)))
                                          tests))))
                         })
          1]
         [(not enough-valid-tests?) 1]
         [else 0])))
