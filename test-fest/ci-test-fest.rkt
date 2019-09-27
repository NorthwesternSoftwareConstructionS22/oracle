#lang at-exp racket

(require racket/cmdline
         racket/runtime-path
         "test-fest-data.rkt"
         "util.rkt"
         "git.rkt"
         "test.rkt"
         "logger.rkt")

(define test-repos-dir "./peer-tests")

(define racket (find-executable-path "racket"))

(define-runtime-path oracle-repo "..")

(define assign-major-number-box (box "0"))
(define assign-minor-number-box (box "0"))
(define test-exe-path (box "echo"))

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
  (set-box! test-exe-path student-path*)])

(define assign-number (cons (unbox assign-major-number-box)
                            (unbox assign-minor-number-box)))

(delete-directory/files test-repos-dir
                        #:must-exist? #f)
(make-directory test-repos-dir)
(log-fest info @~a{Cloning test repos into @test-repos-dir ...})
(define test-repo-paths (clone-repos-into! test-repos-dir
                                           student-test-repos))
(define test-repo-paths+oracle
  (hash-set test-repo-paths
            "oracle"
            (build-path-string oracle-repo "tests")))
(log-fest info @~a{Done. Checking test validitity ...})
(define valid-peer-tests
  (for/hash ([(repo-name repo-path) (in-hash test-repo-paths+oracle)])
    (values repo-name
            (valid-tests/passing-oracle repo-path
                                        assign-number
                                        oracle-repo
                                        #:check-json-validity? #t))))
(define path-to-test-exe (unbox test-exe-path))
(log-fest
 info
 @~a{Done. Running @(pretty-path path-to-test-exe) against valid tests ...})
(define failed-peer-tests
  (test-failures-for path-to-test-exe
                     valid-peer-tests))
(log-fest info @~a{Done.})

(define total-test-count (test-set-count-tests valid-peer-tests))
(define failed-count (test-set-count-tests failed-peer-tests))
(define failed? (not (zero? failed-count)))
(displayln
 @~a{


     =======================================================
        Test fest summary for assignment @|assign-number|: @(if failed?
                                                                "FAIL"
                                                                "OK")
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
       [else 0]))
