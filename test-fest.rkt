#lang at-exp racket

(require racket/cmdline
         "test-fest-data.rkt"
         "test.rkt"
         "logger.rkt")

(define test-repos-dir "./peer-tests")




(define (basename p)
  (define-values {_1 name _2} (split-path p))
  name)

(define racket (find-executable-path "racket"))
(define oracle-path/relative
  "oracle-dist/main.rkt")

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

(define assign-major-number (unbox assign-major-number-box))
(define assign-number
  @~a{@|(unbox assign-major-number-box)|.@|(unbox assign-minor-number-box)|})

(define failed-tests (box '()))
(define (record-failed-test! repo test)
  (set-box! failed-tests
            (cons (list repo test)
                  (unbox failed-tests))))

(delete-directory/files test-repos-dir
                        #:must-exist? #f)
(make-directory test-repos-dir)
(define path-to-test-exe (unbox test-exe-path))
(parameterize ([current-directory test-repos-dir])
  (define path-to-oracle
    @~a{../@|assign-major-number|/@|assign-number|/@oracle-path/relative})
  (define oracle-tests-dir
    @~a{../@|assign-major-number|/@|assign-number|/tests})
  (define (oracle-passes? test-input test-output)
    (log-fest debug @~a{Checking @test-input against oracle.})
    (check-test-pass path-to-oracle
                     test-input
                     test-output
                     #:run-with-racket? #t))
  (log-fest info "Checking test exe against oracle tests ...")
  (for-each-test-in
   oracle-tests-dir
   (λ (test-input test-output)
     (unless (check-test-pass path-to-test-exe
                              test-input
                              test-output)
       (log-fest warning
        @~a{Your executable failed test @test-input from oracle})
       (record-failed-test! "oracle"
                            (path->string (basename test-input)))))
   #:check-other-validity oracle-passes?)
  (log-fest info "Done.\n")

  (log-fest info "Commencing test fest ...")
  (for ([repo (in-list student-test-repos)]
        [i (in-naturals)])
    (log-fest info @~a{Testing against @repo ...})
    (define this-student-tests @~a{student-tests-@i})
    (unless (clone-repo repo this-student-tests)
      (log-fest warning @~a{Failed to clone @repo}))
    (define tests-dir (build-path this-student-tests
                                  assign-major-number
                                  assign-number))
    (cond [(directory-exists? tests-dir)
           (for-each-test-in
            tests-dir
            (λ (test-input test-output)
              (log-fest info @~a{Failed test @test-input from @repo})
              (record-failed-test! repo
                                   (path->string (basename test-input))))
            #:check-other-validity oracle-passes?)]
          [else
           (log-fest info @~a{No test directory for @repo})]))
  (log-fest info "Done.\n"))

(define failed-list (unbox failed-tests))
(define failed? (not (empty? failed-list)))
(displayln
 @~a{


     =======================================================
        Test fest summary for assignment @|assign-number|: @(if failed?
                                                                "FAIL"
                                                                "OK")
     =======================================================
     })
(exit
 (cond [failed?
        (displayln @~a{
                       Failed tests:
                       @(pretty-format failed-list)
                       })
        1]
       [else 0]))
