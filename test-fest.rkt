#lang at-exp racket

(require racket/cmdline)


(define student-test-repos
  '("https://github.com/NorthwesternSoftwareConstructionFall19/dummy-team-tests.git"))

(define path-to-oracle-exe
  "oracle-dist/bin/main")

(define assign-number (box "0"))
(define test-exe-path (box "./student"))

(command-line
 #:once-each
 [("-a" "--assign")
  assign-number*
  "Assignment number"
  (set-box! assign-number assign-number*)]
 [("-t" "--test-exe")
  student-path*
  "Relative path to executable to test"
  (set-box! test-exe-path student-path*)])

(define test-exe-path/relative (build-path ".." (unbox test-exe-path)))

(define (basename p)
  (define-values {_1 name _2} (split-path p))
  name)

(define failed-tests (box '()))
(define (record-failed-test! repo test)
  (set-box! failed-tests
            (cons (list repo test)
                  (unbox failed-tests))))

(define test-repos-dir "./student-tests")
(delete-directory/files test-repos-dir
                        #:must-exist? #f)
(make-directory test-repos-dir)
(parameterize ([current-directory test-repos-dir])
  (for ([repo (in-list student-test-repos)]
        [i (in-naturals)])
    (define this-student-tests @~a{student-tests-@i})
    (unless (system @~a{git clone "@repo" @this-student-tests})
      (displayln @~a{Failed to clone @repo}))
    (define tests-dir (build-path this-student-tests (unbox assign-number)))
    (cond [(directory-exists? tests-dir)
           (for ([test (in-directory tests-dir)])
             (cond [(not (system @~a{../@|i|/@path-to-oracle-exe @test}))
                    (displayln @~a{Test @test invalid})]
                   [(not (system @~a{@test-exe-path/relative @test}))
                    (displayln @~a{Failed test @test from @repo})
                    (record-failed-test! repo (path->string (basename test)))]
                   [else
                    (displayln @~a{Passed test @test})]))]
          [else
           (displayln @~a{No tests for student @i})])))

(define failed-list (unbox failed-tests))
(define failed? (not (empty? failed-list)))
(displayln
 @~a{


     =======================================================
        Test fest summary for assignment @(unbox assign-number): @(if failed?
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
