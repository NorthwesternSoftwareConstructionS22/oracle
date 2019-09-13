#lang at-exp racket

(require racket/cmdline)


(define student-test-repos
  '("https://github.com/NorthwesternSoftwareConstructionFall19/dummy-team-tests.git"))

(define assign-number "0")
(define student-path "./student")

(command-line
 #:once-each
 [("-a" "--assign")
  assign-number*
  "Assignment number"
  (set! assign-number assign-number*)]
 [("-s" "--student")
  student-path*
  "Path to student exe"
  (set! student-path student-path*)])

(define student-path/rel (build-path ".." student-path))

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
    (define tests-dir (build-path this-student-tests assign-number))
    (cond [(directory-exists? tests-dir)
           (for ([test (in-directory tests-dir)])
             (cond [(not (system @~a{../@|i|/main @test}))
                    (displayln @~a{Test @test invalid})]
                   [(not (system @~a{@student-path/rel @test}))
                    (displayln @~a{Failed test @test})]
                   [else
                    (displayln @~a{Passed test @test})]))]
          [else
           (displayln @~a{No tests for student @i})])))
