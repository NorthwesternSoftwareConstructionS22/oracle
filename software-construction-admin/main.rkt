#lang at-exp racket

(provide grading-log-delimiter)

(require "testing.rkt"
         "tests.rkt"
         "config.rkt"
         "common/cmdline.rkt"
         "common/assignments.rkt"
         "common/util.rkt"
         "common/logger.rkt"
         "test-validation/test-novelty.rkt")

(define grading-log-delimiter "-----score-----")

(struct fest-summary (assign-number
                      valid-submitted-test-count
                      enough-valid-tests?
                      failed-tests
                      test-count
                      doing-student-test-validation?)
  #:transparent)

(define (build-executable! assign-number)
  (define assign-dir (assign-number->deliverables-path assign-number))
  (log-fest-info @~a{Building executable in @assign-dir using `make`})
  (parameterize ([current-directory assign-dir])
    (system "make")))

(define (get-pre-validated-tests-by-team assign-number)
  (define all-tests
    (directory->tests (assign-number->validated-tests-path assign-number)))
  (for/hash/fold ([test (in-list all-tests)])
    #:combine cons
    #:default empty
    (values (validated-test-input-file->team-name (test-input-file test))
            test)))

(define (assignment-test-fest team-name assign-number force-test-validation?)
  (define oracle-type (assign->oracle-type assign-number))
  (define oracle-path
    (assign-number->oracle-path assign-number
                                #:racket-based-oracle? (equal? oracle-type
                                                               'interacts)))

  (define assign-has-student-tests? (member assign-number assigns-with-student-tests))
  (define doing-student-test-validation?
    (or force-test-validation?
        (getenv force-validation-env-var)
        (and assign-has-student-tests?
             (is-student-test-validation-time? assign-number))))
  (define current-valid-test-count
    (when doing-student-test-validation?
      (log-fest-info
       @~a{
           It is currently before the test deadline, so validating tests in @;
           @(assign-number->deliverables-path assign-number) @;
           before running test fest.
           Additionally, the test fest below will only run on the instructors' tests.
           })
      (define valid-tests
        (valid-tests/passing-oracle
         (assign-number->deliverables-path assign-number)
         oracle-path
         oracle-type
         #:check-json-validity? all-valid-tests-must-be-json?
         #:require-output-file? (and (member assign-number
                                             assigns-requiring-test-outputs)
                                     #t)))
      (define novel-tests (filter-already-submitted-tests valid-tests assign-number))
      (log-fest-info @~a{
                         Test validation done: @;
                         @(length valid-tests) tests pass basic validity checking, @;
                         and @(length novel-tests) of those are novel.
                         Moving on to testing.
                         })
      (length valid-tests)))

  (define validated-tests-by-team (get-pre-validated-tests-by-team assign-number))

  (build-executable! assign-number)
  (define test-exe-path
    (path->complete-path (assign-number->deliverable-exe-path assign-number)))
  (unless (file-exists? test-exe-path)
    (raise-user-error 'software-construction-admin
                      "could not find the `run` executable\n  expected location: ~a"
                      (pretty-path test-exe-path)))

  (log-fest-info
   @~a{
       Running tests for assignment @(assign-number->string assign-number) on team @team-name's @;
       submission executable @(pretty-path test-exe-path)
       })
  (define failed-tests
    (test-failures-for test-exe-path
                       oracle-path
                       validated-tests-by-team
                       #:racket-based-oracle? (equal? oracle-type
                                                      'interacts)
                       #:oracle-needs-student-output? (equal? oracle-type 'checks-output)))
  (log-fest-info @~a{Done running tests.})


  (define this-teams-valid-tests
    (if doing-student-test-validation?
        current-valid-test-count
        (length (hash-ref validated-tests-by-team
                          team-name
                          empty))))
  (define enough-valid-tests? (or (not assign-has-student-tests?)
                                  (>= this-teams-valid-tests
                                      (expected-valid-test-count assign-number))))

  (define total-test-count (test-set-count-tests validated-tests-by-team))
  (fest-summary assign-number
                this-teams-valid-tests
                enough-valid-tests?
                failed-tests
                total-test-count
                doing-student-test-validation?))

(define (render-fest-summary! summary)
  (match-define (fest-summary assign-number
                              this-teams-valid-tests
                              enough-valid-tests?
                              failed-tests
                              total-test-count
                              doing-student-test-validation?)
    summary)
  (define failed-count (test-set-count-tests failed-tests))
  (define failed? (not (zero? failed-count)))
  (displayln
   @~a{


       =======================================================
       Test fest summary for assignment @(assign-number->string assign-number): @(if failed?
                                                                                     "FAIL"
                                                                                     "OK")
       @(if doing-student-test-validation?
            @~a{Submitted @this-teams-valid-tests / @(expected-valid-test-count assign-number) valid tests}
            "")
       Failed @failed-count / @total-test-count peer tests
       =======================================================

       })
  (when failed?
    (displayln
     @~a{

         Failed tests:
         @(pretty-format
           (for/hash ([(group tests) (in-hash failed-tests)])
             (values group
                     (map (λ (t) (basename (test-input-file t)))
                          tests))))
         })))

(define (fest-summary->exit-code summary)
  (match-define (fest-summary assign-number
                              this-teams-valid-tests
                              enough-valid-tests?
                              failed-tests
                              total-test-count
                              doing-student-test-validation?)
    summary)
  (cond
    [(> (test-set-count-tests failed-tests) 0) 1]
    [(not doing-student-test-validation?) 0]
    [(not enough-valid-tests?) 1]
    [else 0]))

(module+ main
  (match-define (cons (hash-table ['major major]
                                  ['minor minor]
                                  ['team team-name]
                                  ['force-test-validation? force-test-validation?]
                                  ['grading-mode? grading-mode?])
                      args)
    (command-line/declarative
     #:once-each
     [("-M" "--Major")
      'major
      "Assignment major number. E.g. for 5.2 this is 5."
      #:collect {"number" take-latest #f}
      #:mandatory]
     [("-m" "--minor")
      'minor
      "Assignment minor number. E.g. for 5.2 this is 2."
      #:collect {"number" take-latest #f}
      #:mandatory]
     [("-n" "--team-name")
      'team
      "Team name to report test validity results for."
      #:collect {"path" take-latest #f}
      #:mandatory]

     [("-v" "--validate-tests")
      'force-test-validation?
      ("Force test validation regardless of the day and assignment."
       @~a{This can also be done by setting the evironment variable @force-validation-env-var})
      #:record]
     [("-g" "--grading-mode")
      'grading-mode?
      ("Regrade all assignments up to and including the one specified with -M, -m."
       "Also produce a parse-able summary of results instead of a human-readable one."
       "This flag overrides -v (forces test validation *not* to happen).")
      #:record]))

  (define assign-number (cons major minor))
  (unless (or (member assign-number assign-sequence)
              (equal? team-name "f19-dummy-team"))
    (raise-user-error 'software-construction-admin
                      @~a{
                          expected an assigned assignment number, @;
                          i.e. one of @(map assign-number->string assign-sequence)
                            got: @(assign-number->string assign-number)
                          }))
  (cond [grading-mode?
         (define results
           (for/hash ([assign-number (in-list (assign-numbers-up-to assign-number))])
             (define summary (assignment-test-fest team-name assign-number #f))
             (define failed-test-count
               (apply + (map length (hash-values (fest-summary-failed-tests summary)))))
             (values assign-number
                     (list (fest-summary-valid-submitted-test-count summary)
                           failed-test-count
                           (fest-summary-test-count summary)))))
         (display grading-log-delimiter)
         (write results)
         (displayln grading-log-delimiter)]
        [else
         (define summary (assignment-test-fest team-name assign-number force-test-validation?))
         (render-fest-summary! summary)
         (exit (fest-summary->exit-code summary))]))
