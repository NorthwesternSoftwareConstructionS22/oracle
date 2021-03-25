#lang at-exp racket

(require gregor
         "testing.rkt"
         "tests.rkt"
         "config.rkt"
         "common/cmdline.rkt"
         "common/assignments.rkt"
         "common/util.rkt"
         "common/logger.rkt")

(define (get-pre-validated-tests-by-team assign-number)
  (define all-tests
    (directory->tests (assign-number->validated-tests-path assign-number)))
  (for/hash/fold ([test (in-list all-tests)])
    #:combine cons
    #:default empty
    (values (validated-test-input-file->team-name (test-input-file test))
            test)))

(module+ main
  (match-define (cons (hash-table ['major major]
                                  ['minor minor]
                                  ['team team-name]
                                  ['force-test-validation? force-test-validation?])
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
  (define test-exe-path
    (path->complete-path (assign-number->deliverable-exe-path assign-number)))

  (define oracle-type (assign->oracle-type assign-number))
  (define oracle-path
    (assign-number->oracle-path assign-number
                                #:racket-based-oracle? (equal? oracle-type
                                                               'interacts)))

  (unless (file-exists? test-exe-path)
    (raise-user-error 'software-construction-admin
                      "could not find the `run` executable\n  expected location: ~a"
                      (pretty-path test-exe-path)))

  (define assign-has-student-tests? (member assign-number assigns-with-student-tests))
  (define doing-student-test-validation? (or force-test-validation?
                                             (getenv force-validation-env-var)
                                             (and assign-has-student-tests?
                                                  (is-student-test-validation-time?))))
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
      (log-fest-info @~a{Test validation done.})
      (length valid-tests)))

  (define validated-tests-by-team (get-pre-validated-tests-by-team assign-number))

  (log-fest-info
   @~a{
       Running tests for assignment @(assign-number->string assign-number) on team @team-name's @;
       submission executable @(pretty-path test-exe-path)
       })
  (define failed-peer-tests
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
                                  (>= this-teams-valid-tests expected-valid-test-count)))

  (define total-test-count (test-set-count-tests validated-tests-by-team))
  (define failed-count (test-set-count-tests failed-peer-tests))
  (define failed? (not (zero? failed-count)))
  (displayln
   @~a{


       =======================================================
       Test fest summary for assignment @(assign-number->string assign-number): @(if failed?
                                                                                     "FAIL"
                                                                                     "OK")
       Submitted @this-teams-valid-tests / @(expected-valid-test-count assign-number) valid tests
       Failed @failed-count / @total-test-count peer tests
       =======================================================
       })
  (exit
   (cond [failed?
          (displayln
           @~a{

               Failed tests:
               @(pretty-format
                 (for/hash ([(group tests) (in-hash failed-peer-tests)])
                   (values group
                           (map (λ (t) (basename (test-input-file t)))
                                tests))))
               })
          1]
         [(not enough-valid-tests?) 1]
         [else 0])))
