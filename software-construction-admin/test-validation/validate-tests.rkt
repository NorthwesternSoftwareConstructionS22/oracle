#lang at-exp racket

(require racket/runtime-path
         "ci-validate-tests.rkt"
         "../common/cmdline.rkt"
         "../common/util.rkt"
         "../common/git.rkt"
         "../common/logger.rkt"
         "../travis/env.rkt"
         "../grading/repo-snapshots.rkt"
         "../travis/travis.rkt"
         "../common/assignments.rkt"
         "../common/teams.rkt"
         "../common/option.rkt"
         "../tests.rkt"
         "../grading/grading.rkt"
         "../config.rkt")

(define env-file "env.sh")

(define-runtime-path validation-job-info-cache "test-validation-jobs.rktd")
(define-runtime-path bad-log-dump-path "bad-log.txt")

(define/contract (kick-off-test-validation-job! assign-number)
  (assign-number? . -> . (option/c travis:job-id/c))

  (install-and-push-submitted-tests! assign-number)
  (setup-and-push-grading-repo-for-test-validation! assign-number)
  (travis:trigger-build! grading-repo-branch
                         grading-repo-owner
                         grading-repo-name
                         grading-repo-branch
                         @~a{@(assign-number->string assign-number) test validation}))

(define (install-and-push-submitted-tests! assign-number)
  (define submitted-tests-path (assign-number->submitted-tests-path assign-number))
  (check/confirm-dirty-state! oracle-repo-path)
  (log-fest-info @~a{Installing submitted tests into @(pretty-path oracle-repo-path) ...})
  (for ([team (in-list (assign-number->active-team-names assign-number))])
    (define snapshot (team/assign-number->snapshot-path team assign-number))
    (log-fest-info @~a{Extracting tests from @team's snapshot at @(pretty-path snapshot)})
    (call-with-temp-directory
     (λ (temp-dir)
       (unpack-snapshot-into! snapshot "." empty)
       (define tests (directory->tests (assign-number->deliverables-path assign-number)))
       (for-each (install-submitted-test! team submitted-tests-path) tests))))
  (log-fest-info @~a{Committing submitted tests in @(pretty-path oracle-repo-path) and pushing})
  (commit-and-push! oracle-repo-path
                    @~a{Add @(assign-number->string assign-number) submitted tests}
                    #:remote oracle-repo-remote
                    #:branch oracle-repo-branch
                    #:add (list submitted-tests-path)))

(define ((install-submitted-test! team destination) a-test)
  (define to-move (match a-test
                    [(test in #f)
                     (log-sc-info @~a{Skipping test snapshot with no output file: @a-test})
                     empty]
                    [(test in out) (list in out)]))
  (for ([f (in-list to-move)])
    (define new-name (test-file-name->validated (basename f)))
    (define new-path (build-path destination new-name))
    (unless (and (file-exists? new-path)
                 (not (user-prompt!
                       @~a{@new-path already exists. Overwrite it? (No means skip it.)})))
      (rename-file-or-directory f new-path))))

(define (setup-and-push-grading-repo-for-test-validation! assign-number)
  (check/confirm-dirty-state! grading-repo-path)
  (write-env! grading-repo-path
              env-file
              "test-validation"
              assign-number
              "test-validation")
  (log-fest-info @~a{
                     @(pretty-path grading-repo-path) set up for test validation.
                     Committing and pushing.
                     })
  (commit-and-push! grading-repo-path
                    @~a{@(assign-number->string assign-number) test validation}
                    #:remote grading-repo-remote
                    #:branch grading-repo-branch
                    #:add (list env-file)))


(define/contract (extract-valid-test-names job-id)
  (travis:job-id/c . -> . (option/c
                           (listof (and/c string?
                                          has-validated-test-input-file-naming-convention?))))

  (option-let*
   [log-text (travis:get-log! job-id)]
   [names (parse-validated-inputs log-text)]
   names))

(define (parse-validated-inputs log-output-str)
  (match (regexp-match @~a{@validated-test-log-delimeter(.+?)@validated-test-log-delimeter}
                       log-output-str)
    [(list _ validated-test-list-str)
     (present (call-with-input-string validated-test-list-str read))]
    [else
     (display-to-file log-output-str
                      bad-log-dump-path
                      #:exists 'replace)
     (failure @~a{
                  Validated test list not found in log output, so aborting.
                  A copy of the log has been dumped to @(pretty-path bad-log-dump-path)
                  })]))

(define (install-valid-tests! assign-number valid-test-input-names)
  (assign-number?
   (listof (and/c string? has-validated-test-input-file-naming-convention?))
   . -> .
   any)

  (log-fest-info @~a{
                     Copying valid tests @;
                     from @(pretty-path submitted-tests-path) to @(pretty-path validated-tests-path)
                     Valid tests are:
                     @(pretty-format valid-test-input-names)
                     })
  (define submitted-tests-path (assign-number->submitted-tests-path assign-number))
  (define validated-tests-path (assign-number->validated-tests-path assign-number))
  (check/confirm-dirty-state! oracle-repo-path)

  (for ([valid-test-input (in-list valid-test-input-names)])
    (define src (build-path submitted-tests-path valid-test-input))
    (define dst (build-path validated-tests-path valid-test-input))
    (unless (and (file-exists? dst)
                 (not (user-prompt! @~a{@dst already exists. Overwrite it? (No means skip it.)})))
      (copy-file src dst #t))))

(module+ main
  (match-define (cons (hash-table ['major major-number]
                                  ['minor minor-number]
                                  ['kick-off? kick-off?]
                                  ['extract? extract?])
                      args)
    (command-line/declarative
     #:once-each
     [("-M" "--Major")
      'major
      "Assignment major number. E.g. for 5.2 this is 5."
      #:collect {"N" take-latest #f}
      #:mandatory]
     [("-m" "--minor")
      'minor
      "Assignment minor number. E.g. for 5.2 this is 2."
      #:collect {"N" take-latest #f}
      #:mandatory]
     [("-k" "--kick-off")
      'kick-off?
      ("Action: Kick off test validation job on Travis."
       "Conflicts with -e.")
      #:record
      #:conflicts '(extract?)
      #:mandatory-unless (λ (flags) (member 'extract? flags))]
     [("-e" "--extract-valid-tests")
      'extract?
      ("Action: Extract valid tests from job on Travis"
       "and install them in the validated tests directory."
       "Conflicts with -k.")
      #:record
      #:conflicts '(kick-off?)
      #:mandatory-unless (λ (flags) (member 'kick-off? flags))]))

  (current-snapshots-repo-path test-snapshots-repo-path)
  (define assign-number (cons major-number minor-number))
  (cond [kick-off?
         (option-let*
          [job-id (kick-off-test-validation-job! assign-number)]
          [_ (write-to-file job-id
                            validation-job-info-cache
                            #:exists? 'replace)]
          'ok)]
        [extract?
         (option-let*
          [job-id (file->value validation-job-info-cache)]
          [valid-tests (extract-valid-test-names job-id)]
          [_ (install-valid-tests! assign-number valid-tests)]
          'ok)]))
