#lang at-exp racket

(require racket/runtime-path
         "ci-validate-tests.rkt"
         "../common/cmdline.rkt"
         "../common/util.rkt"
         "../common/git.rkt"
         "../common/logger.rkt"
         "../common/assignments.rkt"
         "../common/teams.rkt"
         "../common/option.rkt"
         "../github-actions/actions.rkt"
         "../grading/repo-snapshots.rkt"
         "../tests.rkt"
         "../config.rkt")

(define validation-workflow-name "validate")

(define-runtime-path validation-job-info-cache "test-validation-jobs.rktd")
(define-runtime-path bad-log-dump-path "bad-log.txt")

(define (install-and-push-submitted-tests! assign-number)
  (define submitted-tests-path (assign-number->submitted-tests-path assign-number))
  (check/confirm-dirty-state! oracle-repo-path)
  (make-directory* submitted-tests-path)
  (log-sc-info @~a{Installing submitted tests into @(pretty-path oracle-repo-path) ...})

  (define all-response-box (box #f))
  (define ((install-submitted-test! team) a-test)
    (define to-move (match a-test
                      [(test in #f)
                       (log-sc-info @~a{Skipping test snapshot with no output file: @a-test})
                       empty]
                      [(test in out) (list in out)]))
    (for ([f (in-list to-move)])
      (define new-name (test-file-name->validated (basename f) team))
      (define new-path (build-path submitted-tests-path new-name))
      (log-sc-debug @~a{Moving @(simple-form-path f) to @(simple-form-path new-path)})
      (unless (and (file-exists? new-path)
                   (not (match (or (unbox all-response-box)
                                   (user-prompt!* @~a{
                                                      @new-path already exists. Overwrite it? @;
                                                      (No means skip it.)
                                                      }
                                                  '(y n all skip-all)))
                          ['y #t]
                          ['n #f]
                          ['all (set-box! all-response-box 'y) #t]
                          ['skip-all (set-box! all-response-box 'n) #f])))
        (rename-file-or-directory f new-path #t))))
  (for ([team (in-list (assign-number->active-team-names assign-number))])
    (define snapshot (team/assign-number->snapshot-path team assign-number))
    (log-sc-info @~a{Extracting tests from @team's snapshot at @(pretty-path snapshot)})
    (call-with-temp-directory
     #:name-seed "validate-tests"
     (λ (temp-dir)
       (unpack-snapshot-into! snapshot temp-dir empty)
       (define tests-directory
         (build-path temp-dir
                     (assign-number->deliverables-path assign-number)))
       (define tests
         (if (directory-exists? tests-directory)
             (directory->tests tests-directory)
             empty))
       (for-each (install-submitted-test! team) tests))))
  (log-sc-info @~a{Committing submitted tests in @(pretty-path oracle-repo-path) and pushing})
  (if (user-prompt! @~a{Confirm commit and push to @(pretty-path oracle-repo-path)?})
      (commit-and-push! oracle-repo-path
                        @~a{Add @(assign-number->string assign-number) submitted tests}
                        #:remote oracle-repo-remote
                        #:branch oracle-repo-branch
                        #:add (list submitted-tests-path))
      (displayln "Canceled.")))

(define (setup-and-push-grading-repo-for-test-validation! assign-number)
  (log-sc-info @~a{Setting up the grading repo for validation job.})
  (check/confirm-dirty-state! grading-repo-path)
  (define config-path
    ;; Make sure the config is set up. If it's already there and has the right
    ;; contents, committing this is a no-op.
    ;; That's what we want, since these configs aren't really supposed to change per-commit.
    ;; Hence the need for the indirection with the env file below.
    (install-workflow-config!
     grading-repo-path
     validation-workflow-name
     (list (cons "Validate tests"
                 @~a{
                     racket -O 'debug@"@"sc debug@"@"fest' -W none @;
                     -l software-construction-admin/test-validation/ci-validate-tests -- @;
                     -M $MAJOR @;
                     -m $MINOR
                     }))))
  (define env-path
    (write-workflow-env! grading-repo-path
                         `(("MAJOR" . ,(assign-major-number assign-number))
                           ("MINOR" . ,(assign-minor-number assign-number)))))
  (log-sc-info @~a{
                   @(pretty-path grading-repo-path) set up for test validation.
                   Committing and pushing.
                   })
  (if (user-prompt! @~a{Confirm commit and push to @(pretty-path grading-repo-path)?})
      (commit-and-push! grading-repo-path
                        @~a{@(assign-number->string assign-number) test validation}
                        #:remote grading-repo-remote
                        #:branch grading-repo-branch
                        #:add (list config-path
                                    env-path))
      (displayln "Canceled.")))


(define/contract (extract-valid-test-names job-id)
  (ci-run? . -> . (option/c
                   (listof (and/c string?
                                  has-validated-test-input-file-naming-convention?))))

  (option-let*
   [log-text (get-run-log! job-id)]
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


(define/contract (install-and-push-valid-tests! assign-number valid-test-input-names)
  (assign-number?
   (listof (and/c string? has-validated-test-input-file-naming-convention?))
   . -> .
   any)

  (define submitted-tests-path (assign-number->submitted-tests-path assign-number))
  (define validated-tests-path (assign-number->validated-tests-path assign-number))
  (make-directory* validated-tests-path)
  (log-sc-info @~a{
                   Copying valid tests @;
                   from @(pretty-path submitted-tests-path) to @(pretty-path validated-tests-path)
                   Valid tests are:
                   @(pretty-format valid-test-input-names)
                   })
  (check/confirm-dirty-state! oracle-repo-path)

  (define all-response-box (box #f))
  (for ([valid-test-input (in-list valid-test-input-names)])
    (define src (build-path submitted-tests-path valid-test-input))
    (define dst (build-path validated-tests-path valid-test-input))
    (unless (and (file-exists? dst)
                 (not (match (or (unbox all-response-box)
                                 (user-prompt!* @~a{@dst already exists. Overwrite it?}
                                                '(y n all skip-all)))
                        ['y #t]
                        ['n #f]
                        ['all (set-box! all-response-box 'y) #t]
                        ['skip-all (set-box! all-response-box 'n) #f])))
      (call-with-output-file dst
        #:exists 'replace
        (λ (out)
          (copy-port (call-with-input-file src test-transformer)
                     out)))))

  (log-sc-info @~a{Committing validated tests in @(pretty-path oracle-repo-path) and pushing})
  (if (user-prompt! @~a{Confirm commit and push to @(pretty-path oracle-repo-path)?})
      (commit-and-push! oracle-repo-path
                        @~a{Add @(assign-number->string assign-number) validated tests}
                        #:remote oracle-repo-remote
                        #:branch oracle-repo-branch
                        #:add (list validated-tests-path))
      (displayln "Canceled.")))

(module+ main
  (match-define (cons (hash-table ['major major-number]
                                  ['minor minor-number]
                                  ['kick-off? kick-off?]
                                  ['extract? extract?]
                                  ['snapshot-repo (app current-snapshots-repo-path _)])
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
      #:mandatory-unless (λ (flags) (member 'kick-off? flags))]

     [("-r" "--snapshot-repo")
      'snapshot-repo
      ("Specify a snapshot repo to use."
       @~a{Default: @(simple-form-path test-snapshots-repo-path)})
      #:collect {"path" take-latest test-snapshots-repo-path}]))

  (log-sc-info @~a{Using snapshot repo: @(pretty-path (current-snapshots-repo-path))})
  (define assign-number (cons major-number minor-number))
  (cond [kick-off?
         (install-and-push-submitted-tests! assign-number)
         (setup-and-push-grading-repo-for-test-validation! assign-number)
         (log-sc-info @~a{Launching validation job.})
         (option-let*
          [job-id (launch-run!
                   grading-repo-owner
                   grading-repo-name
                   (~a validation-workflow-name ".yml")
                   grading-repo-branch)]
          [_ (write-to-file (ci-run-url job-id)
                            validation-job-info-cache
                            #:exists 'replace)]
          'ok)]
        [extract?
         (option-let*
          [job-url (file->value validation-job-info-cache)]
          [_ (log-sc-info @~a{Finding job with url @job-url})]
          [runs-by-url (get-runs-by-url! grading-repo-owner
                                         grading-repo-name
                                         (list job-url))]
          [job-id (hash-ref runs-by-url job-url)]
          [_ (log-sc-info @~a{Found the job})]
          [_ (fail-if (not (equal? (ci-run-status job-id) "completed"))
                      @~a{Job is not done yet.})]
          [_ (fail-if (not (equal? (ci-run-conclusion job-id) "success"))
                      @~a{Something went wrong with the validation job, check it out on github.})]
          [_ (log-sc-info @~a{Extracting valid tests from log})]
          [valid-tests (extract-valid-test-names job-id)]
          [_ (install-and-push-valid-tests! assign-number valid-tests)]
          'ok)]))
