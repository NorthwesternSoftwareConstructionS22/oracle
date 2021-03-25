#lang at-exp racket

(provide grading-repo-preserve-files
         kick-off-submission-job!)

(require racket/runtime-path
         racket/pretty
         "../common/cmdline.rkt"
         "../common/assignments.rkt"
         "../common/util.rkt"
         "../common/git.rkt"
         "../common/option.rkt"
         "../common/teams.rkt"
         "../common/logger.rkt"
         "../github-actions/actions.rkt"
         "../tests.rkt"
         "../config.rkt"
         "repo-snapshots.rkt")

(define grading-workflow-name "grade")

(define-runtime-path grading-job-info-cache "grading-jobs.rktd")
(define grading-repo-preserve-files '(".github" ".git"))

(define/contract (kick-off-submission-job! team
                                           assign-number
                                           grading-repo-path
                                           #:type type
                                           #:get-team-submission get-team-submission!
                                           #:workflow workflow-name
                                           #:log-level log-level
                                           #:extra-env-vars extra-env-vars)
  (team-name?
   assign-number?
   path-to-existant-directory?
   #:type string?
   #:get-team-submission (team-name? assign-number? path-to-existant-directory? . -> . any)
   #:workflow string?
   #:log-level string?
   #:extra-env-vars (listof (cons/c string? string?))
   . -> .
   (option/c ci-run?))

  (log-sc-info
   @~a{Kicking off a @type job for @team @(assign-number->string assign-number) ...})
  (clean-directory! grading-repo-path grading-repo-preserve-files)

  (log-sc-debug @~a{Getting team submission into repo @(pretty-path grading-repo-path)})
  (get-team-submission! team assign-number grading-repo-path)
  ;; Make sure the config is set up. If it's already there and has the right
  ;; contents, committing this is a no-op.
  ;; That's what we want, since these configs aren't really supposed to change per-commit.
  ;; Hence the need for the indirection with the env file below.
  (install-workflow-config!
   grading-repo-path
   workflow-name
   (list (cons "Build submission"
               @~a{
                   pushd @(assign-number->deliverables-path assign-number) && @;
                   make && @;
                   popd
                   })
         (cons (~a type " assignment")
               @~a{
                   racket -O @|log-level|@"@"fest @;
                   -l software-construction-admin -- @;
                   -M $MAJOR @;
                   -m $MINOR @;
                   -n $TEAM
                   })))
  (write-workflow-env! grading-repo-path
                       `(("MAJOR" . ,(assign-major-number assign-number))
                         ("MINOR" . ,(assign-minor-number assign-number))
                         ("TEAM" . ,team)
                         .
                         ,extra-env-vars))
  (log-sc-debug @~a{Committing and pushing})
  (commit-and-push! grading-repo-path
                    @~a{@type @team @(assign-number->string assign-number)}
                    #:remote grading-repo-remote
                    #:branch grading-repo-branch
                    ;; Add everything since who knows what files the student code has
                    #:add (list grading-repo-path))
  (log-sc-debug @~a{Launching CI run...})
  (launch-run! grading-repo-owner
               grading-repo-name
               (~a workflow-name ".yml")
               grading-repo-branch))

;; Note: Uses `current-snapshots-repo-path` to find snapshot
(define/contract (kick-off-submission-grading-job! team
                                                   assign-number
                                                   grading-repo-path)
  (team-name?
   assign-number?
   path-to-existant-directory?
   . -> .
   (option/c ci-run?))

  (kick-off-submission-job! team
                            assign-number
                            grading-repo-path
                            #:type "grade"
                            #:get-team-submission unzip-team-snapshot!
                            #:workflow grading-workflow-name
                            #:log-level "error"
                            #:extra-env-vars empty))

(define/contract (unzip-team-snapshot! team assign-number to)
  (team-name?
   assign-number?
   path-to-existant-directory?
   . -> .
   any)

  (define snapshot-path
    (team/assign-number->snapshot-path team
                                       assign-number))
  (log-sc-info @~a{Unzipping @team's snapshot at @(pretty-path snapshot-path)})
  (define commit-sha (unpack-snapshot-into! snapshot-path
                                            to
                                            grading-repo-preserve-files))
  (log-sc-debug @~a{Team's submission sha: @commit-sha}))

(define (get-score-from-log job-id)
  (option-let*
   [_ (fail-if (not (equal? (ci-run-status job-id) "completed"))
               @~a{Job @job-id is not done yet.})]
   [log-text (get-run-log! job-id)]
   [grades (extract-score log-text)]
   grades))

(define (extract-score log-text)
  (define matches
    (regexp-match*
     #px"Submitted \\d+ / \\d+ valid tests.+?Failed (\\d+) / (\\d+) peer tests"
     log-text
     #:match-select cdr))
  (match matches
    [(list _ ... (list failed-test-count total-test-count))
     (present (- 1 (/ (string->number failed-test-count)
                      (string->number total-test-count))))]
    [else
     (failure "No grading results found in log text: something went wrong before grading")]))

(define (count-valid-tests team-name assign-number)
  (define valid-tests-path (assign-number->validated-tests-path assign-number))
  (count (λ (valid-test)
           (equal? (validated-test-input-file->team-name valid-test)
                   team-name))
         (directory-list valid-tests-path)))


(module+ main
  (match-define (cons (hash-table ['team specific-teams]
                                  ['major major-number]
                                  ['minor minor-number]
                                  ['kick-off? kick-off?]
                                  ['extract? extract?]
                                  ['extract-status-only? extract-status-only?]
                                  ['snapshot-repo (app current-snapshots-repo-path _)])
                      args)
    (command-line/declarative
     #:multi
     [("-t" "--team")
      'team
      ("Only grade the specified team(s). Can be provided multiple times."
       "Default: grade all active teams.")
      #:collect {"name" cons empty}]
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
      ("Action: Kick off grading jobs on Travis."
       "Conflicts with -e.")
      #:record
      #:conflicts '(extract? status?)
      #:mandatory-unless (λ (flags) (member 'extract? flags))]
     [("-e" "--extract-grades")
      'extract?
      ("Action: Extract grades from grading jobs on Travis."
       "Conflicts with -k.")
      #:record
      #:conflicts '(kick-off?)
      #:mandatory-unless (λ (flags) (member 'kick-off? flags))]

     [("-s" "--status-only")
      'extract-status-only?
      ("Collect grading job statuses instead of extracting grade info."
       "Only has an effect when -e is specified.")
      #:record]

     [("-r" "--snapshot-repo")
      'snapshot-repo
      ("Specify a snapshot repo to use."
       @~a{Default: @(simple-form-path submission-snapshots-repo-path)})
      #:collect {"path" take-latest submission-snapshots-repo-path}]))

  (log-sc-info @~a{Using snapshot repo: @(pretty-path (current-snapshots-repo-path))})
  (define assign-number (cons major-number minor-number))
  (define teams (match specific-teams
                  ['() (assign-number->active-team-names assign-number)]
                  [other other]))
  (void
   (cond
     [kick-off?
      (define grading-jobs-info
        (for/hash ([team (in-list teams)])
          (define maybe-job-id (kick-off-submission-grading-job! team
                                                                 assign-number
                                                                 grading-repo-path))
          (if (failure? maybe-job-id)
              (log-sc-error
               @~a{
                   Failed to launch grading job for @team :
                   @maybe-job-id
                   })
              (log-sc-info @~a{@team grading job launched.}))
          (values team maybe-job-id)))
      (call-with-output-file grading-job-info-cache
        #:exists 'truncate
        (λ (out)
          (pretty-write (for/hash ([{team job-id} (in-hash grading-jobs-info)]
                                   #:when (present? job-id))
                          (values team (ci-run-url (present-v job-id))))
                        out)))
      'ok]
     [extract?
      (define grading-job-urls-by-team (file->value grading-job-info-cache))
      (define grades
        (option-let*
         [grading-job-ids (get-runs-by-url! grading-repo-owner
                                            grading-repo-name
                                            (hash-values grading-job-urls-by-team))]
         (for/hash ([(team url) (in-hash grading-job-urls-by-team)])
           (log-sc-info @~a{Getting info for @team's job})
           (values team
                   (option-let*
                    [job-id (hash-ref grading-job-ids url)]
                    (if extract-status-only?
                        (ci-run-status job-id)
                        (list (count-valid-tests team assign-number)
                              (match (get-score-from-log job-id)
                                [(present score) score]
                                [failure
                                 ;; keep the (failure ...) wrapper, it's marks the output nicely
                                 failure]))))))))
      (pretty-write grades)])))
