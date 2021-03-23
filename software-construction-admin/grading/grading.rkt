#lang at-exp racket

(provide preserve-files
         grading-repo-remote
         grading-repo-branch
         grading-repo-owner
         grading-repo-name)

(require racket/runtime-path
         racket/pretty
         "../common/cmdline.rkt"
         "../common/assignments.rkt"
         "../common/util.rkt"
         "../common/git.rkt"
         "../common/option.rkt"
         "../common/teams.rkt"
         "../common/env.rkt"
         "../common/logger.rkt"
         "../github-actions/actions.rkt"
         "../tests.rkt"
         "../config.rkt"
         "repo-snapshots.rkt")

(define-runtime-path grading-job-info-cache "grading-jobs.rktd")
(define env-file "env.sh")
(define preserve-files `(".github"
                         ".git"
                         ,env-file))

(define/contract (clean-directory! dir preserve)
  (path-to-existant-directory? (listof string?) . -> . any)

  (for ([f (in-list (directory-list dir))]
        #:unless (matches-any? preserve (path->string f)))
    (define f-path (build-path dir f))
    (displayln @~a{Deleting @f-path})
    (if (file-exists? f-path)
        (delete-file f-path)
        (delete-directory/files f-path))))

(define/contract (copy-team-submission! team
                                        assign-number
                                        to
                                        file-exceptions
                                        #:before-copy [do-before-copy void])
  ({team-name?
    assign-number?
    path-to-existant-directory?
    (listof string?)}
   {#:before-copy (-> any)}
   . ->* .
   any)

  (define snapshot-path
    (team/assign-number->snapshot-path team
                                       assign-number))
  (log-sc-info @~a{Unzipping @team's snapshot at @(pretty-path snapshot-path)})
  (define commit-sha (unpack-snapshot-into! snapshot-path
                                            to
                                            file-exceptions))
  (log-sc-debug @~a{Team's submission sha: @commit-sha}))

(define (extract-score log-text)
  (define matches
    (regexp-match*
     #px"Submitted \\d+ / \\d+ valid tests\\s+Failed (\\d+) / (\\d+) peer tests"
     log-text
     #:match-select cdr))
  (match matches
    [(list _ ... (list failed-test-count total-test-count))
     (present (- 1 (/ (string->number failed-test-count)
                      (string->number total-test-count))))]
    [else
     (failure "No grading results found in log text: something went wrong before grading")]))

(define/contract (kick-off-submission-grading team assign-number grading-repo-path)
  (team-name?
   assign-number?
   path-to-existant-directory?
   . -> .
   (option/c ci-run?))

  (log-sc-info
   @~a{Kicking off a grading job for @team @(assign-number->string assign-number) ...})
  (clean-directory! grading-repo-path preserve-files)

  (log-sc-debug @~a{Copying team submission to repo @(pretty-path grading-repo-path)})
  (copy-team-submission! team
                         assign-number
                         grading-repo-path
                         preserve-files)
  (write-env! grading-repo-path env-file team assign-number "grade")
  (log-sc-debug @~a{Committing and pushing})
  (commit-and-push! grading-repo-path
                    @~a{@team @(assign-number->string assign-number)}
                    #:remote grading-repo-remote
                    #:branch grading-repo-branch
                    #:add grading-repo-path)
  (log-sc-debug @~a{Launching CI run})
  (parameterize ([current-directory grading-repo-path])
    (launch-run! grading-repo-owner
                 grading-repo-name
                 "grading.yml"
                 grading-repo-branch
                 @~a{@team @(assign-number->string assign-number)})))

(define (get-score-from-log job-id)
  (option-let*
   [_ (fail-if (not (equal? (ci-run-status job-id) "completed"))
               @~a{Job @job-id is not done yet.})]
   [log-text (get-run-log! job-id)]
   [_ (display-to-file log-text
                       "last-log.txt"
                       #:exists 'truncate)]
   [grades (extract-score log-text)]
   grades))

(define (count-valid-tests team-name assign-number)
  (define valid-tests-path (assign-number->validated-tests-path assign-number))
  (count (λ (valid-test)
           (equal? (validated-test-input-file->team-name valid-test)
                   team-name))
         (directory-list valid-tests-path)))

;; (present '(217162539 261662772))

(module+ main
  (match-define (cons (hash-table ['team specific-teams]
                                  ['major major-number]
                                  ['minor minor-number]
                                  ['kick-off? kick-off?]
                                  ['extract? extract?]
                                  ['extract-status-only? extract-status-only?])
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
      #:record]))

  (define assign-number (cons major-number minor-number))
  (define teams (match specific-teams
                  ['() (assign-number->active-team-names assign-number)]
                  [other other]))
  (void
   (cond
     [kick-off?
      (define grading-jobs-info
        (for/hash ([team (in-list teams)])
          (values team
                  (kick-off-submission-grading team
                                               assign-number
                                               grading-repo-path))))
      (call-with-output-file grading-job-info-cache
        #:exists 'truncate
        (λ (out)
          (pretty-write (for/hash ([{team job-id} (in-hash grading-jobs-info)]
                                   #:when (or (present? job-id)
                                              (begin0 #f
                                                (log-sc-error
                                                 @~a{
                                                     Failed to launch grading job for @team :
                                                     @job-id
                                                     }))))
                          (values team (ci-run-url (present-v job-id))))
                        out)))]
     [extract?
      (define grading-job-urls-by-team (file->value grading-job-info-cache))
      (define grades
        (option-let*
         [grading-job-ids (get-runs-by-url! grading-repo-owner
                                            grading-repo-name
                                            (hash-values (grading-job-urls-by-team)))]
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
