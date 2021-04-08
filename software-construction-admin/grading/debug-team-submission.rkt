#lang at-exp racket

(require "../common/cmdline.rkt"
         "../common/util.rkt"
         "../common/logger.rkt"
         "../common/option.rkt"
         "../common/assignments.rkt"
         "../common/git.rkt"
         "../common/teams.rkt"
         "../github-actions/actions.rkt"
         "../config.rkt"
         "repo-snapshots.rkt"
         "grading.rkt")

(define debug-workflow-name "debug")

(define/contract (kick-off-submission-debug-job! team
                                                 assign-number
                                                 configure-repo!
                                                 grading-repo-path
                                                 force-test-validation?)
  (team-name?
   assign-number?
   (path-to-existant-directory? . -> . any)
   path-to-existant-directory?
   boolean?
   . -> .
   (option/c ci-run?))

  (kick-off-submission-job!
   team
   assign-number
   grading-repo-path
   #:type "debug"
   #:get-team-submission (λ _
                           (call-with-temp-directory
                            #:name-seed "debug"
                            (λ (temp-dir)
                              (define snapshot
                                (take-snapshot! team
                                                temp-dir
                                                configure-repo!))
                              (unpack-snapshot-into! snapshot
                                                     grading-repo-path
                                                     grading-repo-preserve-files))))
   #:workflow debug-workflow-name
   #:log-level "debug"
   #:extra-env-vars (if force-test-validation?
                        (list (cons force-validation-env-var "true"))
                        empty)))

(define ((checkout-ref! ref) repo-dir)
  (checkout! repo-dir ref))

(module+ main
  (match-define (cons (hash-table ['team team]
                                  ['major major-number]
                                  ['minor minor-number]

                                  ['ref ref]
                                  ['last-commit-before-deadline commit-deadline-date]

                                  ['force-test-validation? force-test-validation?])
                      args)
    (command-line/declarative
     #:once-each
     [("-t" "--team")
      'team
      ("Debug the specified team's code.")
      #:collect {"name" take-latest #f}
      #:mandatory]
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

     [("-r" "--ref")
      'ref
      ("Debug the specified commit, branch, or tag."
       "Default: the default branch of the repo")
      #:collect {"ref" take-latest ""}]
     [("-d" "--last-commit-before-deadline")
      'last-commit-before-deadline
      ("Debug the last commit before a deadline, given in human-readable format. E.g. 'last friday'"
       "This only makes sense if -r specifies a branch (which it does by default).")
      #:collect {"date" take-latest #f}]


     [("-v" "--validate-tests")
      'force-test-validation?
      "Force test validation regardless of the day and assignment."
      #:record]))

  (log-sc-info @~a{Using snapshot repo: @(pretty-path (current-snapshots-repo-path))})
  (define assign-number (cons major-number minor-number))
  (option-let*
   [job-id (kick-off-submission-debug-job! team
                                           assign-number
                                           (match* {commit-deadline-date ref}
                                             [{#f ref}
                                              (checkout-ref! ref)]
                                             [{date-str branch-name}
                                              (define deadline (parse-human-date->ISO date-str))
                                              (λ (repo)
                                                (checkout! repo branch-name)
                                                ((checkout-last-commit-before deadline) repo))])
                                           grading-repo-path
                                           force-test-validation?)]
   [url (ci-run-html-url job-id)]
   (when (user-prompt! @~a{
                           Job is running at @url
                           Open it in browser?
                           })
     (system @~a{firefox '@url'}))))
