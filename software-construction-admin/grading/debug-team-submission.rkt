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
                                                 ref
                                                 grading-repo-path
                                                 force-test-validation?)
  (team-name?
   assign-number?
   string?
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
                                                (λ (repo) (checkout! repo ref))))
                              (unpack-snapshot-into! snapshot
                                                     grading-repo-path
                                                     grading-repo-preserve-files))))
   #:workflow debug-workflow-name
   #:log-level "debug"
   #:extra-env-vars (if force-test-validation?
                        (list (cons force-validation-env-var "true"))
                        empty)))

(module+ main
  (match-define (cons (hash-table ['team team]
                                  ['major major-number]
                                  ['minor minor-number]
                                  ['ref ref]
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
       "Default: master")
      #:collect {"ref" take-latest #f}
      #:mandatory]
     [("-v" "--validate-tests")
      'force-test-validation?
      "Force test validation regardless of the day and assignment."
      #:record]))

  (log-sc-info @~a{Using snapshot repo: @(pretty-path (current-snapshots-repo-path))})
  (define assign-number (cons major-number minor-number))
  (option-let*
   [job-id (kick-off-submission-debug-job! team
                                           assign-number
                                           ref
                                           grading-repo-path
                                           force-test-validation?)]
   [url (ci-run-html-url job-id)]
   (when (user-prompt! @~a{
                           Job is running at @url
                           Open it in browser?
                           })
     (system @~a{firefox '@url'}))))
