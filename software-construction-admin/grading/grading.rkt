#lang at-exp racket

(require racket/runtime-path
         racket/pretty
         "../common/cmdline.rkt"
         "../common/assignments.rkt"
         "../common/util.rkt"
         "../common/tar.rkt"
         "../common/git.rkt"
         "../common/option.rkt"
         "../common/teams.rkt"
         "../travis/env.rkt"
         "../travis/travis-api.rkt"
         "repo-snapshots.rkt")

(define-runtime-path snapshot-repo-path "../../../snapshots")
(define-runtime-path grading-repo-path "../../../grading")
(define-runtime-path grading-job-info-cache "jobs.rktd")
(define env-file "env.sh")
(define preserve-files `(".travis.yml"
                         ".git"
                         ,env-file))
(define grading-repo-remote "origin")
(define grading-repo-branch "master")
(define github-user "llazarek")
(define grading-repo-name "nu-sc-f19-grading")


(define build-other-failure "Build went wrong! Giving up.~n")
(define ping-timer-seconds (make-parameter (* 2 60)))

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
  (displayln @~a{Unzipping @(pretty-path snapshot-path)})
  (define commit-sha (unpack-snapshot-into! snapshot-path
                                            to
                                            file-exceptions))
  (displayln commit-sha))

#;(define (travis:trigger-build/wait-for-results! debug-dir
                                                team
                                                assign-number)
  (parameterize ([current-directory debug-dir])
    ;; (define commit-hash
    ;;   (system/string "git rev-parse --short HEAD"))
    ;; (displayln @~a{Trying to do hash: @commit-hash})
    (option-let*
     [job-id (travis:trigger-build! "master"
                                    team
                                    assign-number)]
     [log-text (let loop ()
                 (sleep (ping-timer-seconds))
                 (match (travis:get-log! job-id
                                         #:must-be-complete? #t)
                   [(failure (== build-other-failure))
                    (failure build-other-failure)]
                   [(failure other-msg)
                    (displayln other-msg)
                    (loop)]
                   [(present v)
                    (present v)]))]
     log-text)))

(define (extract-grade-info log-text)
  (define matches
    (regexp-match*
     #px"Submitted (\\d+) / \\d+ valid tests\\s+Failed (\\d+) / (\\d+) peer tests"
     log-text
     #:match-select cdr))
  (match matches
    [(list _ ... (list valid-test-count failed-test-count total-test-count))
     (present (list (string->number valid-test-count)
                 (- 1 (/ (string->number failed-test-count)
                         (string->number total-test-count)))))]
    [else
     #:when (regexp-match? #px"makefile failed to build an executable"
                           log-text)
     (failure "No run exe")]
    [else
     #:when (regexp-match? #px"Unable to locate deliverable folder at"
                           log-text)
     (failure "No submission directory")]
    [else
     (failure "No grade info found!")]))

(define/contract (kick-off-submission-grading team assign-number grading-repo-path)
  (team-name?
   assign-number?
   path-to-existant-directory?
   . -> .
   (option/c travis:job-id/c))

  (clean-directory! grading-repo-path preserve-files)
  (copy-team-submission! team
                         assign-number
                         grading-repo-path
                         preserve-files)
  (write-env! grading-repo-path env-file team assign-number)
  (commit-and-push! grading-repo-path
                    @~a{[skip travis] @team @(assign-number->string assign-number)}
                    #:remote grading-repo-remote
                    #:branch grading-repo-branch
                    #:add ".")
  (parameterize ([current-directory grading-repo-path])
    (travis:trigger-build! grading-repo-branch
                           github-user
                           grading-repo-name
                           grading-repo-branch
                           @~a{@team @(assign-number->string assign-number)})))

(define (get-grading-results grading-job-info)
  (option-let*
   [log-text (travis:get-log! grading-job-info
                              #:must-be-completed? #t)]
   [_ (display-to-file log-text
                       "last-log.txt"
                       #:exists 'truncate)]
   [grades (extract-grade-info log-text)]
   grades))

(define (add-test-count-if-failure team-name assign-number results)
  (match results
    [(? present?) results]
    [(failure msg)
     (define valid-tests-path (assign-number->validated-tests-path assign-number))
     (define valid-tests
       (if (directory-exists? valid-tests-path)
           (directory-list valid-tests-path)
           '()))
     (define valid-test-count
       (/ (length valid-tests) 2))
     (failure @~a{@valid-test-count 0 #| @msg |#})]))

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
      "Only grade the specified team(s). Can be provided multiple times."
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
      #:conflicts '(extract?)
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
        (λ (out) (pretty-write grading-jobs-info out)))]
     [extract?
      (define get-the-grade-results (if extract-status-only?
                                        travis:get-job-status!
                                        get-grading-results))
      (define grading-jobs-info
        (call-with-input-file grading-job-info-cache read))
      (define grades
        (for/hash ([(team info) (in-hash grading-jobs-info)])
          (define results (get-the-grade-results info))
          (define results+tests-for-failures
            (add-test-count-if-failure team assign-number results))
          (values team
                  results+tests-for-failures)))
      (displayln (~v grades))])))
