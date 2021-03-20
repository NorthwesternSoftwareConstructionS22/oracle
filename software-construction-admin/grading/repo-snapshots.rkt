#lang at-exp racket


(provide team/assign-number->snapshot-path
         unpack-snapshot-into!
         current-snapshots-repo-path)

(require racket/runtime-path
         "../common/cmdline.rkt"
         "../common/util.rkt"
         "../common/git.rkt"
         "../common/tar.rkt"
         "../common/logger.rkt"
         "../common/assignments.rkt"
         "../common/teams.rkt"
         "../common/team-repos.rkt"
         "../config.rkt")

(define current-snapshots-repo-path (make-parameter default-snapshots-repo-path))

(define/contract (assign-number->snapshots-dir assign-number)
  (assign-number? . -> . path-string?)

  (build-path (current-snapshots-repo-path)
              (assign-number->dir-path-part assign-number)))

(define/contract (team/assign-number->snapshot-path team assign-number)
  (team-name? assign-number? . -> . path-string?)

  (build-path (assign-number->snapshots-dir assign-number)
              (~a team ".zip")))

(define/contract (unpack-snapshot-into! snapshot-path
                                        destination
                                        preserve-files)
  (path-to-existant-file?
   path-to-existant-directory?
   (listof string?)
   . -> .
   sha?)

  (call-with-temp-directory
   (λ (snapshot-unpack-path)
     (define snapshot-name (basename snapshot-path))
     (log-sc-info @~a{Unpacking @(pretty-path snapshot-path) into @snapshot-unpack-path})
     (define snapshot-copy-path (build-path snapshot-unpack-path snapshot-name))
     (copy-file snapshot-path snapshot-copy-path)

     (define repo-snapshot-path (unzip-in-place! snapshot-copy-path))

     (define snapshot-commit-sha (get-head-commit-sha repo-snapshot-path))

     (for ([f (in-list (directory-list repo-snapshot-path))]
           #:unless (matches-any? preserve-files (path->string f)))
       (define f-path (build-path repo-snapshot-path f))
       (log-sc-info @~a{Copying @f-path to @destination})
       (rename-file-or-directory f-path (build-path destination f)))

     snapshot-commit-sha)))

(define/contract (take-snapshots! teams destination-dir)
  ((listof team-name?) path-to-existant-directory? . -> . (listof path-to-existant-file?))

  (call-with-temp-directory
   (λ (temp-dir)
     (parameterize ([current-directory temp-dir])
       (for/list ([team (in-list teams)])
         (define repo (clone-repo! (team->dev-repo-name team)))
         (define snapshot (zip! repo))
         (define destination (build-path destination-dir
                                         (basename snapshot)))
         (if (file-exists? destination)
             (match (user-prompt!* @~a{
                                       @(pretty-path destination) already exists. @;
                                       Delete it, skip, or abort?
                                       }
                                   '(d s a))
               ['d
                (delete-file destination)
                (rename-file-or-directory snapshot destination)]
               ['s (void)]
               [else (raise-user-error 'take-snapshots!
                                       "Aborting due to already existant snapshot.")])
             (rename-file-or-directory snapshot destination))
         destination)))))

(define/contract (take-snapshot! team destination-dir)
  (team-name? path-to-existant-directory? . -> . path-to-existant-file?)

  (first (take-snapshots! (list team) destination-dir)))

(define env-file "env.sh")

(define (parse-human-date->ISO str)
  (match (with-output-to-string
           (thunk (system* (find-executable-path "date")
                           "+%Y-%m-%d %H:%M:%S"
                           "-d"
                           str)))
    ["" (raise-user-error 'repo-snapshots "bad date")]
    [other other]))

(module+ main
  (match-define (cons (hash-table ['major major-number]
                                  ['minor minor-number]
                                  ['deadline deadline-string]
                                  ['deadline-hr human-deadline-string]
                                  ['alternative-snapshot-repo _]
                                  ['team specific-teams])
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
     [("-D" "--deadline")
      'deadline
      ("Deadline for assignment in format \"YYYY-MM-DD HH:MM:SS\""
       "Either this or -d must be specified.")
      #:collect {"N" take-latest #f}
      #:mandatory-unless (λ (flags) (member 'deadline-hr flags))]
     [("-d" "--human-readable-deadline")
      'deadline-hr
      ("Deadline for assignment in human-readable format. E.g. 'last friday'"
       "Either this or -D must be specified.")
      #:collect {"date" take-latest #f}
      #:mandatory-unless (λ (flags) (member 'deadline flags))]
     [("-r" "--snapshot-repo-path")
      'alternative-snapshot-repo
      ("Specify an alternative snapshot repo in which to place snapshots."
       @~a{Default: @(pretty-path (current-snapshots-repo-path))})
      #:collect {"path" (set-parameter current-snapshots-repo-path) #f}]
     #:multi
     [("-t" "--team")
      'team
      ("Only snapshot the specified team(s). Can be provided multiple times."
       "Default: snapshot all active teams.")
      #:collect {"name" cons empty}]))

  (define assign-number (cons major-number minor-number))
  (define deadline
    (match* {deadline-string human-deadline-string}
      [{(regexp @pregexp{(\d{4}-\d{2}-\d{2}) (\d{2}:\d{2}:\d{2})} (list _ day time))
        #f}
       (~a day "T" time)]
      [{#f (? string? human-date)}
       (parse-human-date->ISO human-date)]))
  (define teams (match specific-teams
                  ['()
                   (log-sc-info
                    @~a{Snapshotting all active teams for @(assign-number->string assign-number)})
                   (assign-number->active-team-names assign-number)]
                  [other
                   (log-sc-info @~a{Snapshotting only teams @~v[other]})
                   other]))

  (define snapshot-dir (assign-number->snapshots-dir assign-number))
  (make-directory* snapshot-dir)
  (log-sc-info @~a{Taking dev repo snapshots in @snapshot-dir ...})
  (void (take-snapshots! teams snapshot-dir))
  (log-sc-info @~a{Done.})
  (cond [(user-prompt! @~a{Commit snapshots in @(pretty-path (current-snapshots-repo-path))?})
         (add! (current-snapshots-repo-path) snapshot-dir)
         (commit! (current-snapshots-repo-path)
                  @~a{
                      Snapshot: @(assign-number->string assign-number) @;
                      @deadline @;
                      @(if (empty? specific-teams)
                           "all teams"
                           "some teams")

                      teams:
                      @(pretty-write teams)
                      })]
        [else (displayln "\nSkipping.")]))
