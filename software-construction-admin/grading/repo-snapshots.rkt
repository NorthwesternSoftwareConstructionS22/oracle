#lang at-exp racket


(provide team/assign-number->snapshot-path
         unpack-snapshot-into!
         current-snapshots-repo-path
         take-snapshot!
         repo-directory->snapshot!
         move-files!)

(require "../common/cmdline.rkt"
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

(define/contract (team->snapshot-name team)
  (team-name? . -> . string?)
  (~a team ".zip"))

(define/contract (team/assign-number->snapshot-path team assign-number)
  (team-name? assign-number? . -> . path-string?)

  (build-path (assign-number->snapshots-dir assign-number)
              (team->snapshot-name team)))

(define/contract (move-files! source-dir dest-dir preserve-files)
  (path-to-existant-directory?
   path-to-existant-directory?
   (listof string?)
   . -> .
   any)
  (for ([f (in-list (directory-list source-dir))]
        #:unless (matches-any? preserve-files (path->string f)))
    (define f-path (build-path source-dir f))
    (log-sc-debug @~a{Moving @f-path to @(simple-form-path dest-dir)})
    (rename-file-or-directory f-path (build-path dest-dir f))))

(define/contract (unpack-snapshot-into! snapshot-path
                                        destination
                                        preserve-files)
  (path-to-existant-file?
   path-to-existant-directory?
   (listof string?)
   . -> .
   sha?)

  (log-sc-info @~a{Unpacking @snapshot-path to @(simple-form-path destination)})
  (call-with-temp-directory
   #:name-seed "unpack-snapshot"
   (λ (snapshot-unpack-path)
     (define snapshot-name (basename snapshot-path))
     (log-sc-debug
      @~a{
          Unpacking @(pretty-path snapshot-path) into @snapshot-unpack-path first, @;
          then moving each file to @(simple-form-path destination)
          })
     (define snapshot-copy-path (build-path snapshot-unpack-path snapshot-name))
     (copy-file snapshot-path snapshot-copy-path)
     (define repo-snapshot-path (unzip-in-place! snapshot-copy-path))
     (define snapshot-commit-sha (get-head-commit-sha repo-snapshot-path))
     (move-files! repo-snapshot-path destination preserve-files)
     snapshot-commit-sha)))

(define/contract (take-snapshots! teams destination-dir configure-repo!)
  ((listof team-name?)
   path-to-existant-directory?
   (path-to-existant-directory? . -> . any)
   . -> .
   (listof path-to-existant-file?))

  (define full-destination-dir-path (simple-form-path destination-dir))
  (call-with-temp-directory
   #:name-seed "take-snapshot"
   (λ (temp-dir)
     (parameterize ([current-directory temp-dir])
       (define all-response-box (box #f))
       (for/list ([team (in-list teams)])
         (define repo (clone-repo! (team->dev-repo-name team)))
         (define repo-zip (repo-directory->snapshot! repo configure-repo!))
         (define snapshot-name (team->snapshot-name team))
         (define destination (build-path full-destination-dir-path snapshot-name))
         (if (file-exists? destination)
             (match (or (unbox all-response-box)
                        (user-prompt!* @~a{
                                           @(pretty-path destination) already exists. @;
                                           Delete it, skip, or abort?
                                           }
                                       '(d s a delete-all skip-all)))
               [(and (or 'd 'delete-all) response)
                (when (equal? response 'delete-all) (set-box! all-response-box 'd))
                (delete-file destination)
                (rename-file-or-directory repo-zip destination)]
               [(and (or 's 'skip-all) response)
                (when (equal? response 'skip-all) (set-box! all-response-box 's))
                (void)]
               ['a (raise-user-error 'take-snapshots!
                                     "Aborting due to already existant snapshot.")])
             (rename-file-or-directory repo-zip destination))
         destination)))))

(define/contract (repo-directory->snapshot! repo configure-repo!)
  (->i ([repo path-to-existant-directory?]
        [configure-repo! (path-to-existant-directory? . -> . any)])
       [result path-to-existant-file?]
       #:post {repo} (not (directory-exists? repo)))
  (configure-repo! repo)
  (zip! repo))

(define/contract (take-snapshot! team destination-dir configure-repo!)
  (team-name?
   path-to-existant-directory?
   (path-to-existant-directory? . -> . any)
   . -> .
   path-to-existant-file?)

  (first (take-snapshots! (list team) destination-dir configure-repo!)))

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

  (define snapshot-dir (simple-form-path (assign-number->snapshots-dir assign-number)))
  (make-directory* snapshot-dir)
  (log-sc-info @~a{Taking dev repo snapshots in @snapshot-dir ...})
  (void (take-snapshots! teams
                         snapshot-dir
                         ;; Note: This just uses the default branch of the repo.
                         ;; Meaning that all grading will be done based on
                         ;; commits to the default branch of the repo as
                         ;; configured on github.
                         (checkout-last-commit-before deadline)))
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
