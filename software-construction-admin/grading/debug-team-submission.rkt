#lang at-exp racket

;; (require racket/cmdline
;;          racket/runtime-path
;;          "../oracle/test-fest/test-fest-data.rkt"
;;          "../oracle/test-fest/util.rkt"
;;          "../oracle/test-fest/tar.rkt"
;;          "../oracle/test-fest/git.rkt"
;;          "../oracle/test-fest/env.rkt")

;; (define-runtime-path debug-dir ".")
;; (define-runtime-path grading-repo-path "../grading")
;; (define env-file "env.sh")
;; (define debug-files `("debug-team-submission.rkt"
;;                       ".travis.yml"
;;                       ".git"
;;                       ,env-file))

;; (define/contract (matches-any? los some-str)
;;   ((listof string?) string? . -> . boolean?)

;;   (for/or ([s (in-list los)])
;;     (string-contains? s some-str)))

;; (define/contract (clean-directory! dir preserve)
;;   (path-to-existant-directory? (listof string?) . -> . any)

;;   (for ([f (in-list (directory-list dir))]
;;         #:unless (matches-any? preserve (path->string f)))
;;     (define f-path (build-path dir f))
;;     (displayln @~a{Deleting @f-path})
;;     (if (file-exists? f-path)
;;         (delete-file f-path)
;;         (delete-directory/files f-path))))

;; (define/contract (copy-repo-files! from to file-exceptions do-before-copy)
;;   (path-to-existant-directory?
;;    path-to-existant-directory?
;;    (listof string?)
;;    (-> any)
;;    . -> .
;;    any)

;;   (parameterize ([current-directory from])
;;     (do-before-copy))

;;   (for ([f (in-list (directory-list from))]
;;         #:unless (matches-any? file-exceptions (path->string f)))
;;     (define f-path (build-path from f))
;;     (displayln @~a{Copying @f-path to @to})
;;     (if (file-exists? f-path)
;;         (copy-file f-path (build-path to f))
;;         (copy-directory/files f-path
;;                               (build-path to f)
;;                               #:preserve-links? #t))))

;; (define/contract (copy-team-submission! team assign-number to file-exceptions
;;                                         #:before-copy [do-before-copy void])
;;   ({string?
;;     assign-number?
;;     path-to-existant-directory?
;;     (listof string?)}
;;    {#:before-copy (-> any)}
;;    . ->* .
;;    any)

;;   (define team-repo-zip
;;     (build-path-string grading-repo-path
;;                        (assign-number->string assign-number)
;;                        @~a{@|team|-dev.tar.gz}))
;;   (displayln @~a{Unzipping @team-repo-zip})
;;   (define team-repo-path
;;     (unzip! team-repo-zip))

;;   (copy-repo-files! team-repo-path to file-exceptions do-before-copy)
;;   (delete-directory/files team-repo-path))

;; (define/contract (clone-team-repo! team to file-exceptions
;;                                    #:before-copy [do-before-copy void])
;;   ({string?
;;     path-to-existant-directory?
;;     (listof string?)}
;;    {#:before-copy (-> any)}
;;    . ->* .
;;    any)

;;   (define tmp (find-system-path 'temp-dir))
;;   (define clone-path (build-path-string tmp team))
;;   (delete-directory/files clone-path #:must-exist? #f)

;;   (make-directory clone-path)
;;   (define repo-root-name
;;     (parameterize ([current-directory clone-path]
;;                    [git-remote-access-method 'ssh])
;;       (clone-repo! @~a{@|team|-dev})))
;;   (define repo-root-path (build-path-string clone-path repo-root-name))

;;   (copy-repo-files! repo-root-path to file-exceptions do-before-copy)
;;   (delete-directory/files clone-path))

;; (define (display-commit!)
;;   (display "\n  Debugging commit: ")
;;   (system @~a{git rev-parse --short HEAD})
;;   (newline))

;; (define ((checkout-commit hash-code))
;;   (system @~a{git checkout @hash-code}))

;; (module+ main
;;   (define assign-major-number-box (box "0"))
;;   (define assign-minor-number-box (box "0"))
;;   (define team-box (box "dummy-team"))
;;   (define grading-snapshot?-box (box #t))
;;   (define commit-box (box #f))

;;   (command-line
;;    #:once-each
;;    [("-M" "--Major")
;;     assign-number*
;;     "Assignment major number. E.g. for 5.2 this is 5."
;;     (set-box! assign-major-number-box assign-number*)]
;;    [("-m" "--minor")
;;     assign-number*
;;     "Assignment minor number. E.g. for 5.2 this is 2."
;;     (set-box! assign-minor-number-box assign-number*)]
;;    [("-t" "--team")
;;     name
;;     "Team name."
;;     (set-box! team-box name)]
;;    [("-c" "--current")
;;     "Debug the current state of the team's repo instead of a grading snapshot."
;;     (set-box! grading-snapshot?-box #f)]
;;    [("-k" "--commit")
;;     hash-code
;;     "Pick commit to debug."
;;     (set-box! commit-box hash-code)])

;;   (define assign-number (cons (unbox assign-major-number-box)
;;                               (unbox assign-minor-number-box)))
;;   (define team (unbox team-box))

;;   (define before-copy-action
;;     (if (unbox commit-box)
;;         (checkout-commit (unbox commit-box))
;;         display-commit!))

;;   (void
;;    (clean-directory! debug-dir debug-files)

;;    (if (unbox grading-snapshot?-box)
;;        (copy-team-submission! team assign-number debug-dir debug-files
;;                               #:before-copy before-copy-action)
;;        (clone-team-repo! team debug-dir debug-files
;;                          #:before-copy before-copy-action))

;;    (write-env! debug-dir env-file team assign-number)
;;    (displayln "Ready to push; hit enter to continue, or Ctrl-C to cancel.")
;;    (with-handlers ([exn:break? (λ _
;;                                  (displayln "Cancelled!")
;;                                  (exit 1))])
;;      (read-line))
;;    (commit-and-push! debug-dir @~a{@team @(assign-number->string assign-number)}
;;                      #:remote "origin"
;;                      #:branch "master"
;;                      #:add '("."))))
