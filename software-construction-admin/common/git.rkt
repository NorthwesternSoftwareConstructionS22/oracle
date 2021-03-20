#lang at-exp racket

(provide clone-repo!
         clone-repos-into!
         add!
         checkout-last-commit-before
         commit!
         push!
         commit-and-push!
         get-head-commit-sha
         get-status
         sha?
         check/confirm-dirty-state!)

(require "util.rkt"
         "logger.rkt"
         "team-repos.rkt")

(define git-remote-access-method (make-parameter 'ssh))

(define (sha? str)
  (and (string? str)
       (= (string-length str) 40)
       (regexp-match? #rx"^[a-z0-9]+$")))

(define/contract (clone-repo! repo-name)
  (repo-name? . -> . (or/c path-to-existant-directory? #f))

  (log-sc-info @~a{Cloning @repo-name ...})
  (define repo-url (repo-name->url repo-name (git-remote-access-method)))
  (match (system @~a{git clone "@repo-url" > /dev/null 2>&1})
    [#t
     (log-sc-debug @~a{Done.})
     (build-path-string "." repo-name)]
    [#f
     (log-sc-info @~a{Failed to clone repo @repo-name from @repo-url})
     #f]))

(define/contract (clone-repos-into! target-dir repo-names
                                    #:setup-repos [do-setup! void])
  ({path-to-existant-directory?
    (listof repo-name?)}
   {#:setup-repos (-> any)}
   . ->* .
   (hash/c repo-name? path-to-existant-directory?))

  (parameterize ([current-directory target-dir])
    (for*/hash ([repo (in-list repo-names)]
                [repo-path (in-value (clone-repo! repo))]
                #:when repo-path)
      (define full-path-to-repo
        (build-path-string (current-directory) repo-path))
      (parameterize ([current-directory full-path-to-repo])
        (do-setup!))
      (values repo full-path-to-repo))))

(define/contract (add! repo-path paths)
  (path-to-existant-directory?
   (or/c (or/c path-to-existant-file? path-to-existant-directory?)
         (listof (or/c path-to-existant-file? path-to-existant-directory?)))
   . -> .
   any)

  (parameterize ([current-directory repo-path])
    (define path-strs (if (list? paths)
                          (string-join (map ~a paths))
                          paths))
    (define output (system/string @~a{git add @path-strs}))
    (log-sc-debug @~a{
                      git-add output:
                      -----
                      @output
                      -----
                      })))

(define/contract ((checkout-last-commit-before iso-date-deadline))
  (string? . -> . (-> any))

  (define pre-deadline-commit
    (let ([time-str (format "--before='~a'" iso-date-deadline)])
      (system/string
       @~a{git rev-list --date=iso --reverse -n 1 @time-str master})))
  (log-sc-debug @~a{Checking out commit @pre-deadline-commit})
  ;; ll: This sometimes produces output despite the redirections. WTF?
  ;; hack for now: stuff it in a string
  (system/string @~a{git checkout @pre-deadline-commit > /dev/null 2>&1}))

(define/contract (get-status repo-dir)
  (path-to-existant-directory? . -> . (or/c 'clean string?))
  (parameterize ([current-directory repo-dir])
    (define status (system/string "git status"))
    (if (regexp-match? #rx"nothing to commit, working tree clean" status)
        'clean
        status)))

(define/contract (commit! repo-dir msg
                          #:add [paths-to-add empty])
  ({path-to-existant-directory? string?}
   {#:add (listof string?)}
   . ->* .
   any)

  (parameterize ([current-directory repo-dir])
    (log-sc-debug @~a{git-adding @paths-to-add})
    (add! repo-dir paths-to-add)
    (log-sc-debug @~a{Committing ...})
    (define commit-output (system/string @~a{git commit -m "@msg"}))
    (log-sc-debug @~a{
                      git commit output
                      -----
                      @commit-output
                      -----
                      })))

(define/contract (push! repo-dir
                        #:remote [remote "origin"]
                        #:branch [branch "master"])
  ({path-to-existant-directory?}
   {#:remote string?
    #:branch string?}
   . ->* .
   any)

  (parameterize ([current-directory repo-dir])
    (define push-output (system/string @~a{git push @remote @branch}))
    (log-sc-debug @~a{
                      git push @remote @branch output
                      -----
                      @push-output
                      -----
                      })))

(define/contract (commit-and-push! repo-dir msg
                                   #:remote [remote "origin"]
                                   #:branch [branch "master"]
                                   #:add [paths-to-add empty])
  ({path-to-existant-directory? string?}
   {#:remote string?
    #:branch string?
    #:add (listof string?)}
   . ->* .
   any)

  (commit! repo-dir msg
           #:add paths-to-add)
  (push! repo-dir
         #:remote remote
         #:branch branch))

(define (get-head-commit-sha [repo-dir (current-directory)]
                             #:short? [short? #f])
  (parameterize ([current-directory repo-dir])
    (string-trim (system/string @~a{git rev-parse @(if short? "--short" "") HEAD}))))

(define (check/confirm-dirty-state! repo-dir)
  (match (get-status repo-dir)
    ['clean (void)]
    [dirty-status
     #:when (equal? (user-prompt!* @~a{
                                       Warning: @repo-dir is in a dirty state:
                                       @dirty-status

                                       Continue modifying it anyway?
                                       }
                                   '(n y))
                    'y)
     (void)]
    [else
     (raise-user-error 'git "Aborting due to dirty state.")]))
