#lang at-exp racket

(provide (all-defined-out))

(require "util.rkt"
         "logger.rkt"
         "team-repos.rkt")

(define git-remote-access-method (make-parameter 'https))

(define/contract (clone-repo! repo-name)
  (repo-name? . -> . (or/c path-to-existant-directory? #f))

  (define repo-url (repo-name->url repo-name (git-remote-access-method)))
  (log-fest-debug @~a{Cloning @repo-name ...})
  (match (system @~a{git clone "@repo-url" > /dev/null 2>&1})
    [#t
     (log-fest-debug @~a{Done.})
     (build-path-string "." repo-name)]
    [#f
     (log-fest-error @~a{Failed to clone repo @repo-name from @repo-url})
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

(define/contract (git-add path)
  ((or/c path-to-existant-file? path-to-existant-directory?) . -> . any)

  (system @~a{git add @path}))

(define/contract ((checkout-last-commit-before iso-date-deadline))
  (string? . -> . (-> any))

  (define pre-deadline-commit
    (let ([time-str (format "--before='~a'" iso-date-deadline)])
      (system/string
       @~a{git rev-list --date=iso --reverse -n 1 @time-str master})))
  ;; ll: This sometimes produces output despite the redirections. WTF?
  ;; hack for now: stuff it in a string
  (system/string @~a{git checkout @pre-deadline-commit > /dev/null 2>&1}))

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

  (define /dev/null (open-output-nowhere))
  (parameterize ([current-directory repo-dir]
                 [current-output-port /dev/null])
    (for-each git-add paths-to-add)
    (system @~a{git commit -m "@msg"})
    (system @~a{git push @remote @branch}))
  (close-output-port /dev/null))
