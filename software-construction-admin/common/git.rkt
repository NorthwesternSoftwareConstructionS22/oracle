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
         get-current-branch
         sha?
         check/confirm-dirty-state!
         checkout!)

(require "util.rkt"
         "logger.rkt"
         "team-repos.rkt")

(define git-remote-access-method (make-parameter 'ssh))

(define (sha? str)
  (and (string? str)
       (= (string-length str) 40)
       (regexp-match? #rx"^[a-z0-9]+$" str)))

(define (git cmd
             #:in [repo-dir (current-directory)]
             #:output? [output? #t])
  (parameterize ([current-directory repo-dir])
    ((if output? system/string system)
     (~a "git " cmd))))

(define/contract (clone-repo! repo-name)
  (repo-name? . -> . (or/c path-to-existant-directory? #f))

  (log-sc-info @~a{Cloning @repo-name ...})
  (define repo-url (repo-name->url repo-name (git-remote-access-method)))
  (match (git @~a{clone "@repo-url" > /dev/null 2>&1}
              #:output? #f)
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

  (define path-strs (if (list? paths)
                        (string-join (map ~a paths))
                        paths))
  (define output (git @~a{add @path-strs}
                      #:in repo-path))
  (log-sc-debug @~a{
                    git-add output:
                    -----
                    @output
                    -----
                    }))

(define/contract (get-current-branch repo-dir)
  (path-to-existant-directory? . -> . string?)
  (match (git "branch" #:in repo-dir)
    [(regexp #rx"(?m:^\\* (.+?)$)" (list _ branch)) branch]
    [other (raise-user-error 'get-current-branch
                             @~a{
                                 Couldn't get current branch of @repo-dir
                                 Git says: @other
                                 })]))

(define/contract ((checkout-last-commit-before iso-date-deadline
                                               ; #f means default/current branch
                                               [branch #f])
                  repo-dir)
  ({string?}
   {(or/c #f string?)}
   . ->* .
   (path-to-existant-directory? . -> . any))

  (define branch-to-checkout (or branch
                                 (get-current-branch repo-dir)))
  (define pre-deadline-commit
    (string-trim
     (git @~a{rev-list --date=iso --reverse -n 1 --before='@iso-date-deadline' @branch-to-checkout}
          #:in repo-dir)))
  (log-sc-info @~a{Checking out commit @pre-deadline-commit})
  (checkout! repo-dir pre-deadline-commit))

(define/contract (get-status repo-dir)
  (path-to-existant-directory? . -> . (or/c 'clean string?))
  (define status (git "status" #:in repo-dir))
  (if (regexp-match? #rx"nothing to commit, working tree clean" status)
      'clean
      status))

(define/contract (commit! repo-dir msg
                          #:add [paths-to-add empty])
  ({path-to-existant-directory? string?}
   {#:add (listof path-string?)}
   . ->* .
   any)

  (log-sc-debug @~a{git-adding @paths-to-add})
  (add! repo-dir paths-to-add)
  (log-sc-debug @~a{Committing ...})
  (define commit-output (git @~a{commit -m "@msg"}
                             #:in repo-dir))
  (log-sc-debug @~a{
                    git commit output
                    -----
                    @commit-output
                    -----
                    }))

(define/contract (push! repo-dir
                        #:remote [remote "origin"]
                        #:branch [branch "master"])
  ({path-to-existant-directory?}
   {#:remote string?
    #:branch string?}
   . ->* .
   any)

  (define push-output (git @~a{push @remote @branch}
                           #:in repo-dir))
  (log-sc-debug @~a{
                    git push @remote @branch output
                    -----
                    @push-output
                    -----
                    }))

(define/contract (commit-and-push! repo-dir msg
                                   #:remote [remote "origin"]
                                   #:branch [branch "master"]
                                   #:add [paths-to-add empty])
  ({path-to-existant-directory? string?}
   {#:remote string?
    #:branch string?
    #:add (listof path-string?)}
   . ->* .
   any)

  (commit! repo-dir
           msg
           #:add paths-to-add)
  (push! repo-dir
         #:remote remote
         #:branch branch))

(define (get-head-commit-sha [repo-dir (current-directory)]
                             #:short? [short? #f])
  (string-trim (git @~a{rev-parse @(if short? "--short" "") HEAD}
                    #:in repo-dir)))

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

(define (checkout! repo-dir ref)
  (log-sc-debug @~a{git: checking out @ref})
  ;; ll: This sometimes produces output despite the redirections. WTF?
  ;; hack for now: stuff it in a string
  (git @~a{checkout @ref > /dev/null 2>&1}
       #:in repo-dir))
