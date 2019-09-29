#lang at-exp racket

(provide (all-defined-out))

(require "test-fest-data.rkt"
         "util.rkt"
         "logger.rkt")

(define git-remote-access-method (make-parameter 'https))

(define/contract (clone-repo! repo-name)
  (repo-name? . -> . (or/c path-to-existant-directory? #f))

  (define repo-url (repo-name->url repo-name (git-remote-access-method)))
  (log-fest debug @~a{Cloning @repo-name ...})
  (match (system @~a{git clone "@repo-url" > /dev/null 2>&1})
    [#t
     (log-fest debug @~a{Done.})
     (build-path-string "." repo-name)]
    [#f
     (log-fest warning @~a{Failed to clone repo @repo-name from @repo-url})
     #f]))

(define/contract (clone-repos-into! target-dir repo-names
                                    #:setup-repos [do-setup! void])
  ({path-to-existant-directory?
    (listof repo-name?)}
   {#:setup-repos (-> void?)}
   . ->* .
   (hash/c repo-name? path-to-existant-directory?))

  (parameterize ([current-directory target-dir])
    (for*/hash ([repo (in-list repo-names)]
                [repo-path (in-value (clone-repo! repo))]
                #:when repo-path)
      (do-setup!)
      (values repo
              (build-path-string target-dir repo-path)))))

(define/contract (git-add path)
  ((or/c path-to-existant-file? path-to-existant-directory?) . -> . void?)

  (system @~a{git add @path}))

(define (system/string cmd)
  (call-with-output-string
   (λ (out)
     (parameterize ([current-output-port out]
                    [current-error-port out])
       (system cmd)))))

(define/contract (checkout-last-commit-before iso-date-deadline)
  (string? . -> . void?)

  (define pre-deadline-commit
    (let ([time-str (format "--before='~a'" iso-date-deadline)])
      (system/string
       @~a{git rev-list --date=iso --reverse -n 1 @time-str master})))
  (system @~a{git checkout @pre-deadline-commit}))
