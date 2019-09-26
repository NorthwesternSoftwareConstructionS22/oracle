#lang at-exp racket

(provide clone-repo!
         clone-repos-into!)

(require "test-fest-data.rkt"
         "util.rkt"
         "logger.rkt")

(define/contract (clone-repo! repo-name)
  (repo-name? . -> . (or/c path-to-existant-directory? #f))

  (define repo-url (repo-name->url repo-name))
  (log-fest debug @~a{Cloning @repo-name ...})
  (match (system @~a{git clone "@repo-url" > /dev/null 2>&1})
    [#t
     (log-fest debug @~a{Done.})
     (build-path-string "." repo-name)]
    [#f
     (log-fest warning @~a{Failed to clone repo @repo-name})
     #f]))

(define/contract (clone-repos-into! target-dir repo-names)
  (path-to-existant-directory?
   (listof repo-name?)
   . -> .
   (hash/c repo-name? path-to-existant-directory?))

  (parameterize ([current-directory target-dir])
    (for*/hash ([repo (in-list repo-names)]
                [repo-path (in-value (clone-repo! repo))]
                #:when repo-path)
      (values repo
              (build-path-string target-dir repo-path)))))
