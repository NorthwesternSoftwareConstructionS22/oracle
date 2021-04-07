#lang at-exp racket

(provide team->dev-repo-name
         repo-name->url
         repo-name?)

(require "teams.rkt"
         "../config.rkt")

(define ((add-suffix suffix) str)
  (string-append str suffix))

(define team->dev-repo-name identity)

(define repo->team-name identity)

(define (repo-name? name)
  (or (string=? name "oracle")
      (member (repo->team-name name) all-teams)))

(define/contract (repo-name->url name [mode 'https])
  ({repo-name?}
   {(or/c 'https 'ssh)}
   . ->* .
   string?)

  (define path @~a{@|course-github-organization|/@|name|.git})
  (match mode
    ['https @~a{https://github.com/@|path|}]
    ['ssh @~a{git@"@"github.com:@|path|}]))
