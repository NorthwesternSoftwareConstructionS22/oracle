#lang at-exp racket

(provide team->test-repo-name
         team->dev-repo-name
         repo-name->url)

(require "teams.rkt")

(define ((add-suffix suffix) str)
  (string-append str suffix))

(define team->test-repo-name (add-suffix "-tests"))
(define team->dev-repo-name (add-suffix "-dev"))

(define (repo->team-name repo-name)
  (match repo-name
    [(regexp #rx"^(.*)-dev" (list _ name))
     name]
    [(regexp #rx"^(.*)-tests" (list _ name))
     name]
    [else #f]))

(define (repo-name? name)
  (or (string=? name "oracle")
      (member (repo->team-name name) all-teams)))

(define/contract (repo-name->url name [mode 'https])
  ({repo-name?}
   {(or/c 'https 'ssh)}
   . ->* .
   string?)

  (define path @~a{NorthwesternSoftwareConstructionFall19/@|name|.git})
  (match mode
    ['https @~a{https://github.com/@|path|}]
    ['ssh @~a{git@"@"github.com:@|path|}]))
