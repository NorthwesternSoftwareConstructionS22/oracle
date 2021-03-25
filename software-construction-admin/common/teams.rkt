#lang at-exp racket

(provide all-teams
         instructor-team-names
         assign-number->active-team-names
         team-name?)

(define instructor-team-names
  '("robby"
    "chrdimo"))

(define teams-1
  '("f19-dummy-team"))

(define teams-2
  '())

(define all-teams
  (append teams-1
          teams-2))

(define (team-name? name)
  (member name (append all-teams instructor-team-names)))

(define (assign-number->active-team-names assign)
  (match assign
    [(cons (app string->number (? (</c 6))) _) teams-1]
    [else teams-2]))
