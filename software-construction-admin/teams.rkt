#lang at-exp racket

(provide all-teams
         assign-number->active-team-names
         team-name?)

(define teams-1
  '("testTeam"
    "robby"))

(define teams-2
  '())

(define all-teams
  (append teams-1
          teams-2))

(define (team-name? name)
  (member name all-teams))

(define (assign-number->active-team-names assign)
  (match assign
    [(cons (app string->number (? (</c 6))) _) teams-1]
    [else teams-2]))
