#lang at-exp racket

(provide all-teams
         instructor-team-names
         assign-number->active-team-names
         team-name?)

(define instructor-team-names
  '("robby"))

(define teams-1
  '("teamTest"
    ;; example format: "team1"
    ))

;; No team switch this quarter
(define teams-2
  '())

(define all-teams
  teams-1)

(define (team-name? name)
  (member name (append all-teams instructor-team-names)))

(define (assign-number->active-team-names assign)
  teams-1)
