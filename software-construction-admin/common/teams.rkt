#lang at-exp racket

(provide all-teams
         instructor-team-names
         assign-number->active-team-names
         team-name?)

(define instructor-team-names
  '("robby"))

(define teams-1
  '("teamTest"
    "team1"
    "team2"
    "team3"
    "team4"
    "team5"
    "team6"
    "team7"
    "team8"
    "team9"
    "team10"
    "team11"
    "team12"
    "team14"
    "team15"
    "team16"
    "team17"
    "team18"
    "team19"
    "team20"
    "team21"
    "team22"
    "team23"
    "team24"
    "team25"))

;; No team switch this quarter
(define teams-2
  '())

(define all-teams
  teams-1)

(define (team-name? name)
  (member name (append all-teams instructor-team-names)))

(define (assign-number->active-team-names assign)
  teams-1)
