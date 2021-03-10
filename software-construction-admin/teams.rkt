#lang at-exp racket

(provide all-teams
         assign->active-team-names
         team-name?)

(define teams-1
  '("dummy-team"
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
    "team13"
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
    "team25"
    "team26"
    "team27"))

(define teams-2
  '("team28"
    "team29"
    "team30"
    "team31"
    "team32"
    "team33"
    "team34"
    "team35"
    "team36"
    "team37"
    "team38"
    "team39"
    "team40"
    "team41"
    "team42"
    "team43"
    "team44"
    "team45"
    "team46"
    "team47"
    "team48"
    "team49"
    "team50"
    "team51"
    "team52"
    "team53"))

(define all-teams
  (append teams-1
          teams-2))

(define (team-name? name)
  (member name all-teams))

(define (assign->active-team-names assign)
  (match assign
    [(cons (app string->number (? (</c 6))) _) teams-1]
    [else teams-2]))
