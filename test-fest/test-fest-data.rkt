#lang at-exp racket

(provide (all-defined-out)
         (struct-out test))

(require "util.rkt"
         "logger.rkt"
         racket/date)

(define student-groups
  #;'("dummy-team"
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
    "team27")
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

;; Travis kills any job running longer than 115 min
(define absolute-max-timeout-seconds (* 115 60))
;; or not producing output for 10 min
(define ci-output-timeout-seconds (* 8 60))

(define oracle-timeout (* 1 60))
(define (oracle->student-timeout secs)
  (* 100 secs))

(define max-number-tests 5)

(define input-file-rx #rx"(.*/)input([0-9]+)$")


;; sunday is 0, saturday is 6
(define pre-validated-test-days '(1 2)) ;; monday and tuesday
(define (use-pre-validated-tests?)
  (define week-day (date-week-day (current-date)))
  (member week-day pre-validated-test-days))



(define (group-name? s)
  (member s student-groups))

(define ((add-suffix suffix) str)
  (string-append str suffix))

(define group->test-repo-name (add-suffix "-tests"))
(define group->dev-repo-name (add-suffix "-dev"))

(define student-test-repos
  (map group->test-repo-name student-groups))
(define student-dev-repos
  (map group->dev-repo-name student-groups))

(define (repo->team-name repo-name)
  (match repo-name
    [(regexp #rx"(.*)-dev" (list _ name))
     name]
    [(regexp #rx"(.*)-tests" (list _ name))
     name]
    [else #f]))

(define (repo-name? name)
  (or (string=? name "oracle")
      (string=? name "valid-tests")
      (member (repo->team-name name) student-groups)))

(define/contract (repo-name->url name [mode 'https])
  ({repo-name?}
   {(or/c 'https 'ssh)}
   . ->* .
   string?)

  (define path @~a{NorthwesternSoftwareConstructionFall19/@|name|.git})
  (match mode
    ['https @~a{https://github.com/@|path|}]
    ['ssh @~a{git@"@"github.com:@|path|}]))



(define (test-input-file? path)
  (regexp-match? input-file-rx path))

(define/contract (test-input-file->output-file path)
  (test-input-file? . -> . path-string?)
  (match path
    [(regexp input-file-rx (list _ path n))
     @~a{@|path|/output@n}]
    [(? path?)
     (test-input-file->output-file (path->string path))]))



(define major-number? (and/c string? #rx"[0-9]+"))
(define minor-number? major-number?)
(define assign-number? (cons/c major-number? minor-number?))
(define/contract (assign-number->string a)
  (assign-number? . -> . string?)
  (match a
    [(cons major minor) @~a{@|major|.@|minor|}]))
(define/contract (assign-number->dir-path a)
  (assign-number? . -> . string?)
  (build-path-string (car a)
                     (assign-number->string a)))


(struct test (in out timeout-seconds) #:transparent)
(define test/c (struct/c test path-string? path-string? natural?))

(define test-set/c (hash/c repo-name? (listof test/c)))

(define/contract (test-set-count-tests t)
  (test-set/c . -> . natural?)
  (for/sum ([tests (in-hash-values t)])
    (length tests)))


(define/contract (find-oracle-file oracle-repo-path
                                   assign-number)
  (path-to-existant-directory? assign-number? . -> . path-to-existant-file?)

  (define assign-dir
    (build-path-string oracle-repo-path
                       "distribute"
                       (assign-number->dir-path assign-number)))
  (define path
    (and (directory-exists? assign-dir)
         (for/first ([f (in-directory assign-dir)]
                     #:when (equal? (path-get-extension f) #".rkt"))
           f)))
  (match path
    [#f
     (log-fest error
               @~a{
                   No oracle exists for this assignment @assign-number yet.
                   Refusing to validate any tests.
                   Try again when the oracle has been released.

                   })
     (raise-user-error 'test-fest "Missing oracle.")]
    [else path]))
