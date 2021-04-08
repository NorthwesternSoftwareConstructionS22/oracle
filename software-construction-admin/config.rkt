#lang at-exp racket

(provide (all-defined-out))

(require racket/runtime-path
         gregor)

;; See also common/teams.rkt to configure teams

(define assign-sequence
  '(("2" . "1")
    ("2" . "2")
    ("3" . "1")
    ("4" . "1")
    ("5" . "1")
    ("5" . "2")
    ("6" . "1")
    ("7" . "1")
    ("8" . "1")
    ("9" . "1")))
(define assigns-conflicting-with-past-tests
  '(("5" . "1")
    ("5" . "2")
    ("6" . "1")
    ("9" . "1")))
(define assign-conflict-exceptions
  ;; These assignments won't be checked when checking test novelty against past tests
  ;; (for those assignments in `assigns-conflicting-with-past-tests`)
  '(("2" . "1")
    ("2" . "2")))
(define assigns-with-student-tests
  '(("2" . "1")
    ("2" . "2")
    ("3" . "1")
    ("4" . "1")
    ("5" . "1")
    ("6" . "1")
    ("9" . "1")))
(define (assign-with-student-test? a)
  (member a assigns-with-student-tests))
(define/contract assigns-requiring-test-outputs
  (listof assign-with-student-test?)
  '(("2" . "1")
    ("2" . "2")
    ("3" . "1")
    ("4" . "1")))
(define oracle-type/c (or/c 'normal 'checks-output 'interacts))
(define/contract assign->oracle-type
  (any/c . -> . oracle-type/c)
  (match-lambda [(cons (or "5" "9") _)     'checks-output]
                [(cons (or "6" "7" "8") _) 'interacts]
                [else                      'normal]))

;; this used to depend on the amount time the oracle took but since
;; the oracle sometimes needs the student's output, this is a
;; constant number of seconds for now
(define submission-timeout-seconds 5)
(define oracle-timeout-seconds (* 1 60))

(define (expected-valid-test-count assign-number)
  (if (member assign-number assigns-with-student-tests)
      5
      0))
(define all-valid-tests-must-be-json? #t)


(define course-github-organization "NorthwesternSoftwareConstructionS21")

(define-runtime-path test-snapshots-repo-path "../../test-snapshots")
(define-runtime-path submission-snapshots-repo-path "../../submission-snapshots")
(define default-snapshots-repo-path test-snapshots-repo-path)

(define-runtime-path oracle-repo-path "..")
(define validated-tests-dir (build-path oracle-repo-path
                                        "backgammon-oracle/validated-tests"))
(define submitted-tests-dir (build-path oracle-repo-path
                                        "backgammon-oracle/submitted-tests"))
(define oracle-binary-dir (build-path oracle-repo-path
                                      "backgammon-oracle/oracle"))
(define oracle-repo-owner course-github-organization)
(define oracle-repo-name "oracle")
(define oracle-repo-remote (~a "git@github.com:"
                               oracle-repo-owner
                               "/"
                               oracle-repo-name
                               ".git"))
(define oracle-repo-branch "master")


(define-runtime-path grading-repo-path "../../grading")
(define grading-repo-owner "LLazarek")
(define grading-repo-name "nu-sc-grading")
(define grading-repo-remote (~a "git@github.com:"
                                grading-repo-owner
                                "/"
                                grading-repo-name
                                ".git"))
(define grading-repo-branch "master")

(define (before-5pm? time) (< (->hours time) 17))
(define/contract assign-test-deadlines
  (hash/c assign-with-student-test?
          moment?)
  (hash '("2" . "1") (moment 2021 4 7 17
                             #:tz "America/Chicago")
        '("2" . "2") (moment 2021 4 7 17
                             #:tz "America/Chicago")
        '("3" . "1") (moment 2021 4 14 17
                             #:tz "America/Chicago")
        '("4" . "1") (moment 2021 4 21 17
                             #:tz "America/Chicago")
        '("5" . "1") (moment 2021 4 28 17
                             #:tz "America/Chicago")
        '("6" . "1") (moment 2021 5 5 17
                             #:tz "America/Chicago")
        '("9" . "1") (moment 2021 5 26 17
                             #:tz "America/Chicago")))
(define (is-student-test-validation-time? assign)
  (define current-time (now/moment #:tz "America/Chicago"))
  (moment<? current-time
            (hash-ref assign-test-deadlines assign current-time)))
(define force-validation-env-var "SC_FORCE_VALIDATION")


(require "common/logger.rkt")
(define (strip-.0s original-in)
  (define original-bytes (port->bytes original-in))
  (define stripped-bytes (regexp-replace* #rx#"([0-9])[.]0+"
                                          original-bytes
                                          #"\\1"))
  (unless (bytes=? original-bytes stripped-bytes)
    (log-fest-info
     @~a{
         Normalizing contents of @(object-name original-in) to remove `.0`s and using that instead.
         Original contents:
         ------------------------------
         @original-bytes
         ------------------------------

         Normalized contents:
         ------------------------------
         @stripped-bytes
         ------------------------------
         }))
  (open-input-bytes stripped-bytes))

(module+ test
  (require rackunit)
  (check-equal? (port->bytes (strip-.0s (open-input-bytes #"1")))
                #"1")
  (check-equal? (port->bytes (strip-.0s (open-input-bytes #"1.")))
                #"1.")
  (check-equal? (port->bytes (strip-.0s (open-input-bytes #"1.0")))
                #"1")
  (check-equal? (port->bytes (strip-.0s (open-input-bytes #"1.00000")))
                #"1")
  (check-equal? (port->bytes (strip-.0s (open-input-bytes #"1.02")))
                #"12")
  (check-equal? (port->bytes (strip-.0s (open-input-bytes #"1.02.0")))
                #"12"))

(define/contract test-transformer
  (input-port? . -> . input-port?)
  strip-.0s)
