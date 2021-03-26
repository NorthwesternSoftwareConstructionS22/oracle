#lang racket

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
(define assigns-with-student-tests
  '(("2" . "1")
    ("2" . "2")
    ("3" . "1")
    ("4" . "1")
    ("5" . "1")
    ("6" . "1")
    ("9" . "1")))
(define/contract assigns-requiring-test-outputs
  (flat-named-contract 'subset-of-assigns-with-student-tests?
                       (λ (l) (subset? l assigns-with-student-tests)))
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
(define oracle-repo-owner "NorthwesternSoftwareConstructionS21")
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
(define (is-student-test-validation-time?)
  ((disjoin saturday?
            sunday?
            monday?
            tuesday?
            (conjoin wednesday? before-5pm?))
   (now #:tz "America/Chicago")))
(define force-validation-env-var "SC_FORCE_VALIDATION")

