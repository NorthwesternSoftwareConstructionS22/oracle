#lang racket/base

(provide (all-defined-out))

(require racket/runtime-path
         racket/format)

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
(define oracle-repo-owner "NorthwesternSoftwareConstructionFall19")
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

(define assign-sequence
  '(("1" . "2")
    ("2" . "1")
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

;; ;; sunday is 0, saturday is 6
;; (define pre-validated-test-days '(1 2 3 4)) ;; monday - thursday
;; (define (use-pre-validated-tests?)
;;   (define week-day (->wday (today #:tz "America/Chicago")))
;;   (log-fest-debug
;;             @~a{Today: @week-day, pre-valid-days: @pre-validated-test-days})
;;   (member week-day pre-validated-test-days))

