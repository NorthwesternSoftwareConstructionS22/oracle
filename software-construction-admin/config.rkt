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
(define oracle-repo-owner "llazarek")
(define oracle-repo-name "nu-sc-oracle")
(define oracle-repo-remote (~a "git@github.com:"
                               oracle-repo-owner
                               "/"
                               oracle-repo-name
                               ".git"))
(define oracle-repo-branch "master")


(define-runtime-path grading-repo-path "../../grading")
(define grading-repo-owner "llazarek")
(define grading-repo-name "nu-sc-f19-grading")
(define grading-repo-remote (~a "git@github.com:"
                                grading-repo-owner
                                "/"
                                grading-repo-name
                                ".git"))
(define grading-repo-branch "master")

