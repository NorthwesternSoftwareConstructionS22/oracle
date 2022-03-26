#lang at-exp racket

(require "../tests.rkt"
         "../common/assignments.rkt"
         "../common/util.rkt"
         racket/runtime-path)

(define-runtime-path validated-dir "../../welcome-to-oracle/validated-tests")
(define-runtime-path robbys-dir "../../../welcome-to/Deliverables")

(define (ensure-empty! dir)
  (when (directory-exists? dir)
    (delete-directory/files dir))
  (make-directory* dir))

(for* ([assign '(("3" . "1") ("3" . "2") ("4" . "1") ("5" . "1") ("5" . "2") ("6" . "1"))]
       [dest (in-value (build-path validated-dir (assign-number->dir-path-part assign)))]
       #:when (ensure-empty! dest) ; for side effect only
       [f (in-list (directory-list (build-path robbys-dir (assign-number->dir-path-part assign))
                                   #:build? #t))]
       #:when (has-test-input-file-naming-convention? f))
  (copy-file f (build-path dest (test-file-name->validated (basename f) "robby"))))
