#lang at-exp racket

(provide serialize-test
         deserialize-test
         serialize-tests
         deserialize-tests
         find-cached-tests-path
         find-cached-test-info-path)

(require "test-fest-data.rkt"
         "util.rkt")

(define (serialize-test tests-repo-path)
  (match-lambda
    [(struct* test ([in in]
                    [out out]
                    [timeout-seconds timeout]))
     (list (find-relative-path-string tests-repo-path in)
           (find-relative-path-string tests-repo-path out)
           timeout)]))

(define (deserialize-test tests-repo-path)
  (match-lambda
    [(list in-rel out-rel timeout)
     (test (simple-form-path-string (build-path-string tests-repo-path in-rel))
           (simple-form-path-string (build-path-string tests-repo-path out-rel))
           timeout)]))

(define (serialize-tests tests-repo-path lot)
  (map (serialize-test tests-repo-path) lot))
(define (deserialize-tests tests-repo-path serialized)
  (map (deserialize-test tests-repo-path) serialized))

(module+ test
  (require rackunit)
  (define dir (simple-form-path-string ".."))
  (define in (simple-form-path-string "util.rkt"))
  (define out (simple-form-path-string "git.rkt"))

  (define t (test in out 42))
  (define serialized ((serialize-test dir) t))
  (check-equal? ((deserialize-test dir) serialized)
                t))


(define/contract (find-cached-tests-path tests-repo-path assign-number)
  (path-string? assign-number? . -> . string?)

  (build-path-string tests-repo-path
                     (assign-number->dir-path assign-number)))

(define (find-cached-test-info-path tests-repo-path assign-number)
  (path-string? assign-number? . -> . string?)

  (build-path-string (find-cached-tests-path tests-repo-path assign-number)
                     "valid-tests.rktd"))
