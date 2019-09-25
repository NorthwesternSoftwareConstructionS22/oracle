#lang at-exp racket

(provide (all-defined-out))

(define student-test-repos
  '("dummy-team-tests"
    "dummy-team-tests"))

(define (repo-name->url name)
  @~a{https://github.com/NorthwesternSoftwareConstructionFall19/@|name|.git})

(define max-number-tests 5)


(define input-file-rx #rx"(.*/)input-([0-9]+)")

(define (test-input-file? path)
  (regexp-match? input-file-rx path))

(define/contract (test-input-file->output-file path)
  (test-input-file? . -> . path-string?)
  (match path
    [(regexp input-file-rx (list _ path n))
     @~a{@|path|/output-@n}]))
