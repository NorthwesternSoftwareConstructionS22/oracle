#lang at-exp racket

(provide (struct-out test)
         test/c
         test-set/c

         has-test-input-file-naming-convention?
         has-test-output-file-naming-convention?
         test-input-file->team-name

         directory->tests

         test-set-count-tests
         max-number-tests)

(require "assignments.rkt"
         "util.rkt"
         "teams.rkt")

(define test-input-file-rx #rx"^input_([a-zA-Z0-9-]+)_([a-zA-Z0-9-]+).json$")
(define test-output-file-rx #rx"^output_([a-zA-Z0-9-]+)_([a-zA-Z0-9-]+).json$")

(define (has-test-input-file-naming-convention? path)
  (regexp-match? test-input-file-rx (basename path)))
(define (has-test-output-file-naming-convention? path)
  (regexp-match? test-output-file-rx (basename path)))

(define (test-input-file->team-name path)
  (match (basename path)
    [(regexp test-input-file-rx (list _ test-id name))
     name]
    [else #f]))

(module+ test
  (require rackunit)
  (check-equal? (test-input-file->team-name (bytes->path #"input_test49_robby.json"))
                "robby"))

(struct test (input-file output-file) #:transparent)
(define test/c (struct/c test
                         (and/c path-to-existant-file?
                                has-test-input-file-naming-convention?)
                         (or/c #f
                               (and/c path-to-existant-file?
                                      has-test-input-file-naming-convention?))))

(define test-set/c (hash/c team-name? (listof test/c)))

(define/contract (test-set-count-tests t)
  (test-set/c . -> . natural?)
  (for/sum ([tests (in-hash-values t)])
    (length tests)))


(define/contract (directory->tests path)
  (path-to-existant-directory? . -> . (listof test/c))

  (define all-files (directory-list path #:build? #f))
  (for/list ([maybe-input (in-list all-files)]
             #:when (has-test-input-file-naming-convention? maybe-input))
    (define corresponding-output-file
      (build-path path
                  (~a "output"
                      (string-trim (~a maybe-input)
                                   "input"
                                   #:right? #f))))
    (define output-file (and (file-exists? corresponding-output-file)
                             corresponding-output-file))
    (test (build-path path maybe-input)
          output-file)))

;; lltodo: this should be configurable easily somehow
(define (max-number-tests assign)
  (match (assign-major-number assign)
    [(app string->number (? (=/c 7))) 10]
    [else 5]))
