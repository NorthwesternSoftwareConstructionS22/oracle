#lang at-exp racket

(provide (struct-out test)
         test/c
         test-set/c

         has-test-input-file-naming-convention?
         has-test-output-file-naming-convention?
         has-validated-test-input-file-naming-convention?
         has-validated-test-output-file-naming-convention?
         validated-test-input-file->team-name
         test-file-name->validated

         directory->tests

         test-set-count-tests
         max-number-tests)

(require "common/assignments.rkt"
         "common/util.rkt"
         "common/teams.rkt")

(define test-file-rx @~a{([a-zA-Z0-9_-]+).json$})
(define validated-test-file-rx @~a{([a-zA-Z0-9-]+)_([a-zA-Z0-9_-]+).json$})

(define test-input-file-rx (pregexp @~a{^input@test-file-rx}))
(define test-output-file-rx (pregexp @~a{^output@test-file-rx}))
(define validated-test-input-file-rx (pregexp @~a{^input@validated-test-file-rx}))
(define validated-test-output-file-rx (pregexp @~a{^output@validated-test-file-rx}))

(define (has-test-input-file-naming-convention? path)
  (regexp-match? test-input-file-rx (basename path)))
(define (has-test-output-file-naming-convention? path)
  (regexp-match? test-output-file-rx (basename path)))
(define (has-validated-test-input-file-naming-convention? path)
  (regexp-match? validated-test-input-file-rx (basename path)))
(define (has-validated-test-output-file-naming-convention? path)
  (regexp-match? validated-test-output-file-rx (basename path)))

(define/contract (validated-test-input-file->team-name path)
  (validated-test-input-file-rx . -> . team-name?)

  (match (basename path)
    [(regexp validated-test-input-file-rx (list _ name test-id))
     name]
    [else #f]))

(define/contract (test-file-name->validated filename team)
  ((or/c test-input-file-rx test-output-file-rx)
   team-name?
   . -> .
   (or/c validated-test-input-file-rx
                                                              validated-test-output-file-rx))

  (define prefix (match filename
                   [(regexp test-input-file-rx  (list _ prefix)) (~a "input"  prefix)]
                   [(regexp test-output-file-rx (list _ prefix)) (~a "output" prefix)]))
  (~a prefix "_" team ".json"))


(struct test (input-file output-file) #:transparent)
(define test/c (struct/c test
                         (and/c path-to-existant-file?
                                has-test-input-file-naming-convention?)
                         (or/c #f
                               (and/c path-to-existant-file?
                                      has-test-output-file-naming-convention?))))

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
