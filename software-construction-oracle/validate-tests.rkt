#lang at-exp racket

(require racket/cmdline
         racket/runtime-path
         racket/pretty
         "test-fest-data.rkt"
         "util.rkt"
         "git.rkt"
         "test.rkt"
         "logger.rkt"
         "test-cache.rkt")

(module+ main
  (define assign-major-number-box (box "0"))
  (define assign-minor-number-box (box "0"))
  (define tests-repo-box (box "/not/given"))
  (define oracle-repo-box (box "/not/given"))

  (command-line
   #:once-each
   [("-M" "--Major")
    assign-number*
    "Assignment major number. E.g. for 5.2 this is 5."
    (set-box! assign-major-number-box assign-number*)]
   [("-m" "--minor")
    assign-number*
    "Assignment minor number. E.g. for 5.2 this is 2."
    (set-box! assign-minor-number-box assign-number*)]
   [("-t" "--tests-repo-path")
    path
    "Path to the valid-tests repo."
    (set-box! tests-repo-box path)]
   [("-o" "--oracle-repo-path")
    path
    "Path to the oracle repo."
    (set-box! oracle-repo-box path)])

  (define assign-number (cons (unbox assign-major-number-box)
                              (unbox assign-minor-number-box)))

  (define valid-tests-repo-path (unbox tests-repo-box))
  (define this-assign-tests-dir
    (find-cached-tests-path valid-tests-repo-path assign-number))
  (define test-repo-paths
    (filter directory-exists?
            (directory-list this-assign-tests-dir #:build? #t)))
  (define oracle-repo (unbox oracle-repo-box))
  (define all-valid-tests
    (flatten
     (for/list ([test-repo-path (in-list test-repo-paths)])
       (valid-tests/passing-oracle test-repo-path
                                   assign-number
                                   oracle-repo
                                   #:check-json-validity? #t))))

  (pretty-write (serialize-tests valid-tests-repo-path all-valid-tests)))
