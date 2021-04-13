#lang at-exp racket

(provide validated-test-log-delimeter)

(require "test-novelty.rkt"
         "../tests.rkt"
         "../testing.rkt"
         "../config.rkt"
         "../common/cmdline.rkt"
         "../common/util.rkt"
         "../common/assignments.rkt"
         "../common/teams.rkt"
         "../common/logger.rkt")

(define validated-test-log-delimeter "-----validated-----")

(module+ main
  (match-define (cons (hash-table ['major major-number]
                                  ['minor minor-number])
                      args)
    (command-line/declarative
     #:once-each
     [("-M" "--Major")
      'major
      "Assignment major number. E.g. for 5.2 this is 5."
      #:collect {"number" take-latest #f}
      #:mandatory]
     [("-m" "--minor")
      'minor
      "Assignment minor number. E.g. for 5.2 this is 2."
      #:collect {"number" take-latest #f}
      #:mandatory]))

  (define assign-number (cons major-number minor-number))
  (define oracle-number-for-validation (oracle-number-for-validating assign-number))
  (define oracle-type (assign->oracle-type oracle-number-for-validation))
  (define oracle-path
    (assign-number->oracle-path oracle-number-for-validation
                                #:racket-based-oracle? (equal? oracle-type 'interacts)))
  (define all-valid-tests
    (valid-tests/passing-oracle (assign-number->submitted-tests-path assign-number)
                                oracle-path
                                oracle-type
                                #:check-json-validity? all-valid-tests-must-be-json?
                                #:require-output-file? (and (member assign-number
                                                                    assigns-requiring-test-outputs)
                                                            #t)))
  (define novel-valid-tests
    (filter-already-submitted-tests all-valid-tests
                                    assign-number))

  (display validated-test-log-delimeter)
  (write
   (for/list ([test (in-list novel-valid-tests)])
     (basename (test-input-file test))))
  (displayln validated-test-log-delimeter))
