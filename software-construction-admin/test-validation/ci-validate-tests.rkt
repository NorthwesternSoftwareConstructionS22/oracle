#lang at-exp racket

(provide validated-test-log-delimeter)

(require "../tests.rkt"
         "../testing.rkt"
         "../common/cmdline.rkt"
         "../common/util.rkt"
         "../common/assignments.rkt")

(define validated-test-log-delimeter "-----validated-----")

(define (same-input-exists-in tests)
  (define (test->input-json a-test)
    (call-with-input-file (test-input-file a-test)
      read-json/safe))
  (define test-inputs (list->set (map test->input-json tests)))
  (λ (a-test)
    (set-member? test-inputs (test->input-json a-test))))

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
  (define all-valid-tests
    (valid-tests/passing-oracle (assign-number->submitted-tests-path assign-number)
                                (assign-number->oracle-path assign-number)
                                #:check-json-validity? #t))
  (define instructor-tests
    (directory->tests (assign-number->validated-tests-path assign-number)))
  (define valid-tests-different-than-instructor
    (filter-not (same-input-exists-in instructor-tests)
                all-valid-tests))

  (display validated-test-log-delimeter)
  (write
   (for/list ([test (in-list valid-tests-different-than-instructor)])
     (basename (test-input-file test))))
  (displayln validated-test-log-delimeter))
