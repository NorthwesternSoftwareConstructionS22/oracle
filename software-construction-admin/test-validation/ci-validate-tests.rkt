#lang at-exp racket

(provide validated-test-log-delimeter)

(require "../tests.rkt"
         "../testing.rkt"
         "../config.rkt"
         "../common/cmdline.rkt"
         "../common/util.rkt"
         "../common/assignments.rkt"
         "../common/teams.rkt")

(define validated-test-log-delimeter "-----validated-----")

(define (same-input-exists-in tests)
  (define (test->input-json a-test)
    (call-with-input-file (test-input-file a-test)
      read-json/safe))
  (define test-inputs (list->set (map test->input-json tests)))
  (λ (a-test)
    (set-member? test-inputs (test->input-json a-test))))

(define (instructor-test? a-test)
  (member (validated-test-input-file->team-name (test-input-file a-test))
                 instructor-team-names))
(define/contract (get-instructor-tests assign-number)
  (assign-number? . -> . (listof (and/c test/c instructor-test?)))
  (filter instructor-test?
          (directory->tests (assign-number->validated-tests-path assign-number))))

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
  (define oracle-type (assign->oracle-type assign-number))
  (define oracle-path
    (assign-number->oracle-path assign-number
                                #:racket-based-oracle? (equal? oracle-type 'interacts)))
  (define all-valid-tests
    (valid-tests/passing-oracle (assign-number->submitted-tests-path assign-number)
                                oracle-path
                                oracle-type))
  (define instructor-tests (get-instructor-tests assign-number))
  (define valid-tests-different-than-instructor
    (filter-not (same-input-exists-in instructor-tests)
                all-valid-tests))

  (define valid-tests-different-than-past-assignments
    (cond [(member assign-number assigns-conflicting-with-past-tests)
           (define all-past-assignments
             (take assign-sequence
                   (index-of assign-sequence assign-number)))
           (define all-past-tests
             (append-map (compose1 directory->tests
                                   assign-number->validated-tests-path)
                         all-past-assignments))
           (filter-not (same-input-exists-in all-past-tests)
                       valid-tests-different-than-instructor)]
          [else valid-tests-different-than-instructor]))

  (display validated-test-log-delimeter)
  (write
   (for/list ([test (in-list valid-tests-different-than-past-assignments)])
     (basename (test-input-file test))))
  (displayln validated-test-log-delimeter))
