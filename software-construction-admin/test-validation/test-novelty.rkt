#lang at-exp racket

(provide filter-already-submitted-tests)

(require "../config.rkt"
         "../tests.rkt"
         "../common/logger.rkt"
         "../common/util.rkt"
         "../common/assignments.rkt"
         "../common/teams.rkt")

(define/contract (filter-already-submitted-tests all-tests assign-number)
  ((listof test/c) assign-number? . -> . (listof test/c))

  (log-fest-info "Checking that tests are unique.")
  (define unique-tests (remove-duplicates-within-teams all-tests))
  (unless (= (length unique-tests) (length all-tests))
    (log-fest-error @~a{
                        Filtering out duplicate tests: @;
                        @(map (compose1 pretty-path test-input-file)
                              (set-subtract all-tests unique-tests))
                        }))

  (define assign-instructor-tests (get-instructor-tests assign-number))
  (log-fest-info "Checking tests for novelty against instructor tests.")
  (define tests-different-than-instructor
    (filter-not (same-input-exists-in assign-instructor-tests)
                unique-tests))

  (cond [(member assign-number assigns-conflicting-with-past-tests)
         (log-fest-info "Checking tests for novelty against previous assignments.")
         (define all-past-assignments
           (set-subtract (take assign-sequence
                               (index-of assign-sequence assign-number))
                         assign-conflict-exceptions))
         (define all-past-tests
           (append-map (compose1 directory->tests
                                 assign-number->validated-tests-path)
                       all-past-assignments))
         (filter-not (same-input-exists-in all-past-tests)
                     tests-different-than-instructor)]
        [else tests-different-than-instructor]))


(define (instructor-test? a-test)
  (member (validated-test-input-file->team-name (test-input-file a-test))
          instructor-team-names))

(define/contract (get-instructor-tests assign-number)
  (assign-number? . -> . (listof (and/c test/c instructor-test?)))
  (filter instructor-test?
          (directory->tests (assign-number->validated-tests-path assign-number))))

(define (test->input-json a-test)
  (call-with-input-file (test-input-file a-test)
    read-json*/safe))
(define (same-input-exists-in tests)
  (define test-inputs (list->set (map test->input-json tests)))
  (λ (a-test)
    (define found? (set-member? test-inputs (test->input-json a-test)))
    (when found?
      (log-fest-error
       @~a{
           Test @(pretty-path (test-input-file a-test)) is invalid @;
           because it has already been submitted by an instructor, @;
           or in a past assignment.
           }))
    found?))

(define (remove-duplicates-within-teams all-tests)
  (define (test->team-name a-test)
    (and (has-validated-test-input-file-naming-convention? (test-input-file a-test))
         (validated-test-input-file->team-name (test-input-file a-test))))
  (define grouped (group-by test->team-name all-tests))
  (flatten (for/list ([team-tests (in-list grouped)])
             (remove-duplicates team-tests
                                #:key test->input-json))))

