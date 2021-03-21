#lang at-exp racket

(provide travis:job-id/c
         travis:trigger-build!
         travis:get-log!
         travis:get-job-status!)

(require "../common/option.rkt"
         "travis-api.rkt")

(define travis:job-id/c (list/c positive-integer?
                                positive-integer?))

(define/contract (travis:trigger-build! repo-owner
                                        repo-name
                                        commit-ref
                                        build-msg)
  (string? string? string? string? . -> . (option/c travis:job-id/c))

  (option-let*
   [build-info
    (travis-request!
     @~a{/repo/@|repo-owner|%2F@|repo-name|/requests}
     #:method POST
     #:data (string->bytes/utf-8
             ;; lltodo: does the message need to be the commit message without [skip travis]?
             ;; That's what I had before.
             @~a{{"request":{"message":"@build-msg", "branch":"@commit-ref"}}})
     #:output-type "application/json"
     #:extra-headers '("Content-Type: application/json"))]
   [_ (fail-if (not (hash? build-info)) @~a{Error: @build-info})]

   [request-info (hash-ref/option build-info
                                  'request
                                  @~a{Request failed, got: @build-info})]
   [request-id (hash-ref/option request-info 'id)]
   [repo-info (hash-ref/option request-info 'repository)]
   [repo-id (hash-ref/option repo-info 'id)]
   (list repo-id request-id)))

(define/contract (travis:get-log! job-id
                                  #:must-be-completed? [complete? #t]
                                  #:fail-for-job-failure? [skip-failed-job? #t])
  ({travis:job-id/c}
   {#:must-be-completed? boolean?
    #:fail-for-job-failure? boolean?}
   . ->* .
   (option/c string?))

  (option-let*
   [(list repo-id request-id) job-id]
   [status-info (travis-request! @~a{/repo/@|repo-id|/request/@|request-id|})]
   [_           (fail-if (not (hash? status-info)) @~a{Error: @status-info})]

   [builds      (hash-ref/option status-info 'builds "No builds yet")]
   [a-build     (first/option    builds "No builds yet")]
   [build-id    (hash-ref/option a-build     'id)]
   [build-state (hash-ref/option a-build     'state)]

   [_           (fail-if (and skip-failed-job?
                              (member build-state '("canceled" "errored")))
                         "Job canceled or errored.")]
   [_           (fail-if (and complete?
                              (not (and (not skip-failed-job?)
                                        (member build-state '("canceled" "errored"))))
                              (not (member build-state '("passed" "failed"))))
                         "Build not done")]

   [build-info  (travis-request! @~a{/build/@build-id})]
   [_           (fail-if (not (hash? build-info)) @~a{Error: @build-info})]

   [jobs        (hash-ref/option build-info 'jobs "No jobs?")]
   [a-job       (first/option    jobs "No jobs?")]
   [job-id      (hash-ref/option a-job      'id)]
   [log-text    (travis-request! @~a{/job/@|job-id|/log})]

   log-text))

(define (travis:get-job-status! job-id)
  (option-let*
   [(list repo-id request-id) job-id]
   [status-info (travis-request! @~a{/repo/@|repo-id|/request/@|request-id|})]
   [_           (fail-if (not (hash? status-info)) @~a{Error: @status-info})]

   [builds      (hash-ref/option status-info 'builds "No builds yet")]
   [a-build     (first/option    builds              "No builds yet")]
   [build-state (hash-ref/option a-build     'state)]

   build-state))
