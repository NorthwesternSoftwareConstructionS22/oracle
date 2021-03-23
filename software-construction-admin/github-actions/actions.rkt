#lang at-exp racket

(provide get-all-runs!
         cancel-run!
         ;; restart-run!
         ;; get-run-log!
         launch-run!
         )

(require "github-api.rkt"
         "../common/option.rkt"
         "../common/git.rkt"
         gregor
         file/unzip)

(struct ci-run (id
                url
                commit
                status
                conclusion
                creation-time
                log-url
                cancel-url)
  #:prefab)

(define (get-all-runs! repo-owner repo-name)
  (option-let*
   [run-info (github-request! (~a "repos/" repo-owner "/" repo-name "/actions/runs"))]
   [runs (hash-ref/option run-info
                          'workflow_runs
                          @~a{Failed to get runs: @run-info})]
   (map json->ci-run runs)))

(define (json->ci-run run-info-json)
  (match run-info-json
    [(hash-table ['id id]
                 ['url url]
                 ['head_sha commit]
                 ['status status]
                 ['conclusion conclusion]
                 ['created_at creation-time]
                 ['logs_url log-url]
                 ['cancel_url cancel-url]
                 _ ...)
     (ci-run id
             url
             commit
             status
             conclusion
             (iso8601->moment creation-time)
             log-url
             cancel-url)]
    [else (failure "Unexpected run info shape in api response")]))

(define/contract (cancel-run! a-run)
  (ci-run? . -> . any)

  (github-request! (ci-run-cancel-url a-run)
                   #:method POST
                   #:read-response void))

;; lltodo: wiw:
;; - Modify github-request! to
;;   1. Return the status code as well as calling read-response
;;   2. Only call read-response if the content-length is not 0, otherwise produce another arg value
;; - Play with the etag header on listing runs to see if it will only return if a there's a new run or what. It might also return if any runs status changes?
;;   If it only returns on new run/status change, we can poll without needing to worry too much I think.
;;   + Checked out etag and it doesn't work like that: it will give a new thing pretty much every time when there's an actively running job


;; idea: have this function launch a sequence of runs, and then another function that can return the launches for those sequences. The second function will just do some slow polling (once per 5sec) until it has all the runs.

(define branch-name? string?)

(define run-retrieval-polling-period-seconds 5)
(define run-retrieval-polling-timeout-seconds (* 1 60))
(define/contract (launch-run! repo-owner repo-name workflow-id ref)
  (string?
   string?
   string?
   (or/c sha? branch-name?)
   . -> .
   (option/c ci-run?))

  (define (wait/poll-for-new-job! original-jobs)
    (define original-count (length original-jobs))
    (let loop ([retry-count 0])
      (match (get-all-runs! repo-owner repo-name)
        [(present current-jobs)
         #:when (> (length current-jobs) original-count)
         (set-first (set-subtract current-jobs original-jobs))]
        [else
         #:when (< (* retry-count run-retrieval-polling-period-seconds)
                   run-retrieval-polling-timeout-seconds)
         (sleep run-retrieval-polling-period-seconds)
         (loop (add1 retry-count))]
        [else (failure "Couldn't find launched job")])))

  (option-let*
   [jobs-before-launch (get-all-runs! repo-owner repo-name)]
   [_ (github-request! (~a "repos/" repo-owner "/" repo-name
                           "/actions/workflows/" workflow-id "/dispatches")
                       #:method POST
                       #:data (string->bytes/utf-8 @~a{{"ref": "@ref"}})
                       #:read-response
                       (match-lambda** [{204 _}    (present 'ok)]
                                       [{other in}
                                        (failure (if (sync/timeout 0 in)
                                                     (failure (~a other ": " (port->string in)))
                                                     (failure (~a "response code " other))))]))]
   [the-run (wait/poll-for-new-job! jobs-before-launch)]
   the-run))

#;(define (get-run-log! a-run)
    use `unzip` here, do a little experimenting with it)
