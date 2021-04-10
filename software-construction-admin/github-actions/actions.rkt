#lang at-exp racket

(provide get-all-runs!
         get-runs-by-url!
         cancel-run!
         launch-run!
         get-run-log!
         (struct-out ci-run)

         install-workflow-config!
         write-workflow-env!)

(require "github-api.rkt"
         "../common/option.rkt"
         "../common/git.rkt"
         "../common/util.rkt"
         "../config.rkt"
         gregor
         file/unzip)

(struct ci-run (id
                url
                html-url
                commit
                status
                conclusion
                creation-time
                log-url
                cancel-url)
  #:prefab)

;; lltodo: this doesn't deal with pagination
(define/contract (get-all-runs! repo-owner repo-name)
  (string? string? . -> . (option/c (listof ci-run?)))

  (option-let*
   [run-info (github-request! (~a "repos/" repo-owner "/" repo-name "/actions/runs"))]
   [runs (hash-ref/option run-info
                          'workflow_runs
                          @~a{Failed to get runs: @run-info})]
   (map json->ci-run runs)))

(define/contract (get-runs-by-url! repo-owner repo-name urls)
  (string? string? (listof string?) . -> . (option/c (hash/c string? (option/c ci-run?))))

  ;; Alternative version that (probably) more efficiently uses api calls, but
  ;; needs to deal with pagination
  #;(option-let*
   [all-runs (get-all-runs! repo-owner repo-name)]
   [runs-by-url (for/hash ([run (in-list all-runs)])
                  (values (ci-run-url run) run))]
   (for/hash ([url (in-list urls)])
     (values url
             (hash-ref/option runs-by-url url @~a{No run found with url @url}))))
  (for/hash ([url (in-list urls)])
    (values url (get-run-by-url! url))))

(define/contract (get-run-by-url! url)
  (string? . -> . (option/c ci-run?))

  (option-let*
   #:extra-failure-message (~a url " : ")
   [run-json (github-request! url)]
   (json->ci-run run-json)))

(define (failed-to action code headers in)
  (failure
   @~a{
       Failed to @action, response code @code
       Message: @(try-read-body-string code headers in)
       }))

(define (json->ci-run run-info-json)
  (match run-info-json
    [(hash-table ['id id]
                 ['url url]
                 ['html_url html-url]
                 ['head_sha commit]
                 ['status status]
                 ['conclusion conclusion]
                 ['created_at creation-time]
                 ['logs_url log-url]
                 ['cancel_url cancel-url]
                 _ ...)
     (ci-run id
             url
             html-url
             commit
             status
             conclusion
             (iso8601->moment creation-time)
             log-url
             cancel-url)]
    [else (failure "Unexpected run info shape in api response")]))

(define/contract (cancel-run! a-run)
  (ci-run? . -> . boolean?)

  (github-request! (ci-run-cancel-url a-run)
                   #:method POST
                   #:read-response (λ (code headers in) (equal? code 202))))

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

  (define (ci-run-newer? run1 run2)
    (moment>? (ci-run-creation-time run1)
              (ci-run-creation-time run2)))
  (define (newest-job-in jobs)
    (first (sort jobs ci-run-newer?)))
  (define (wait/poll-for-new-job! original-jobs)
    (define original-latest-job (newest-job-in original-jobs))
    (let loop ([retry-count 0])
      (match (get-all-runs! repo-owner repo-name)
        [(present (app newest-job-in latest-job))
         #:when (ci-run-newer? latest-job original-latest-job)
         (present latest-job)]
        [else
         #:when (< (* retry-count run-retrieval-polling-period-seconds)
                   run-retrieval-polling-timeout-seconds)
         (sleep run-retrieval-polling-period-seconds)
         (loop (add1 retry-count))]
        [else (failure @~a{
                           Couldn't find launched job after polling for @;
                           @|run-retrieval-polling-timeout-seconds|s
                           })])))

  (option-let*
   [jobs-before-launch (get-all-runs! repo-owner repo-name)]
   [_ (github-request! (~a "repos/" repo-owner "/" repo-name
                           "/actions/workflows/" workflow-id "/dispatches")
                       #:method POST
                       #:data (string->bytes/utf-8 @~a{{"ref": "@ref"}})
                       #:read-response (match-lambda**
                                        [{204 _ _} (present 'ok)]
                                        [{other headers in}
                                         (failed-to "launch run"
                                                    other
                                                    headers
                                                    in)]))]
   [the-run (wait/poll-for-new-job! jobs-before-launch)]
   the-run))

(define/contract wget
  path-to-existant-file?
  (find-executable-path "wget"))

(define/contract (get-run-log! a-run #:section [section-name #f])
  ({ci-run?}
   {#:section (or/c string? #f)}
   . ->* .
   (option/c string?))

  (define (url->log-contents log-download-url)
    (match-define (list stdout stdin _ stderr ctl)
      (process* wget
                "-O"
                "-"
                log-download-url))
    (close-output-port stdin)

    (define log-sections (box empty))
    (unzip
     stdout
     (match-lambda**
      [{(regexp #rx"^.*/([0-9]+)_(.+).txt$" (list _
                                                  section-number-bytes
                                                  section-name-bytes))
        #f
        contents-port}
       #:when (or (not section-name)
                  (string-ci=? (bytes->string/utf-8 section-name-bytes) section-name))
       ;; Sections aren't necessarily unpacked in order, so hang on to the section number
       ;; to order them later
       (define section-number (string->number (bytes->string/utf-8 section-number-bytes)))
       (set-box! log-sections
                 (cons (list section-number (port->string contents-port))
                       (unbox log-sections)))]
      [{_ _ _} (void)]))
    (ctl 'kill)
    (close-input-port stdout)
    (close-input-port stderr)

    (match (unbox log-sections)
      ['() (failure "Failed to get log using download url")]
      [sections
       (present (string-join (map second (sort sections < #:key first))
                             "\n"))]))

  (option-let*
   [log-download-url (github-request!
                      (ci-run-log-url a-run)
                      #:read-response (match-lambda**
                                       [{302 (regexp #rx"(?mi:^location: (.+?)$)" (list _ url)) _}
                                        url]
                                       [{302 headers in}
                                        (failed-to @~a{
                                                       parse log download url in headers:
                                                       @headers
                                                       }
                                                   302
                                                   headers
                                                   in)]
                                       [{other headers in}
                                        (failed-to "get log download url"
                                                   other
                                                   headers
                                                   in)]))]
   [contents (url->log-contents log-download-url)]
   contents))

(define/contract (install-workflow-config! repo-path
                                           name
                                           steps)
  (path-to-existant-directory? string? dict? . -> . path-to-existant-file?)

  (define workflows-path (build-path repo-path
                                     ".github"
                                     "workflows"))
  (make-directory* workflows-path)
  (define config-path (build-path workflows-path
                                  (~a name ".yml")))
  (display-to-file (make-workflow-config-contents name steps)
                   config-path
                   #:exists 'replace)
  config-path)

(define workflow-env-file "env.sh")
(define/contract (write-workflow-env! repo-path env)
  (path-to-existant-directory? dict? . -> . path-to-existant-file?)

  (define env-path (build-path repo-path workflow-env-file))
  (display-to-file (string-join (for/list ([{var value} (in-dict env)])
                                  @~a{@|var|=@|value|})
                                "\n")
                   env-path
                   #:exists 'truncate)
  env-path)

(define (make-workflow-config-contents name steps)
  @~a{
      on:
        - workflow_dispatch

      jobs:
        @|name|:
          runs-on: ubuntu-20.04
          steps:
            - name: Checkout
              uses: actions/checkout@"@"main
            - name: Install Racket
              uses: Bogdanp/setup-racket@"@"v0.11
              with:
                architecture: 'x64'
                distribution: 'full'
                version: '8.0'
            - name: Set up environment
              run: cat @workflow-env-file >> $GITHUB_ENV
            - name: Install oracle
              run: raco pkg install --auto https://github.com/@|oracle-repo-owner|/@|oracle-repo-name|.git
      @(string-join
        (for/list ([{step-name step-cmd} (in-dict steps)])
          @~a{
              @"      "- name: @step-name
              @"      "  run: @step-cmd
              })
        "\n")
      })
