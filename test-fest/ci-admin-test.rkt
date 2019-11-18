#lang at-exp racket

(require racket/cmdline
         racket/runtime-path
         json
         "cmdline.rkt"
         "test-fest-data.rkt"
         "util.rkt"
         "test.rkt"
         "logger.rkt"
         "process.rkt")

(define-runtime-path oracle-remote-player-path
  "../distribute/8/8.1/random-remote-player.rkt")
(define-runtime-path oracle-admin-path
  "../distribute/8/8.1/admin.rkt")

(define ITERATION-TIMEOUT-SECONDS (* 10 60))

(define PORTS '(8080 8081 8082))

(define (fail! msg)
  (eprintf "~a~n" msg)
  (exit 1))

(define (check-path! p)
  (unless (file-exists? p)
    (fail! @~a{Error: @p doesn't exist})))

(define (config-hash? h)
  (match h
    [(hash-table ['IP "localhost"]
                 ['port (? number?)]
                 ['default-player (? string?)])
     #t]
    [else #f]))

(define/contract (read/check-config-validity! config-path)
  (path-to-existant-file? . -> . config-hash?)

  (define config (call-with-input-file config-path
                   read-json/safe))
  (when (eq? config bad-json)
    (fail! @~a{Error: config is not valid json}))
  (when (not (config-hash? config))
    (fail! @~a{Error: config @config-path has invalid format: @config}))
  config)

(struct actor (path run-with-racket? run-in) #:transparent)
(define actor/c (struct/c actor
                          (and/c path-to-existant-file? absolute-path?)
                          boolean?
                          path-to-existant-directory?))

;; Checks that NEITHER `admin` NOR `player` crash on `config`
(define/contract (run-game! admin player config-path config)
  (actor/c
   actor/c
   path-to-existant-file?
   config-hash?
   . -> .
   any)

  (log-fest info @~a{
                     Running game
                     with admin @(pretty-path (actor-path admin))
                     and remote player @(pretty-path (actor-path player))
                     and config @config

                     })

  (write-config! config config-path)

  (match-define (list wait-for-admin-result wait-for-player-result)
    (for/list ([an-actor (in-list (list admin player))])
      (define stdout-temp-file (make-temporary-file))
      (define stdout (open-output-file stdout-temp-file
                                       #:exists 'truncate))
      (define-values {proc _}
        (launch-process! (actor-path an-actor)
                         #:stdout stdout
                         #:run-with-racket? (actor-run-with-racket? an-actor)
                         #:run-in (actor-run-in an-actor)))

      (define (wait&cleanup)
        (define result (watch-for-failure proc))
        (subprocess-kill proc #t)
        (close-output-port stdout)
        (unless result
          (log-fest warning
                    @~a{@(pretty-path (actor-path an-actor)) crashed! (non-zero exit code)})
          (log-fest warning
                    @~a{Output: @~v[(file->string stdout-temp-file)]}))
        (delete-file stdout-temp-file)
        result)

      wait&cleanup))

  (and (wait-for-admin-result)
       (wait-for-player-result)))

(define/contract (write-config! config path)
  (config-hash? path-to-existant-file? . -> . any)

  (call-with-output-file path
    #:exists 'truncate
    (λ (out) (write-json config out))))

(define/contract (watch-for-failure proc)
  (subprocess? . -> . boolean?)

  (and (wait/keep-ci-alive proc ITERATION-TIMEOUT-SECONDS)
       (equal? (subprocess-status proc) 0)))

(module+ main
  (match-define (cons flags args)
    (command-line/declarative
     #:once-each
     [("-M" "--Major")
      'major
      "Assignment major number. E.g. for 5.2 this is 5."
      #:mandatory
      #:collect ["number" take-latest #f]]
     [("-m" "--minor")
      'minor
      "Assignment minor number. E.g. for 5.2 this is 2."
      #:mandatory
      #:collect ["number" take-latest #f]]
     [("-a" "--test-admin")
      'admin-path
      "Path to admin executable to test."
      #:mandatory
      #:collect ["path" take-latest #f]]
     [("-r" "--repo-root")
      'repo-path
      "Path to root of submission repo"
      #:collect ["path" take-latest "."]]))

  (define assign-number (cons (hash-ref flags 'major)
                              (hash-ref flags 'minor)))
  (define assign-dir
    (build-path-string (hash-ref flags 'repo-path)
                       "Deliverables"
                       (assign-number->dir-path assign-number)))

  (define admin-path (hash-ref flags 'admin-path))
  (define config-path (build-path-string assign-dir "go.config"))

  (check-path! admin-path)
  (check-path! config-path)
  (define config (read/check-config-validity! config-path))

  (define admin (actor (simple-form-path-string admin-path)
                       #f
                       assign-dir))
  (define oracle-player (actor oracle-remote-player-path
                               #t
                               assign-dir))

  (for ([port (in-list PORTS)])
    (unless (run-game! admin
                       oracle-player
                       config-path
                       (hash-set config 'port port))
      (fail! @~a{
                 Abnormal exit for game
                 with admin @(pretty-path admin-path)
                 and remote player @(pretty-path oracle-remote-player-path)
                 on port @port
                 }))))
