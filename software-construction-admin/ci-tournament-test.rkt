#lang at-exp racket

(require racket/runtime-path
         json
         "cmdline.rkt"
         "test-fest-data.rkt"
         "util.rkt"
         "testing.rkt"
         "logger.rkt"
         "process.rkt"
         "assignments.rkt")

(define-runtime-path oracle-remote-player-path
  "../distribute/8/8.1/random-remote-player.rkt")

(define ITERATION-TIMEOUT-SECONDS (* 10 60))
(define REMOTE-PLAYER-COUNT 3)

(define PORT 8080)

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

;; Checks that `tournament` doesn't crash
(define/contract (run-game! tournament
                            player
                            config-path
                            config
                            tournament-type
                            remote-player-count)
  (actor/c
   actor/c
   path-to-existant-file?
   config-hash?
   (or/c 'league 'cup)
   natural?
   . -> .
   (or/c false? (not/c false?)))

  (log-fest-info @~a{
                     Running game
                     with tournament @(pretty-path (actor-path tournament))
                     and remote player @(pretty-path (actor-path player))
                     and config @config
                     of type @tournament-type

                     })

  (write-config! config config-path)

  (match-define (cons wait-for-tournament-result
                      wait-for-player-results)
    (for/list ([an-actor (in-list
                          (list* tournament
                                 (build-list remote-player-count
                                             (λ _ (struct-copy actor
                                                               player)))))])
      (define stdout-temp-file @~a{./temp-stdout-@(eq-hash-code an-actor)})
      (log-fest-info @~a{Sending output to @stdout-temp-file})
      (define stdout (open-output-file stdout-temp-file
                                       #:exists 'truncate))
      (define-values {proc _}
        (launch-process! (actor-path an-actor)
                         (if (eq? an-actor tournament)
                             (list @~a{--@tournament-type}
                                   (~a remote-player-count))
                             empty)
                         #:stdout stdout
                         #:run-with-racket? (actor-run-with-racket? an-actor)
                         #:run-in (actor-run-in an-actor)))

      (define (wait&cleanup)
        (define result (watch-for-failure proc))
        (subprocess-kill proc #t)
        (close-output-port stdout)

        (unless result
          (log-fest-error @~a{@(pretty-path (actor-path an-actor)) crashed! (non-zero exit code)}))
        (log-fest-info @~a{
                           @(pretty-path (actor-path an-actor)) output:
                           =====
                           @(file->string stdout-temp-file)
                           =====

                           })

        (delete-file stdout-temp-file)
        result)

      ;; Hack to avoid race condition of player crashing bc tournament not listening
      (sleep 10)

      wait&cleanup))

  (define player-results
    ;; put into list and andmap laterZ instead of for/and to force every player
    ;; to show output
    (for/list ([wait (in-list wait-for-player-results)])
      (wait)))
  (define tournament-results (wait-for-tournament-result))
  (and (andmap identity
               player-results)
       tournament-results))

(define/contract (write-config! config path)
  (config-hash? path-to-existant-file? . -> . any)

  (call-with-output-file path
    #:exists 'truncate
    (λ (out) (write-json config out))))

(define/contract (watch-for-failure proc)
  (subprocess? . -> . boolean?)

  (and (sync/timeout ITERATION-TIMEOUT-SECONDS proc)
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
     [("-t" "--test-tournament")
      'tournament-path
      "Path to tournament executable to test."
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
                       (assign-number->dir-path-part assign-number)))

  (define tournament-path (hash-ref flags 'tournament-path))
  (define config-path (build-path-string assign-dir "go.config"))

  (check-path! tournament-path)
  (check-path! config-path)
  (define config (read/check-config-validity! config-path))

  (define tournament (actor (simple-form-path-string tournament-path)
                            #f
                            assign-dir))
  (define oracle-player (actor oracle-remote-player-path
                               #t
                               assign-dir))

  (for ([tournament-type (in-list '(league cup))])
    (unless (run-game! tournament
                       oracle-player
                       config-path
                       (hash-set config 'port PORT)
                       tournament-type
                       REMOTE-PLAYER-COUNT)
      (fail! @~a{
                 Abnormal exit
                 with tournament @(pretty-path tournament-path)
                 and remote player @(pretty-path oracle-remote-player-path)
                 on port @PORT
                 of type @tournament-type
                 with @REMOTE-PLAYER-COUNT remote players
                 }))))
