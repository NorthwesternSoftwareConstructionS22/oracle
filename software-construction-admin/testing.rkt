#lang at-exp racket

(provide valid-tests
         valid-tests/passing-oracle
         test-failures-for
         log-test-failure-comparison?)
(module+ test (require rackunit))

(require json
         "tests.rkt"
         "config.rkt"
         "common/util.rkt"
         "common/logger.rkt"
         "common/process.rkt"
         (for-syntax syntax/parse))

;; The CI kills any job running longer than 115 min
(define absolute-max-timeout-seconds (* 115 60))
(define test-log-messages-delimiter "\n")

;; Values being compared may be large, so they can be suppressed to prevent
;; filling up the log when other `info`-level information is desired
(define log-test-failure-comparison? (make-parameter #t))

(define/contract (run-exe-on-input exe-path
                                   input-json
                                   [timeout-seconds absolute-max-timeout-seconds]
                                   #:munge-json-style [munge-json-style #f])
  (->* (path-to-existant-file?
        (or/c path-to-existant-file? input-port?))
       (natural?
        #:munge-json-style (or/c 'pad 'dribble #f))
       (or/c bytes? #f))

  (when munge-json-style
    (log-fest-debug @~a{@(pretty-path exe-path) using @munge-json-style munging style}))

  (define input-json-port (if (input-port? input-json) input-json (open-input-file input-json)))
  (define stdin (add-munging input-json-port munge-json-style))
  (define-values {proc stdout stderr}
    ;; ll: Can't use pipe here because that's not a `file-stream-port?`
    (launch-process! exe-path
                     #:stdin stdin
                     #:stdout #f
                     #:stderr #f
                     #:limit-stdout? #t
                     #:limit-stderr? #t))

  (unless munge-json-style
    (unless (input-port? input-json) (close-input-port input-json-port))) ;; Make sure process gets eof

  (define terminated? (sync/timeout timeout-seconds proc))

  (subprocess-kill proc #t) ;; Ensure the process is dead

  (log-fest-debug @~a{@(pretty-path exe-path) done.})
  (log-fest-debug @~a{Closing exe ports})

  (log-fest-debug @~a{Reading exe output})
  (define stderr-bytes (port->bytes stderr))
  (define stderr-empty? (equal? stderr-bytes #""))
  (define stdout-bytes
    (cond [(not stderr-empty?)
           (log-fest-error
            @~a{
                @(pretty-path exe-path) produced output on stderr:
                ------------------------------
                @(try-decode-bytes->string stderr-bytes)
                ------------------------------
                })
           #f]
          [terminated?
           (port->bytes stdout)]
          [else
           (log-fest-error
            @~a{@(pretty-path exe-path) timed out (@|timeout-seconds|s)})
           #f]))
  (close-input-port stdout)
  (close-input-port stderr)
  stdout-bytes)

(define/contract (add-munging port munging-style)
  (-> input-port?
      (or/c 'pad 'dribble #f)
      input-port?)
  (cond
    [(not munging-style) port]
    [else
     (define-values (in out) (make-pipe))
     (thread
      (λ ()
        (case munging-style
          [(pad)
           (define json (read-json port))
           (close-input-port port)
           (send-padded-json json out)
           (close-output-port out)]
          [(dribble)
           (let loop ([n 0])
             (define b (read-byte port))
             (cond
               [(eof-object? b)
                (close-input-port port)
                (close-output-port out)]
               [else
                (write-byte b out)
                (case (modulo n 4)
                  [(0) (void)]
                  [(1 2) (sleep 0.002)]
                  [(1 2) (sleep 0.01)])
                (loop (+ n 1))]))])))
     in]))

(define bolus-bytes (make-bytes 5001 (char->integer #\space)))
(define (send-padded-json json port)
  (define (bolus) (write-bytes bolus-bytes port))

  (bolus)
  (cond
    [(list? json)
     (write-bytes #"[" port)
     (bolus)
     (for ([item (in-list json)]
           [i (in-naturals)])
       (unless (zero? i)
         (write-bytes #"," port)
         (bolus))
       (write-json item port))
     (write-bytes #"]" port)
     (bolus)]
    [(hash? json)
     (write-bytes #"{" port)
     (bolus)
     (for ([key (in-list (sort (hash-keys json) symbol<?))]
           [i (in-naturals)])
       (unless (zero? i)
         (write-bytes #"," port)
         (bolus))
       (write-json (symbol->string key) port)
       (bolus)
       (write-bytes #":" port)
       (bolus)
       (write-json (hash-ref json key) port)
       (bolus))
     (write-bytes #"}" port)
     (bolus)]
    [else
     (write-json json port)
     (bolus)]))

(module+ test
  (let ()
    (define-values (in out) (make-pipe))
    (write-json #t out)
    (close-output-port out)
    (define new-val (read-json (add-munging in 'pad)))
    (check-equal? new-val #t))

  (let ()
    (define-values (in out) (make-pipe))
    (write-json "okay" out)
    (close-output-port out)
    (define new-val (read-json (add-munging in 'pad)))
    (check-equal? new-val "okay"))

  (let ()
    (define-values (in out) (make-pipe))
    (write-json (list 1 2 3) out)
    (close-output-port out)
    (define new-val (read-json (add-munging in 'pad)))
    (check-equal? new-val (list 1 2 3)))

  (let ()
    (define-values (in out) (make-pipe))
    (write-json (hasheq 'x 3 'y 4) out)
    (close-output-port out)
    (define new-val (read-json (add-munging in 'pad)))
    (check-equal? new-val (hasheq 'x 3 'y 4)))

  (let ()
    (define-values (in out) (make-pipe))
    (write-json (hasheq 'x 3 'y 4) out)
    (close-output-port out)
    (define new-val (read-json (add-munging in 'dribble)))
    (check-equal? new-val (hasheq 'x 3 'y 4))))

;; todo: we will crash before we get here when there aren't any test
;; cases but this shouldn't be an error for some of the assignments,
;; as they don't have any test case requirement (but they will have
;; an oracle)
(define/contract (exe-passes-test?/racket-oracle exe-path oracle-path the-test)
  (-> path-to-existant-file?
      path-to-existant-file?
      test/c
      boolean?)

  (define input-file (test-input-file the-test))
  (log-fest-info @~a{Running @(pretty-path exe-path) on test @(test-display-name input-file) ...})

  (define the-oracle (fetch-racket-based-oracle oracle-path))

  (define copy-of-stderr (open-output-string))
  (define cust (make-custodian))
  (define passed?
    (parameterize ([current-custodian cust])
      (let/ec escape
        (define (test-failed)
          (custodian-shutdown-all cust)
          (escape #f))
        (with-handlers ([exn:fail? (λ (x)
                                     (custodian-shutdown-all cust)
                                     (define sp (open-output-string))
                                     (parameterize ([current-error-port sp])
                                       ((error-display-handler)
                                        (exn-message x)
                                        x))
                                     (log-fest-error @~a{An error occurred while running the test @(test-display-name input-file):
                          ------------------------------
                          @(get-output-string sp)
                          ------------------------------
                          })
                                     (test-failed))])

          (define-values (stdin-pipe-in stdin-pipe-out) (make-pipe))

          (define-values {proc stdout stderr}
            (launch-process! exe-path
                             #:stdin stdin-pipe-in
                             #:stdout #f
                             #:stderr #f
                             #:limit-stdout? #t
                             #:limit-stderr? #t))
          (define stderr-piping-thread
            (thread (λ () (copy-port stderr copy-of-stderr) (close-output-port copy-of-stderr))))

          (define (give-up-due-to-json-problem)
            (sync stderr-piping-thread)
            (test-failed))
          (define passed? (the-oracle stdout stdin-pipe-out
                                      (and input-file (file->bytes input-file))
                                      (make-send-json exe-path input-file give-up-due-to-json-problem)
                                      (make-recv-json exe-path input-file give-up-due-to-json-problem)))
          (custodian-shutdown-all cust)
          passed?))))
  (define stderr (get-output-string copy-of-stderr))
  (cond
    [(string=? "" stderr)
     (log-fest-info @~a{
                        Test @(if passed? "passed" "failed").
                        @test-log-messages-delimiter
                        })
     passed?]
    [else
     (log-fest-error @~a{
                         Test failed because stderr of the submission was not empty:
                         ------------------------------
                         @stderr
                         ------------------------------
                         @test-log-messages-delimiter
                         })
     #f]))


(define (make-send-json exe-path input-file test-failed)
  (define (send-json out json where #:communication-style [style 'normal])
    (with-handlers ([exn:fail? (λ (x)
                                 (log-fest-error
                                  @~a{
                                      @(pretty-path exe-path) fails test @(test-display-name input-file) @;
                                      because the following JSON message could not be sent
                                      ------------------------------
                                      @(jsexpr->bytes json)
                                      ------------------------------
                                      })
                                 (raise x))])
      (log-fest-debug @~a{Sending to @where
 ------------------------------
 @(jsexpr->bytes json)
 ------------------------------
 })
      (with-timeout (send-a-json-maybe-messing-with-it json out style) (format "json to be sent ~a" where))
      (newline out)
      (flush-output out)))

  send-json)

(define (send-a-json-maybe-messing-with-it json out style)
  (match style
    ['normal (write-json json out)]
    ['pad
     (send-padded-json json out)]
    ['dribble
     (define-values (pipe-in pipe-out) (make-pipe))
     (thread (λ ()
               (write-json json pipe-out)
               (close-output-port pipe-out)))
     (define start-time (current-seconds))
     (let loop ([n 0])
       (define b (read-byte pipe-in))
       (unless (eof-object? b)
         (write-byte b out)
         (case (modulo n 3)
           [(0) (sleep)]
           [(1) (sleep 0.001)]
           [(2) (void)])
         (loop (+ n 1))))]))

(define (make-recv-json exe-path input-file test-failed)
  (define (recv-json in ctc where
                     #:timeout-seconds [timeout-seconds timeout-seconds]
                     #:allow-newlines? [allow-newlines? #f]
                     #:allow-eof? [allow-eof? #f])
    (define val
      (cond
        [allow-newlines?
         (if timeout-seconds
             (with-timeout
                 (read-json in)
               (format "json ~afrom ~a"
                       (if allow-eof? "or eof " "")
                       where)
               #:timeout-seconds timeout-seconds)
             (read-json in))]
        [else
         (define-values (the-bytes timed-out?) (read-line/timeout in timeout-seconds))
         (when timed-out?
           (raise-user-error
            'with-timeout
            (string-append
             "timed out after ~a seconds waiting for ~a\n"
             "  got: ~s\n"
             "      (the previous line shows the bytes received in a quoted form;\n"
             "       the leading # and \" are racket's notation for bytes and the\n"
             "       data inside is shown, with quotes escaped and non-ASCII shown\n"
             "       in octal)")
            timeout-seconds
            (format "json ~afrom ~a"
                    (if allow-eof? "or eof " "")
                    where)
            the-bytes))
         (read-json (open-input-bytes the-bytes))]))
    (when (and (eof-object? val) (not allow-eof?))
      (log-fest-error
       @~a{
           @(pretty-path exe-path) fails test @(test-display-name input-file) @;
           because it didn't send a JSON object, got eof @where
           })
      (test-failed))
    (if (eof-object? val)
        (log-fest-debug @~a{
 Received eof from @where
 })
        (log-fest-debug @~a{
 Received from @where
 ------------------------------
 @(jsexpr->bytes val)
 ------------------------------
 }))
    (unless (or (eof-object? val)
                ((flat-contract-predicate ctc) val))
      (log-fest-error
       @~a{
           @(pretty-path exe-path) fails test @(test-display-name input-file) @;
           because its JSON result did not have the right shape:
           ------------------------------
           @(jsexpr->bytes val)
           ------------------------------
           })
      (test-failed))
    val)
  recv-json)

(define timeout-seconds 5)

(define-syntax (with-timeout stx)
  (syntax-parse stx
    [(_ e:expr what:expr)
     #'(with-timeout/proc (λ () e) (λ () what))]
    [(_ e:expr what:expr #:timeout-seconds timeout-seconds)
     #'(with-timeout/proc (λ () e) (λ () what) timeout-seconds)]))
(define (with-timeout/proc thunk what-thunk [timeout-seconds timeout-seconds])
  (define chan (make-channel))
  (thread
   (λ ()
     (with-handlers ([exn:fail? (λ (exn) (channel-put chan exn))])
       (channel-put chan (vector (thunk))))))
  (define result (sync/timeout timeout-seconds chan))
  (unless result
    (raise-user-error 'with-timeout "timed out after ~a seconds waiting for ~a"
                      timeout-seconds
                      (what-thunk)))
  (when (exn:fail? result) (raise result))
  (vector-ref result 0))


;; returns the data it got and a boolean saying if it timed out (or not)
(define (read-line/timeout in [timeout-seconds timeout-seconds])
  (define too-long (and timeout-seconds
                        (+ (current-inexact-milliseconds) (* timeout-seconds 1000))))
  (define b (make-bytes 1))
  (define read-bytes (make-bytes 1))
  (define total-read 0)
  (define (add-a-byte b)
    (unless (< total-read (bytes-length read-bytes))
      (define nb (make-bytes (* 2 (bytes-length read-bytes))))
      (bytes-copy! nb 0 read-bytes 0)
      (set! read-bytes nb))
    (bytes-set! read-bytes total-read b)
    (set! total-read (+ total-read 1)))
  (define (fetch-result) (subbytes read-bytes 0 total-read))
  (let loop ()
    (match (sync
            (if too-long
                (handle-evt (alarm-evt too-long) (λ (_) #f))
                never-evt)
            (read-bytes!-evt b in))
      [#f (values (fetch-result)
                  #t)]
      [1
       (cond
         [(equal? b #"\n")
          (values (fetch-result)
                  #f)]
         [else
          (add-a-byte (bytes-ref b 0))
          (loop)])]
      [(? eof-object?)
       (values (fetch-result)
               #f)])))
(module+ test
  (define (wrap-read-line/timeout the-bytes)
    (define-values (result-bytes timed-out?)
      (read-line/timeout (open-input-bytes the-bytes)))
    (list result-bytes timed-out?))
  (check-equal?
   (wrap-read-line/timeout #"")
   (list #"" #f))
  (check-equal?
   (wrap-read-line/timeout #"\n")
   (list #"" #f))
  (check-equal?
   (wrap-read-line/timeout #"\na")
   (list #"" #f))
  (check-equal?
   (wrap-read-line/timeout #"a\n")
   (list #"a" #f))
  (check-equal?
   (wrap-read-line/timeout #"ab\n")
   (list #"ab" #f))
  (check-equal?
   (wrap-read-line/timeout #"abc\n")
   (list #"abc" #f))
  (check-equal?
   (wrap-read-line/timeout #"abcd\n")
   (list #"abcd" #f))
  (check-equal?
   (wrap-read-line/timeout #"abcde\n")
   (list #"abcde" #f))
  (check-equal?
   (wrap-read-line/timeout #"a b c\n")
   (list #"a b c" #f))

  (let ()
    (define-values (in out) (make-pipe))
    (write-bytes #"ab" out)
    (define-values (the-bytes timed-out?) (read-line/timeout in 0.5))
    (check-equal? #"ab" the-bytes)
    (check-true timed-out?)))
  
(define (fetch-racket-based-oracle oracle-path)
  (contract
   (->i ([stdout input-port?]  ;; stdout of user program
         [stdin output-port?]  ;; stdin of user program
         [test-case bytes?]    ;; one test case

         ;; if something goes wrong with either of the
         ;; communication functions, they just don't return.
         ;; sending can go wrong for various technical
         ;; reasons (if the network communication was shut
         ;; down for example) and receiving can go wrong for
         ;; those but also because the result wasn't json
         ;; or wasn't valid according to the contract

         ;; a function to just send some json out, the string
         ;; goes into the students debug log, along with the JSON
         [send-json (->* (output-port? jsexpr? string?)
                         (#:communication-style (or/c 'normal 'dribble 'pad))
                         void?)]

         ;; a function to receive some json in, the string
         ;; goes into the students debug log, along with the JSON
         [recv-json (->* (input-port? flat-contract? string?)
                         (#:allow-newlines? boolean?
                          #:allow-eof? boolean?
                          #:timeout-seconds (or/c #f natural?))
                         (or/c jsexpr? eof-object?))])

        ;; indicates if something went wrong; if it did, there were
        ;; expected to be log messages that explain what happened
        [result boolean?])
   (dynamic-require oracle-path 'oracle)
   (~a oracle-path)
   "...-admin/testing.rkt"))

(define (bytes->json/safe b)
  (call-with-input-bytes b read-json/safe))

(define/contract (exe-passes-test? exe-path oracle-path t #:oracle-needs-student-output? oracle-needs-student-output?
                                   #:munge-json-style [munge-json-style #f])
  (->* (path-to-existant-file?
        path-to-existant-file?
        test/c
        #:oracle-needs-student-output? boolean?)
       (#:munge-json-style (or/c 'pad 'dribble #f))
       boolean?)

  (define input-file (test-input-file t))

  (log-fest-info @~a{Running @(pretty-path exe-path) on test @(test-display-name input-file) ...})
  (define exe-output-bytes (run-exe-on-input exe-path input-file submission-timeout-seconds
                                             #:munge-json-style munge-json-style))
  (when (bytes? exe-output-bytes)
    (log-fest-debug @~a{
                        The raw output of @(pretty-path exe-path) was:
                        ------------------------------
                        @(try-decode-bytes->string exe-output-bytes)
                        ------------------------------
                        }))
  (match exe-output-bytes
    [(? bytes? (app bytes->json/safe (and exe-output-json
                                          (not (== bad-json)))))
     (log-fest-debug @~a{Running the oracle on test @(test-display-name input-file) ...})
     (define oracle-input
       (if oracle-needs-student-output?
           (input-port-append #t
                              ;; this file gets closed because
                              ;; the call to copy-port in launch-process!
                              ;; will copy all of the data out of this
                              ;; port (which'll trigger the close
                              ;; via input-port-append)
                              (open-input-file input-file)
                              (open-input-bytes #"\n")
                              (open-input-bytes (jsexpr->bytes exe-output-json)))
           input-file))
     (match-define-values {(list oracle-output-bytes) _ oracle-time-ms _}
                          (time-apply run-exe-on-input
                                      (list oracle-path
                                            oracle-input
                                            oracle-timeout-seconds)))

     (define oracle-output-json
       (if (not oracle-output-bytes)
           bad-json
           (bytes->json/safe oracle-output-bytes)))
     (cond [(equal? oracle-output-json bad-json)
            (log-fest-error @~a{
                                The oracle seems to be confused. Giving up on this test.
                                @test-log-messages-delimiter
                                })
            #f]
           [(and oracle-needs-student-output?
                 (not oracle-output-json))
            (log-fest-error @~a{
                                @(pretty-path exe-path) fails test @(test-display-name input-file) @;
                                because it produced an invalid result
                                It produced this json:
                                ------------------------------
                                @(with-output-to-string (thunk (write-json exe-output-json)))
                                ------------------------------
                                @test-log-messages-delimiter
                                })
            #f]
           [(and (not oracle-needs-student-output?)
                 (not (jsexpr=? exe-output-json oracle-output-json)))
            (log-fest-error @~a{
                                @(pretty-path exe-path) fails test @(test-display-name input-file) @;
                                because it produced the wrong result.
                                It produced this json:
                                ------------------------------
                                @(with-output-to-string (thunk (write-json exe-output-json)))
                                ------------------------------
                                @test-log-messages-delimiter
                                })
            (when (log-test-failure-comparison?)
              (log-fest-error @~a{
                                  The expected output json for @(test-display-name input-file) is:
                                  ------------------------------
                                  @(with-output-to-string (thunk (write-json oracle-output-json)))
                                  ------------------------------
                                  @test-log-messages-delimiter
                                  })
              (log-test-failure-comparison? #f))
            #f]
           [else
            (log-fest-info @~a{
                               Test passed.
                               @test-log-messages-delimiter
                               })
            #t])]
    [(? bytes? non-json-bytes)
     (log-fest-error @~a{
                         @(pretty-path exe-path) fails test @(test-display-name input-file) @;
                         because it produced invalid json.
                         The raw output is below. @;
                         @(if (= (bytes-length non-json-bytes) process-stdout-bytes-limit)
                              @~a{
                                  It hit the @process-stdout-bytes-limit @;
                                  bytes size limit, which might be why it was invalid.
                                  }
                              "")
                         ------------------------------
                         @(try-decode-bytes->string non-json-bytes)
                         ------------------------------
                         @test-log-messages-delimiter
                         })
     #f]
    [#f
     (log-fest-error @~a{
                         @(pretty-path exe-path) fails test @(test-display-name input-file) @;
                         because something went wrong while running it.
                         @test-log-messages-delimiter
                         })
     #f]))

(define/contract (valid-tests test-directory
                              check-validity
                              #:check-json-validity? [check-json-validity? #t]
                              #:require-output-file? [require-output-file? #t]
                              #:test-timeout [test-timeout absolute-max-timeout-seconds]
                              #:max-count [max-count +inf.0])
  (->* {path-to-existant-directory?
        (path-to-existant-file? (or/c path-to-existant-file? #f) . -> . boolean?)}
       {#:check-json-validity? boolean?
        #:require-output-file? boolean?
        #:test-timeout natural?
        #:max-count natural?}
       (listof test/c))

  (define all-tests (directory->tests test-directory))
  (define valid
    (filter (match-lambda [(test input-file output-file)
                           (cond [(and require-output-file?
                                       (or (not output-file)
                                           (not (file-exists? output-file))))
                                  (log-fest-error
                                   @~a{
                                       Invalid: @(pretty-path input-file), missing output file.
                                       @test-log-messages-delimiter
                                       })
                                  #f]
                                 [(and check-json-validity?
                                       (not (valid-json-file? input-file)))
                                  (log-fest-error
                                   @~a{
                                       Invalid: @(pretty-path input-file), input is not json.
                                       @test-log-messages-delimiter
                                       })
                                  #f]
                                 [(and check-json-validity?
                                       output-file
                                       (not (valid-json-file? output-file)))
                                  (log-fest-error
                                   @~a{
                                       Invalid: @(pretty-path input-file), output is not json.
                                       @test-log-messages-delimiter
                                       })
                                  #f]
                                 [(not (check-validity input-file output-file))
                                  (log-fest-error
                                   @~a{
                                       Invalid: @(pretty-path input-file), fails validity test.
                                       @test-log-messages-delimiter
                                       })
                                  #f]
                                 [else
                                  (log-fest-info
                                   @~a{
                                       @(pretty-path input-file) is valid.
                                       @test-log-messages-delimiter
                                       })
                                  #t])])
            all-tests))
  (define invalid-tests (set-subtract (apply set all-tests) (apply set valid)))
  (unless (set-empty? invalid-tests)
    (log-fest-info
     @~a{
         invalid tests:
         @(apply string-append
                 (for/list ([t (in-list (sort (set->list invalid-tests) string<? #:key ~s))])
                   (~a "  input file:  " (pretty-path (test-input-file t)) "\n"
                       "  output file: " (pretty-path (test-output-file t)) "\n")))
         }))
  (take valid (inexact->exact (truncate (min max-count (length valid))))))

(define/contract (valid-tests/passing-oracle test-directory
                                             oracle-path
                                             oracle-type
                                             #:check-json-validity? [check-json-validity? #t]
                                             #:require-output-file? [require-output-file? #t])
  ({path-to-existant-directory?
    path-to-existant-file?
    oracle-type/c}
   {#:check-json-validity? boolean?
    #:require-output-file? boolean?}
   . ->* .
   (listof test/c))

  (valid-tests
   test-directory
   #:check-json-validity? check-json-validity?
   #:require-output-file? require-output-file?
   (λ (in out) ;; Now we can assume the input and output both have valid json
     (match oracle-type
       ['normal
        (define oracle-output (run-exe-on-input oracle-path
                                                (call-with-input-file in test-transformer)))
        (cond [(bytes? oracle-output)
               (define oracle-output-json (bytes->json/safe oracle-output))
               (define expected-output-json (call-with-input-file out
                                              (compose1 read-json/safe test-transformer)))
               (define passes? (jsexpr=? oracle-output-json expected-output-json))
               (when (and (not passes?)
                          (log-test-failure-comparison?))
                 (log-fest-error
                  @~a{
                      The output file @(pretty-path out) does not match the oracle's output.
                      The oracle answer for this test is:
                      ------------------------------
                      @(with-output-to-string (thunk (write-json oracle-output-json)))
                      ------------------------------
                      })
                 (log-test-failure-comparison? #f))
               passes?]
              [else #f])]
       ;; There are no expected outputs for assignments with these oracles.
       ;; If there are tests for these assignments, however, they are required (in the config)
       ;; to have test outputs.
       ;; Just make sure that the oracle doesn't break on them, and that the test outputs are
       ;; valid.
       ['checks-output
        (define oracle-input
          (input-port-append #t
                             ;; these files get closed because
                             ;; the call to copy-port in launch-process!
                             ;; will copy all of the data out of this
                             ;; port (which'll trigger the close
                             ;; via input-port-append)
                             (open-input-file in)
                             (open-input-bytes #"\n")
                             (open-input-file out)))
        (match-define oracle-output-bytes
          (run-exe-on-input oracle-path
                            oracle-input
                            oracle-timeout-seconds))
        (cond
          [(not oracle-output-bytes)
           #f]
          [else
           (define oracle-output-json (bytes->json/safe oracle-output-bytes))
           (cond
             [(or (equal? oracle-output-json bad-json)
                  (not (boolean? oracle-output-json)))
              (log-fest-error
               @~a{
                   The oracle seems to be confused. Giving up validating this test.
                   The oracle produced:
                   ------------------------------
                   @~s[oracle-output-json]
                   ------------------------------
                   })
              #f]
             [else oracle-output-json])])]
       ['interacts
        (exe-passes-test?/racket-oracle oracle-path
                                        oracle-path
                                        (test in out))]))))

(define (test-failures-for exe-path oracle-path tests-by-group
                           #:munge-json? [munge-json? #f]
                           #:racket-based-oracle? [racket-based-oracle? #f]
                           #:oracle-needs-student-output? [oracle-needs-student-output? #f])
  (->* (path-to-existant-file? path-to-existant-file? test-set/c)
       (#:oracle-needs-student-output? boolean?
        #:munge-json? boolean?)
       test-set/c)

  (define munge-counter 0)
  (define (passes-test? t)
    (set! munge-counter (+ munge-counter 1))
    (cond
      [racket-based-oracle? (exe-passes-test?/racket-oracle exe-path oracle-path t)]
      [else (exe-passes-test? exe-path oracle-path t #:oracle-needs-student-output? oracle-needs-student-output?
                              #:munge-json-style (and munge-json?
                                                      (case (modulo munge-counter 3)
                                                        [(0) 'dribble]
                                                        [(1) #f]
                                                        [(2) 'pad])))]))
  (for*/hash ([group (in-list (sort (hash-keys tests-by-group) string<?))]
              [tests (in-value (sort (hash-ref tests-by-group group)
                                     string<?
                                     #:key (compose1 ~a test-input-file)))]
              [failed-tests (in-value (filter-not passes-test? tests))]
              #:unless (empty? failed-tests))
    (values group failed-tests)))
