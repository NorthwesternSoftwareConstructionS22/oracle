#lang at-exp racket

(provide launch-process!
         wait/keep-ci-alive)

(require "util.rkt"
         "test-fest-data.rkt"
         "logger.rkt")

(define (containing-directory path)
  (define-values {dir _} (basename path #:with-directory? #t))
  dir)

(define/contract (launch-process! exe-path
                                  #:stdin [stdin #f]
                                  #:run-with-racket? [run-with-racket? #f]
                                  #:run-in [run-in
                                            (containing-directory exe-path)])
  ({path-to-existant-file?}
   {#:stdin (or/c output-port? #f)
    #:run-with-racket? boolean?
    #:run-in path-string?}
   . ->* .
   (values subprocess? input-port?))

  (define-values {proc stdout _1 _2}
    (parameterize ([current-directory run-in])
      (if run-with-racket?
          (subprocess/racket-bytecode (list #f stdin 'stdout)
                                      exe-path)
          (subprocess
           #f stdin 'stdout
           'new
           exe-path))))
  (values proc stdout))

(define racket-exe (find-executable-path "racket"))

(define/contract (subprocess/racket-bytecode subprocess-args path)
  (list? path-to-existant-file? . -> . any)

  (call-with-extended-environment
   (hash "PLT_COMPILED_FILE_CHECK" "exists"
         "PLTCOMPILEDROOTS" "compiled/@(version):")
   (thunk (apply subprocess (append subprocess-args
                                    (list racket-exe path))))))

;; Travis CI kills any job that has no output for 10 minutes; prevent that.
(define/contract (wait/keep-ci-alive proc timeout-seconds)
  (subprocess? (and/c real? positive?) . -> . (or/c subprocess? #f))

  (define waiting-period
    (min timeout-seconds ci-output-timeout-seconds))
  (define rounds-to-wait
    (round-up (/ timeout-seconds waiting-period)))
  (log-fest debug
            @~a{Waiting for @rounds-to-wait rounds of @|waiting-period|s})
  (for/or ([i (in-range rounds-to-wait)])
    (displayln ".")
    (sync/timeout waiting-period proc)))
