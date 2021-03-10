#lang at-exp racket

(provide launch-process!
         wait/keep-ci-alive
         process-stdout-bytes-limit)

(require "util.rkt"
         "logger.rkt")

(define (containing-directory path)
  (define-values {dir _} (basename path #:with-directory? #t))
  dir)

(define process-stdout-bytes-limit (* 16 1024))
(define process-stderr-bytes-limit process-stdout-bytes-limit)
(define/contract (launch-process! exe-path
                                  [args empty]
                                  #:stdin [stdin #f]
                                  #:stdout [stdout #f]
                                  #:stderr [stderr 'stdout]
                                  #:run-in [run-in (containing-directory exe-path)]
                                  #:limit-stdout? [limit-stdout? #f]
                                  #:limit-stderr? [limit-stderr? #f])
  (->i {[exe-path path-to-existant-file?]}
       {[args (listof string?)]
        #:stdin [stdin (or/c (and/c input-port? file-stream-port?) #f)]
        #:stdout [stdout (or/c (and/c output-port? file-stream-port?) #f)]
        #:stderr [stderr (or/c (and/c output-port? file-stream-port?) 'stdout #f)]
        #:run-in [run-in path-string?]
        #:limit-stdout? [limit-stdout? boolean?]
        #:limit-stderr? [limit-stderr? boolean?]}
       (values [r1 subprocess?]
               [r2 (stdout)
                   (if (input-port? stdout)
                       false?
                       input-port?)]
               [r3 (stderr)
                   (if (input-port? stderr)
                       false?
                       input-port?)]))

  (define-values {proc returned-stdout _ returned-stderr}
    (parameterize ([current-directory run-in])
      (apply subprocess
             stdout stdin stderr
             'new
             exe-path
             args)))
  (values proc
          (if limit-stdout?
              (make-limited-input-port returned-stdout
                                       process-stdout-bytes-limit)
              returned-stdout)
          (if limit-stderr?
              (make-limited-input-port returned-stderr
                                       process-stderr-bytes-limit)
              returned-stderr)))

(define ci-no-output-heartbeat-minutes 8)

;; Travis CI kills any job that has no output for 10 minutes; prevent that.
;; Returns whether proc completed
(define/contract (wait/keep-ci-alive proc timeout-seconds)
  (subprocess? (and/c real? positive?) . -> . boolean?)

  (log-fest-debug @~a{Waiting for process with timeout: @|timeout-seconds|s})
  (let loop ([time-left timeout-seconds])
    (define time-left/less-waiting-time
      (max (- time-left (* ci-no-output-heartbeat-minutes 60))
           0))
    (define proc-finished? (sync/timeout ci-no-output-heartbeat-minutes proc))
    (displayln ".")
    (cond [proc-finished? #t]
          [(zero? time-left/less-waiting-time) #f]
          [else (loop time-left/less-waiting-time)])))
