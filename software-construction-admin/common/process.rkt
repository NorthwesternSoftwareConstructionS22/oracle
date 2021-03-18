#lang at-exp racket

(provide launch-process!
         process-stdout-bytes-limit)

(require "util.rkt")

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

