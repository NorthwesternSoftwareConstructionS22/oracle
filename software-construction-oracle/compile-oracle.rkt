#lang at-exp racket

(require racket/cmdline
         racket/runtime-path
         "test-fest-data.rkt"
         "util.rkt")

(define raco-exe (find-executable-path "raco"))
(define-runtime-path distribute-dir "../distribute")

(module+ main
  (define assign-major-number-box (box "0"))
  (define assign-minor-number-box (box "0"))
  (define oracle-path-box (box "./main.rkt"))

  (command-line
   #:once-each
   [("-M" "--Major")
    assign-number*
    "Assignment major number. E.g. for 5.2 this is 5."
    (set-box! assign-major-number-box assign-number*)]
   [("-m" "--minor")
    assign-number*
    "Assignment minor number. E.g. for 5.2 this is 2."
    (set-box! assign-minor-number-box assign-number*)]
   [("-f" "--oracle-file")
    path
    "Path to main module of oracle"
    (set-box! oracle-path-box path)])

  (define assign-number (cons (unbox assign-major-number-box)
                              (unbox assign-minor-number-box)))
  (define oracle-path (unbox oracle-path-box))

  (define-values {oracle-dir oracle-file-name}
    (basename oracle-path #:with-directory? #t))
  (define assign-distrib-path
    (build-path-string distribute-dir
                       (assign-number->dir-path assign-number)))
  (define oracle-distrib-placeholder-file
    (build-path-string assign-distrib-path
                       oracle-file-name))

  (delete-directory/files assign-distrib-path
                          #:must-exist? #f)
  (make-directory* assign-distrib-path)
  (void (system @~a{echo "" > @oracle-distrib-placeholder-file}))
  (void (call-with-extended-environment
         (hash "PLTCOMPILEDROOTS" "compiled/@(version):"
               "PLT_COMPILE_ANY" "yes")
         (thunk (system* raco-exe "make" oracle-path))))
  (rename-file-or-directory (build-path oracle-dir "compiled")
                            (build-path assign-distrib-path "compiled")))
