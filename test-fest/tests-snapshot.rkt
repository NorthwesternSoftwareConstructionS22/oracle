#lang at-exp racket

(require racket/cmdline
         racket/runtime-path
         "util.rkt"
         "test-fest-data.rkt"
         "git.rkt"
         "tar.rkt"
         "logger.rkt"
         "env.rkt"
         "test-cache.rkt")

(define env-file "env.sh")

(define-runtime-path oracle-repo "..")

(module+ main
  (define assign-major-number-box (box "0"))
  (define assign-minor-number-box (box "0"))
  (define tests-repo-box (box "/not/given"))
  (define clean-invalid-box (box #f))

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
   [("-t" "--tests-repo-path")
    path
    "Path to the tests repo."
    (set-box! tests-repo-box path)]

   [("-c" "--clean-invalid")
    "Clean invalid tests from the snapshot instead of taking a new snapshot."
    (set-box! clean-invalid-box #t)])

  (define assign-number (cons (unbox assign-major-number-box)
                              (unbox assign-minor-number-box)))
  (define valid-tests-repo-path (unbox tests-repo-box))
  (define this-assign-tests-dir (assign-number->dir-path assign-number))
  (define this-assign-tests-dir-path
    (find-cached-tests-path valid-tests-repo-path assign-number))

  (match (unbox clean-invalid-box)
    [#f
     (displayln
      "Is this copy of the oracle repo up to date? Hit Enter to continue.")
     (void (read-line))

     (delete-directory/files this-assign-tests-dir-path #:must-exist? #f)
     (make-directory* this-assign-tests-dir-path)
     (log-fest info
               @~a{Cloning test repos into @this-assign-tests-dir-path ...})
     (void
      (clone-repos-into! this-assign-tests-dir-path
                         student-test-repos
                         #:setup-repos (thunk (delete-directory/files ".git")))
      (copy-directory/files (build-path oracle-repo "tests")
                            (build-path this-assign-tests-dir-path "oracle")))
     (log-fest info @~a{Done. Writing env ...})
     (write-env! valid-tests-repo-path
                 env-file
                 "dummy"
                 assign-number)
     (log-fest info @~a{Done. git-adding snapshot ...})
     (displayln "About to commit and push; hit Enter to continue.")
     (void (read-line))
     (commit-and-push! valid-tests-repo-path
                       @~a{Validate: @(assign-number->string assign-number)}
                       #:add (list this-assign-tests-dir
                                   env-file)
                       #:remote "origin"
                       #:branch "master")]

    [#t
     (log-fest info @~a{Reading valid test information from stdin...})
     (define valid-tests-serialized (read))
     (log-fest info @~a{Deserializing test info...})
     (define valid-tests (deserialize-tests valid-tests-repo-path
                                            valid-tests-serialized))
     (define valid-test-file-set
       (for*/set ([t (in-list valid-tests)]
                  [path (in-list (list (test-in t) (test-out t)))])
         path))
     (log-fest debug @~a{Determined valid test set @valid-test-file-set})
     (log-fest info @~a{Cleaning invalid tests...})
     (for ([path (in-directory this-assign-tests-dir-path)]
           #:when (file-exists? path)
           [path-absolute (in-value (simple-form-path-string path))]
           #:unless (set-member? valid-test-file-set path-absolute))
       (log-fest debug @~a{Deleting @path-absolute})
       (delete-file path-absolute))
     (log-fest info @~a{Done. Writing valid test file...})
     (write-to-file valid-tests-serialized
                    (find-cached-test-info-path valid-tests-repo-path
                                                assign-number))

     (displayln "About to commit and push; hit Enter to continue.")
     (void (read-line))
     (commit-and-push! valid-tests-repo-path
                       @~a{Validated @(assign-number->string assign-number)}
                       #:add (list this-assign-tests-dir env-file)
                       #:remote "origin"
                       #:branch "master")]))
