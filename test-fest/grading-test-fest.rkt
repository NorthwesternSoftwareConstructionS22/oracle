#lang at-exp racket

(require racket/cmdline
         racket/runtime-path
         "test-fest-data.rkt"
         "util.rkt"
         "git.rkt"
         "test.rkt"
         "logger.rkt"
         "tar.rkt")

(define test-repos-dir "./peer-tests")

(define-runtime-path oracle-repo "..")

(define/contract (make-repo-exe! repo-path assign-number)
  (path-to-existant-directory?
   assign-number?
   . -> .
   (or/c path-to-existant-file? #f))

  (define assign-dir (build-path repo-path
                                 "Deliverables"
                                 (assign-number->dir-path assign-number)))
  (and (directory-exists? assign-dir)
       (parameterize ([current-directory assign-dir])
         (system "make > /dev/null 2>&1")
         (system "chmod u+x ./run")
         (file-exists? "./run"))
       (build-path-string assign-dir "run")))

(define/contract (summarize-test-fest assign-number results valid-tests)
  (assign-number? (hash/c repo-name? test-set/c) test-set/c . -> . void?)

  (define total-valid-test-count
    (test-set-count-tests valid-tests))
  (displayln
   @~a{

       ==================== Test fest summary ====================
                            Assignment @assign-number
       ===========================================================
       @"("
       })
  (writeln assign-number)
  (displayln "(")
  (for ([(dev-repo-name failed-tests) (in-hash results)])
    (define team-name (repo->team-name dev-repo-name))
    (define test-repo-name (group->test-repo-name team-name))
    (define valid-tests-for-repo (hash-ref valid-tests test-repo-name empty))
    (define valid-test-count (length valid-tests-for-repo))
    (define total-failed-test-count (test-set-count-tests failed-tests))
    (define test-fest-grade (- 1 (/ total-failed-test-count
                                    total-valid-test-count)))
    (displayln
     @~a{(@~v[team-name] @valid-test-count @test-fest-grade)}))
  (displayln "))"))

(module+ main
  (define assign-major-number-box (box "0"))
  (define assign-minor-number-box (box "0"))
  (define repo-cache-file-path-box (box "./student-snapshots"))

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
   [("-r" "--grading-repo-path")
    path
    "Path to the root of the grading repo."
    (current-directory path)])

  (define assign-number (cons (unbox assign-major-number-box)
                              (unbox assign-minor-number-box)))
  (define repo-cache-file (find-repo-cache-file assign-number))
  (unless (not (file-exists? repo-cache-file))
    (eprintf "Error: Repo cache file not found~n"))
  (define student-repo-caches
    (with-handlers ([exn:fail?
                     (λ _
                       (log-fest error
                                 @~a{Failed to read @repo-cache-file})
                       (exit 1))])
      (call-with-input-file repo-cache-file read)))

  (delete-directory/files test-repos-dir
                          #:must-exist? #f)
  (make-directory test-repos-dir)
  (log-fest info @~a{Cloning test repos into @test-repos-dir ...})
  (define test-repo-paths (clone-repos-into! test-repos-dir
                                             student-test-repos))
  (define test-repo-paths+oracle
    (hash-set test-repo-paths
              "oracle"
              (build-path-string oracle-repo "tests")))
  (log-fest info @~a{Done. Checking test validitity ...})
  (define valid-peer-tests
    (for/hash ([(repo-name repo-path) (in-hash test-repo-paths+oracle)])
      (values repo-name
              (valid-tests/passing-oracle repo-path
                                          assign-number
                                          oracle-repo
                                          #:check-json-validity? #t))))

  (log-fest info @~a{Unpacking dev repos ...})
  (define dev-repo-paths
    (unzip-repos! student-repo-caches))
  (log-fest info @~a{Done. Commencing test fest ...})
  (define test-fest-results
    (for/hash ([(repo-name repo-path) (in-hash dev-repo-paths)])
      (log-fest info @~a{Testing @repo-name ...})
      (log-fest info @~a{repo: @repo-path :: cwd: @(current-directory)})
      (define failed-peer-tests
        (match (make-repo-exe! repo-path assign-number)
          [#f valid-peer-tests]
          [path-to-test-exe
           (parameterize ([current-directory (build-path repo-path "Deliverables")])
             (test-failures-for path-to-test-exe
                                valid-peer-tests))]))
      (values repo-name failed-peer-tests)))
  (log-fest info @~a{Test fest complete.})

  (summarize-test-fest assign-number
                       test-fest-results
                       valid-peer-tests))
