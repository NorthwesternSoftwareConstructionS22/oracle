#lang at-exp racket

(require "util.rkt"
         "test-fest-data.rkt"
         "git.rkt"
         "tar.rkt"
         "logger.rkt")

(module+ main
  (define assign-major-number-box (box "0"))
  (define assign-minor-number-box (box "0"))
  (define deadline-box (box "2019-10-01 23:59:59"))

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
   [("-d" "--deadline")
    deadline
    @~a{Deadline for assignment in iso format "YYYY-MM-DD HH:MM:SS"}
    (set-box! deadline-box deadline)]
   [("-r" "--grading-repo-path")
    path
    "Path to the root of the grading repo."
    (current-directory path)])

  (define assign-number (cons (unbox assign-major-number-box)
                              (unbox assign-minor-number-box)))
  (define repo-cache-file (find-repo-cache-file assign-number))
  (define deadline-parts (string-split (unbox deadline-box)))
  (define deadline @~a{@(first deadline-parts)T@(second deadline-parts)})

  (define snapshot-dir (build-path "."
                                   (assign-number->string assign-number)))
  (delete-directory/files snapshot-dir #:must-exist? #f)
  (make-directory snapshot-dir)
  (log-fest info @~a{Cloning dev repos into @snapshot-dir ...})
  (define dev-repos
    (clone-repos-into! snapshot-dir
                       student-dev-repos
                       #:setup-repos (checkout-last-commit-before deadline)))
  (log-fest info @~a{Done. Zipping dev repos ...})
  (define dev-zips (zip-repos! dev-repos #:delete-original? #t))
  (log-fest info @~a{Done. Writing cache file @repo-cache-file ...})
  (call-with-output-file repo-cache-file
    (thunk (write dev-zips))
    #:mode 'text
    #:exists 'truncate)
  (log-fest info @~a{Done. git-adding snapshot ...})
  (git-add repo-cache-file)
  (git-add snapshot-dir)
  (displayln @~a{
                 Git-added snapshot of student dev repos.
                 Commit and then push to kick off grading.
                 }))
