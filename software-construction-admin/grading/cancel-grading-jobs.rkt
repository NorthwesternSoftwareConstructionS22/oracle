#lang racket

(require "../github-actions/actions.rkt")
(require "../common/option.rkt")
(define h (file->value "grading-jobs.rktd"))
(for ([url (in-list (hash-values h))])
  (option-let*
   [run (get-run-by-url! url)]
   (cancel-run! run))
  (displayln "."))

