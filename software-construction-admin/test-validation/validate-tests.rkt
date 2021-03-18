#lang at-exp racket

(require racket/pretty
         "cmdline.rkt"
         "tests.rkt"
         "testing.rkt"
         "util.rkt"
         "assignment-paths.rkt")

(module+ main
  (match-define (cons (hash-table ['major major-number]
                                  ['minor minor-number])
                      args)
    (command-line/declarative
     #:once-each
     [("-M" "--Major")
      'major
      "Assignment major number. E.g. for 5.2 this is 5."
      #:collect {"number" take-latest #f}
      #:mandatory]
     [("-m" "--minor")
      'minor
      "Assignment minor number. E.g. for 5.2 this is 2."
      #:collect {"number" take-latest #f}
      #:mandatory]))

  (define assign-number (cons major-number minor-number))
  (define all-valid-tests
    (valid-tests/passing-oracle (assign-number->submitted-tests-path assign-number)
                                (assign-number->oracle-path assign-number)
                                #:check-json-validity? #t))

  (displayln "-----begin-validated-----")
  (pretty-write
   (for/list ([test (in-list all-valid-tests)])
     (basename (test-input-file test))))
  (displayln "-----end-validated-----"))

