#lang racket/base

(provide command-line/declarative
         take-latest
         snoc
         set-parameter)

(require syntax/parse/define
         racket/cmdline
         racket/match
         racket/list
         (for-syntax racket/base))

;; Example syntax:
#;(command-line/declarative
   #:program "foobar"
   #:once-each
   [("-T" "--tests-only")
    'tests-only
    "Only run tests, not benchmarks."
    #:record
    #:conflicts '(benchmarks-only benchmark)]
   [("-B" "--benchmarks-only")
    'benchmarks-only
    "Only run benchmarks, not benchmarks."
    #:record
    #:conflicts '(tests-only test)]
   #:multi
   [("-t" "--test")
    'test
    "Run this specific test; disable all others (except those also specified with this option)"
    #:collect ["path" cons '()]]
   [("-b" "--benchmark")
    'benchmark
    "Run this specific benchmark; disable all others (except those also specified with this option)"
    #:collect ["benchmark name" cons '()]]
   #:args (positional-1 positional-2))


(struct accumulator (current init combine)
  #:transparent
  #:property
  prop:procedure
  (λ (self new-val)
    (match-define (accumulator current init combine) self)
    (accumulator (combine new-val current)
                 init
                 combine)))

(define (record _1 _2) #t)
(define (make-collector combine init)
  (accumulator init
               init
               combine))

(define/match (collect-flags flag-accum state)
  [{'() state} state]
  [{(cons (cons acc-key val) rest) state}
   (define accumulate (hash-ref state acc-key))
   (collect-flags rest
                  (hash-set state acc-key (accumulate val)))])

(define (specified-in? collected-flags flag)
  (match collected-flags
    [(hash-table [(== flag) (accumulator current init _)] _ ...)
     (not (equal? current init))]
    [else #f]))

(define (check-conflicts! collected-flags conflict-hash mandatory-args)
  (for ([(k conflicts) (in-hash conflict-hash)]
        #:when (specified-in? collected-flags k)
        [conflicting-k (in-list conflicts)]
        #:when (specified-in? collected-flags conflicting-k))
    (raise-user-error 'command-line/declarative
                      "Argument error: ~a and ~a cannot both be specified"
                      k conflicting-k))
  (define specified-args
    (for/list ([k (in-hash-keys collected-flags)]
               #:when (specified-in? collected-flags k))
      k))
  (define unspecified-mandatory-args
    (for/list ([(flag test) (in-hash mandatory-args)]
               #:when (not (or (specified-in? collected-flags flag)
                               (test specified-args))))
      flag))
  (unless (empty? unspecified-mandatory-args)
    (raise-user-error 'command-line/declarative
                      "Missing mandatory arguments: ~a"
                      unspecified-mandatory-args)))

(define (strip-accumulators flags-hash)
  (for/hash ([(k acc) (in-hash flags-hash)])
    (values k (accumulator-current acc))))



(begin-for-syntax
  (require syntax/parse
           racket/list)
  (define-syntax-class flag-spec
    #:commit
    #:attributes
    [flags name handler help-msgs arg-name
           init-value
           collector-function
           conflicting-flags
           mandatory-kw
           mandatory-unless-pred]

    [pattern
     [flags:expr name:expr {~or* (descs:expr ...)
                                 desc:str}
                 {~alt
                  {~optional {~and mandatory-kw #:mandatory}}
                  {~optional {~seq #:mandatory-unless
                                   mandatory-unless-pred:expr}}
                  {~once {~or* {~and record-kw #:record}
                               {~seq #:collect [arg-name:expr
                                                collector:expr
                                                init:expr]}}}
                  {~optional {~seq #:conflicts conflicts:expr}}}
                 ...

                 ;; {~optional {~and mandatory-kw #:mandatory}}
                 ;; {~optional {~seq #:mandatory-unless
                 ;;                  mandatory-unless-pred:expr}}
                 ;; {~or* {~and record-kw #:record}
                 ;;       {~seq #:collect [arg-name:expr
                 ;;                        collector:expr
                 ;;                        init:expr]}}
                 ;; {~optional {~seq #:conflicts conflicts:expr}}
                 ]
     #:with handler #`(let ([flag-name name])
                        (λ (_ #,@(if (attribute collector) #'(arg) #'()))
                          #,(cond [(attribute record-kw)
                                   #'(cons flag-name #t)]
                                  [else
                                   #'(cons flag-name arg)])))
     #:with help-msgs #'(list {~? {~@ descs ...} desc})
     #:with init-value #'{~? init #f}
     #:with collector-function (if (attribute record-kw)
                                   #'record
                                   #'collector)
     #:with conflicting-flags (if (attribute conflicts)
                                  #'conflicts
                                  #''())]))

(define-simple-macro
  (command-line/declarative {~optional {~seq #:program name:expr}}
                 {~optional {~seq #:argv argv:expr}}
                 {~alt {~seq kw:keyword spec:flag-spec ...}
                       {~seq {~and help-kw:keyword
                                   {~or* #:usage-help #:help-labels #:ps}}
                             help-str:str ...}}
                 ...
                 {~optional {~seq #:args {~or* (pos-arg:id ...)
                                               (pos-arg:id ...+ . pos-arg-rest:id)
                                               pos-arg-rest:id}}})
  #:with [pos-arg-inferred-name ...] #'{~? (pos-arg ... pos-arg-rest)
                                           {~? (pos-arg ...)
                                               {~? (pos-arg-rest)
                                                   ()}}}
  #:with flag-init-hash #'(hash {~@ spec.name
                                    (make-collector spec.collector-function
                                                    spec.init-value)}
                                ...
                                ...)
  #:with flag-conflict-hash #'(make-immutable-hash
                               (list (cons spec.name spec.conflicting-flags)
                                     ...
                                     ...))
  #:with [{~alt (mandatory-arg _) (_)} ...]
  #'((spec.name {~? spec.mandatory-kw}) ... ...)
  #:with [{~alt (maybe-mandatory-arg test) (_)} ...]
  #'((spec.name {~? spec.mandatory-unless-pred}) ... ...)
  (command-line
   {~? {~@ #:program name}}
   {~? {~@ #:argv argv}}
   {~@ help-kw help-str ...} ...
   {~@ kw [spec.flags => spec.handler (list spec.help-msgs {~? spec.arg-name})] ...}
   ...
   #:handlers
   (λ {~? (flag-accum pos-arg ... . pos-arg-rest)
          {~? (flag-accum pos-arg ...)
              {~? (flag-accum . pos-arg-rest)
                  (flag-accum)}}}
     (define flags-hash
       (collect-flags flag-accum
                      flag-init-hash))
     (check-conflicts! flags-hash
                       flag-conflict-hash
                       (hash {~@ mandatory-arg (λ _ #f)} ...
                             {~@ maybe-mandatory-arg test} ...))
     (cons (strip-accumulators flags-hash)
           {~? (list* pos-arg ... pos-arg-rest)
               {~? (list pos-arg ...)
                   {~? pos-arg-rest
                       '()}}}))
   (map symbol->string '(pos-arg-inferred-name ...))))

(define (take-latest new old) new)
(define (snoc el l)
  (append l (list el)))
(define (set-parameter p [transform values])
  (λ (new _) (p (transform new))))

(module+ test
  (require ruinit
           racket)
  (test-begin
    #:name basic
    (ignore (define (parse args)
              (command-line/declarative
               #:argv args
               #:once-each
               [("-v")
                'verbose
                "verboseness"
                #:record]
               [("--ff")
                'ff
                ("ff" "is great" (~a "inside multiline parens can be an expr!"))
                #:collect ["arg" take-latest "a-default"]]
               #:multi
               [("-a")
                'arg
                "arg"
                #:collect ["arg" cons '()]]
               #:args pos-args)))
    (test-equal? (parse '("-v"))
                 (cons (hash 'verbose #t
                             'ff "a-default"
                             'arg '())
                       '()))
    (test-equal? (parse '("-v" "--ff" "ff-arg" "hello"))
                 (cons (hash 'verbose #t
                             'ff "ff-arg"
                             'arg '())
                       '("hello")))
    (test-equal? (parse '("hello"))
                 (cons (hash 'verbose #f
                             'ff "a-default"
                             'arg '())
                       '("hello")))
    (test-equal? (parse '())
                 (cons (hash 'verbose #f
                             'ff "a-default"
                             'arg '())
                       '()))
    (test-equal? (parse '("-a" "1" "-a" "2" "-a" "3" ))
                 (cons (hash 'verbose #f
                             'ff "a-default"
                             'arg '("3" "2" "1"))
                       '())))

  (test-begin
    #:name mandatory
    (ignore (define (parse args)
              (command-line/declarative
               #:argv args
               #:multi
               [("-n")
                'thing
                ("A thing to process" "Mandatory.")
                #:mandatory
                #:collect ["name" snoc '()]]
               #:once-each
               [("-F" "--only-f")
                'only-f
                ("Only do F's"
                 (number->string (+ 2 2)))
                #:record]
               [("-A")
                'A
                ("Do Fs?"
                 "Mandatory unless -F specified.")
                #:mandatory-unless (λ (flags) (member 'only-f flags))
                #:record]
               #:args pos-args)))
    (test-equal? (parse '("-A" "-n" "1" "-n" "2"))
                 (cons (hash 'A #t
                             'thing '("1" "2")
                             'only-f #f)
                       '()))
    (test-exn exn:fail?
              (parse '("-n" "1" "-n" "2")))
    (test-exn exn:fail?
              (parse '("-A")))
    (test-equal? (parse '("-F" "-n" "1"))
                 (cons (hash 'A #f
                             'thing '("1")
                             'only-f #t)
                       '()))
    (test-equal? (parse '("-A" "-F" "-n" "1"))
                 (cons (hash 'A #t
                             'thing '("1")
                             'only-f #t)
                       '())))

  (test-begin
    #:name conflicts
    (ignore (define (parse args)
              (command-line/declarative
               #:argv args
               #:once-each
               [("-F" "--only-f")
                'only-f
                ("Only do F's"
                 (number->string (+ 2 2)))
                #:record]
               [("-A")
                'A
                ("Do everything?"
                 "Mandatory unless -F specified.")
                #:mandatory-unless (λ (flags) (member 'only-f flags))
                #:record]
               [("-N")
                'not-f
                "Don't do F's"
                #:record
                #:conflicts '(only-f)]
               #:args pos-args)))
    (test-equal? (parse '("-A" "1" "2"))
                 (cons (hash 'A #t
                             'only-f #f
                             'not-f #f)
                       '("1" "2")))
    (test-equal? (parse '("-F" "1" "2"))
                 (cons (hash 'A #f
                             'only-f #t
                             'not-f #f)
                       '("1" "2")))
    (test-equal? (parse '("-A" "-N" "1" "2"))
                 (cons (hash 'A #t
                             'only-f #f
                             'not-f #t)
                       '("1" "2")))
    (test-exn exn:fail?
              (parse '("-F" "-N" "1" "2"))))

  (test-begin
    #:name help-text-clauses
    (ignore (define (parse args)
              (command-line/declarative
               #:argv args
               #:usage-help
               "this"
               "is"
               "usage"
               "text"
               #:once-each
               [("-F" "--only-f")
                'only-f
                ("Only do F's"
                 (number->string (+ 2 2)))
                #:record]
               #:ps
               "this"
               "is"
               "a ps")))
    (test-equal? (parse '("-F"))
                 (cons (hash 'only-f #t)
                       '()))
    (test-equal? (parse '())
                 (cons (hash 'only-f #f)
                       '())))

  (test-begin
    #:name pos-args
    (ignore (define (parse args)
              (command-line/declarative
               #:argv args
               #:args (arg1 arg2 . more-args))))
    (test-equal? (parse '("1" "2" "3" "4"))
                 (cons (hash)
                       '("1" "2" "3" "4")))))
