#lang at-exp racket

(provide assign-number?
         assign-number->string
         assign-number->dir-path-part
         assign-major-number
         assign-minor-number

         assign-number->oracle-path
         assign-number->validated-tests-path
         assign-number->submitted-tests-path
         assign-number->deliverables-path
         assign-number->deliverable-exe-path)

(require racket/runtime-path
         "util.rkt"
         "../config.rkt")

(define major-number? (and/c string? #rx"[0-9]+"))
(define minor-number? major-number?)
(define assign-number? (cons/c major-number? minor-number?))

(define/contract (assign-number->string a)
  (assign-number? . -> . string?)

  (match a
    [(cons major minor) @~a{@|major|.@|minor|}]))

(define/contract (assign-number->dir-path-part a)
  (assign-number? . -> . string?)

  (build-path-string (car a)
                     (assign-number->string a)))

(define/contract assign-major-number (assign-number? . -> . major-number?) car)
(define/contract assign-minor-number (assign-number? . -> . minor-number?) cdr)

(define/contract (assign-number->validated-tests-path assign-number)
  (assign-number? . -> . path-string?)

  (build-path validated-tests-dir
              (assign-number->dir-path-part assign-number)))

(define/contract (assign-number->submitted-tests-path assign-number)
  (assign-number? . -> . path-string?)

  (build-path submitted-tests-dir
              (assign-number->dir-path-part assign-number)))

(define/contract (assign-number->deliverables-path assign-number)
  (assign-number? . -> . path-string?)

  (build-path "Deliverables"
              (assign-number->dir-path-part assign-number)))

(define/contract (assign-number->deliverable-exe-path assign-number)
  (assign-number? . -> . path-string?)

  (build-path (assign-number->deliverables-path assign-number)
              "run"))


(define/contract (assign-number->oracle-path assign-number
                                             #:racket-based-oracle? racket-based-oracle?)
  (assign-number? #:racket-based-oracle? boolean? . -> . (or/c path-to-existant-file? #f))

  (define candidate
    (build-path oracle-binary-dir
                (format "oracle~a.~a~a"
                        (car assign-number)
                        (cdr assign-number)
                        (if racket-based-oracle? ".rkt" ""))))
  (and (file-exists? candidate)
       candidate))

