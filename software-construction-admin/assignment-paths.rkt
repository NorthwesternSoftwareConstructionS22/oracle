#lang at-exp racket

(provide assign-number->oracle-path
         assign-number->validated-tests-path
         assign-number->submitted-tests-path)

(require racket/runtime-path
         "assignments.rkt"
         "util.rkt")

(define-runtime-path validated-tests-dir "../backgammon-oracle/validated-tests")
(define-runtime-path submitted-tests-dir "../backgammon-oracle/submitted-tests")
(define-runtime-path oracle-binary-dir "../backgammon-oracle/oracle")

(define/contract (assign-number->validated-tests-path assign-number)
  (assign-number? . -> . path-string?)

  (build-path validated-tests-dir
              (assign-number->dir-path-part assign-number)))

(define/contract (assign-number->submitted-tests-path assign-number)
  (assign-number? . -> . path-string?)

  (build-path submitted-tests-dir
              (assign-number->dir-path-part assign-number)))


(define/contract (assign-number->oracle-path assign-number)
  (assign-number? . -> . (or/c path-to-existant-file? #f))

  (define assign-dir
    (build-path oracle-binary-dir
                (assign-number->dir-path-part assign-number)))
  (and (directory-exists? assign-dir)
       (build-path assign-dir "oracle")
       #;(for/first ([f (in-directory assign-dir)]
                   #:when (equal? (path-get-extension f) #".rkt"))
         f)))

