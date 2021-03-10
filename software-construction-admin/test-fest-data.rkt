#lang at-exp racket

(provide use-pre-validated-tests?)

(require "logger.rkt"
         gregor)

;; sunday is 0, saturday is 6
(define pre-validated-test-days '(1 2 3 4)) ;; monday - thursday
(define (use-pre-validated-tests?)
  (define week-day (->wday (today #:tz "America/Chicago")))
  (log-fest-debug
            @~a{Today: @week-day, pre-valid-days: @pre-validated-test-days})
  (member week-day pre-validated-test-days))

