#lang racket

(provide log-fest)

(require syntax/parse/define)

(define fest-logger (make-logger 'fest
                                 (current-logger)))

(define-simple-macro (log-fest level:id msg)
  (log-message fest-logger
               'level
               (string-append (format "~a: " 'level) msg)
               #f))

