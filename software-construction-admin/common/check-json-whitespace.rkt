#lang racket

(provide
 (contract-out
  [port-only-contains-json-conforming-whitespace? (-> input-port? boolean?)]))

(require "logger.rkt")

(define (json-whitespace? ch)
  (or (eq? ch #\space)
      (eq? ch #\tab)
      (eq? ch #\newline)
      (eq? ch #\return)))

(define (port-only-contains-json-conforming-whitespace?
         port
         #:log-error-message [log-an-error (λ (x) (log-fest-error x))])
  (port-count-lines! port)
  (let loop ()
    (define c (read-char port))
    (cond
      [(eof-object? c) #t]
      [(implies (char-whitespace? c)
                (json-whitespace? c))
       (loop)]
      [else
       (define-values (line col pos)
         (port-next-location port))
       (log-an-error
        (format
         "found whitespace that isn't JSON-approved whitespace\n  line: ~a\n  column: ~a\n  char: ~s"
         line col c))
       #f])))

(module+ test
  (require rackunit)
  (define (try input-str)
    (define str #f)
    (define res (port-only-contains-json-conforming-whitespace?
                 (open-input-string input-str)
                 #:log-error-message (λ (s) (set! str s))))
    (list res str))
  (check-equal? (try "") (list #t #f))
  (check-equal? (try "a") (list #t #f))
  (check-equal? (try "a b") (list #t #f))
  (check-equal? (try "a b") (list #t #f))
  (check-equal? (try (string #\u00A0))
                (list #f
                      "found whitespace that isn't JSON-approved whitespace\n  line: 1\n  column: 1\n  char: #\\u00A0"))
  (check-equal? (try (string #\u00A0 #\u00A0))
                (list #f
                      "found whitespace that isn't JSON-approved whitespace\n  line: 1\n  column: 1\n  char: #\\u00A0"))
  (check-equal? (try (string #\newline #\space #\space #\u00A0))
                (list #f
                      "found whitespace that isn't JSON-approved whitespace\n  line: 2\n  column: 3\n  char: #\\u00A0")))
