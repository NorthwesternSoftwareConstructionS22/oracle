#lang at-exp racket

(provide assign-number?
         assign-number->string
         assign-number->dir-path-part
         assign-major-number
         assign-minor-number)

(require "util.rkt")

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
