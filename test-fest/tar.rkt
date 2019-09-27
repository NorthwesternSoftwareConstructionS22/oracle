#lang at-exp racket

(provide (all-defined-out))

(require "util.rkt"
         "test-fest-data.rkt")

(define/contract (zip-repos! repo-map #:delete-original? [delete-original? #t])
  ({(hash/c repo-name? path-to-existant-directory?)}
   {#:delete-original? boolean?}
   . ->* .
   (hash/c repo-name? path-to-existant-file?))

  (for/hash ([(name zip) (in-hash repo-map)])
    (values name (zip! zip #:delete-original? delete-original?))))

(define/contract (zip! dir #:delete-original? [delete-original? #t])
  ({path-to-existant-directory?}
   {#:delete-original? boolean?}
   . ->* .
   path-to-existant-file?)

  (system @~a{tar -czf @|dir|.tar.gz @dir})
  (when delete-original?
    (delete-directory/files dir))
  (path-replace-extension dir ".tar.gz"))

(define/contract (unzip-repos! repo-caches)
  ((hash/c repo-name? path-to-existant-file?)
   . -> .
   (hash/c repo-name? path-to-existant-directory?))

  (for/hash ([(name zip) (in-hash repo-caches)])
    (values name (unzip! zip))))

(define/contract (unzip! zip-file)
  (path-to-existant-file? . -> . path-to-existant-directory?)

  (define-values {containing-dir filename _2} (split-path zip-file))
  (parameterize ([current-directory containing-dir])
    (system @~a{tar -xzf @filename}))
  ;; twice to remove .tar.gz
  (path-replace-extension (path-replace-extension zip-file "")
                          ""))

(define/contract (find-repo-cache-file assign)
  (assign-number? . -> . path-string?)

  @~a{@(assign-number->string assign).cache})
