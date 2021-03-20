#lang at-exp racket

(provide zip-repos!
         zip!
         unzip-repos!
         unzip-in-place!)

(require "util.rkt"
         "team-repos.rkt"
         "assignments.rkt")

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

  ;; Need to tar it up as a direct child in order to avoid creating the whole
  ;; absolute path when untar'ed:
  ;; /home/foo/bar/baz/...
  (define-values {containing-dir dir-name}
    (basename dir #:with-directory? #t))
  (parameterize ([current-directory containing-dir])
    (system @~a{tar -czf @|dir-name|.tar.gz @dir-name}))
  (when delete-original?
    (delete-directory/files dir))
  (path-replace-extension dir ".tar.gz"))

(define/contract (unzip-repos! repo-caches)
  ((hash/c repo-name? path-to-existant-file?)
   . -> .
   (hash/c repo-name? path-to-existant-directory?))

  (for/hash ([(name zip) (in-hash repo-caches)])
    (values name (unzip-in-place! zip))))

(define/contract (unzip-in-place! zip-file)
  (path-to-existant-file? . -> . path-to-existant-directory?)

  (define-values {containing-dir filename}
    (basename zip-file #:with-directory? #t))
  (define tar-output
    (parameterize ([current-directory containing-dir])
      (system/string @~a{tar -xzvf @filename})))
  (define extracted-directory-name (first (string-split tar-output "/\n")))
  (build-path containing-dir extracted-directory-name))

(define/contract (find-repo-cache-file assign)
  (assign-number? . -> . path-string?)

  @~a{@(assign-number->string assign).cache})
