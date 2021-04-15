#lang racket

#|

Run this script to validate json files in
the current working directory (and subdirectories)

|#

(require json)

(define (main)
  (for ([file (in-directory)])
    (check-file file)))

(define (check-file file)
  (when (and (file-exists? file)
             (regexp-match? #rx#"[.]json$" file))
    (with-handlers ([exn:fail?
                     (λ (exn) (bad-file exn file))])
      (call-with-input-file file
        (λ (port)
          (let loop ()
            (define j (read-json port))
            (unless (eof-object? j) (loop))))))))

(define (bad-file exn file)
  (printf "~a\n   ~a\n" file (exn-message exn)))

(module+ main (main))
