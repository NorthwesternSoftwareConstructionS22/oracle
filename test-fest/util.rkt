#lang at-exp racket

(provide (all-defined-out))

(require json)

(define bad-json
  (let ()
    (struct bad ())
    (bad)))

(define (read-json/safe port)
  (with-handlers ([exn:fail:read? (const bad-json)])
    (read-json port)))

(define (valid-json? port)
  (not (eq? (read-json/safe port) bad-json)))

(define (valid-json-file? path)
  (call-with-input-file path
    valid-json?
    #:mode 'text))

(define (jsexpr=? a b)
  (equal? a b)
  #;(or (equal? a b)
      (match* {a b}
        [{(? list?) (? list?)}
         (unordered-list=? a b jsexpr=?)]
        [{(? hash-eq?) (? hash-eq?)}
         (and (set=? (hash-keys a) (hash-keys b))
              (for/and ([(k v/a) (in-hash a)])
                (jsexpr=? v/a (hash-ref b k))))])))

;; (define (unordered-list=? a b [=? equal?])
;;   (and (= (length a)
;;           (length b))
;;        (for/and ([v/a (in-list a)])
;;          (member v/a b =?))))

;; (module+ test
;;   (require rackunit)
;;   (check-true (unordered-list=? '()
;;                                 '()))
;;   (check-true (unordered-list=? '(1)
;;                                 '(1)))
;;   (check-true (unordered-list=? '(1 2 3)
;;                                 '(2 3 1)))

;;   (check-true (jsexpr=? "a" "a"))
;;   (check-true (jsexpr=? #t #t))
;;   (check-true (jsexpr=? 1 1))
;;   (check-true (jsexpr=? 100.2 100.2))
;;   (check-true (jsexpr=? 'null 'null))
;;   (check-false (jsexpr=? #t "#t"))

;;   (check-true (jsexpr=? '(1 ))))



(define (build-path-string . args)
  (path->string (apply build-path args)))

(define (basename p)
  (define-values {_1 name _2} (split-path p))
  (path->string name))
