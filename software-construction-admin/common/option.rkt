#lang at-exp racket

(provide (struct-out failure)
         (struct-out present)
         option/c
         option-let*
         hash-ref/option
         first/option
         fail-if)

(require (for-syntax syntax/parse))

(struct failure (msg) #:prefab)
(struct present (v) #:prefab)

(define (option/c v-ctc)
  (or/c (struct/c present v-ctc) failure? v-ctc))

(define-syntax (option-let* stx)
  (syntax-parse stx
    [(_ {~optional {~seq #:extra-failure-message extra-msg} #:defaults ([extra-msg #'""])}
        [pat:expr maybe-value:expr] more-clauses ... result)
     #'(match maybe-value
         [(failure msg) (failure (~a extra-msg msg))]
         [(or (present pat) pat)
          (option-let* more-clauses ... result)])]
    [(_ {~optional {~seq #:extra-failure-message _}} result) #'(present result)]))

(define/contract (hash-ref/option h k [fail-msg @~a{Key '@k' not found}])
  ({hash? any/c} {string?} . ->* . (option/c any/c))
  (match h
    [(hash-table [(== k) v] _ ...) (present v)]
    [else                          (failure fail-msg)]))

(define (first/option l [failure-msg "empty"])
  (if (empty? l) (failure failure-msg) (first l)))

(define (fail-if bool msg)
  (if bool
      (failure msg)
      (present #f)))
