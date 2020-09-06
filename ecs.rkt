#lang racket

(require racket/generic
         (for-syntax syntax/parse)
         (rename-in racket/base
                      (map racket/map)
                      (filter racket/filter)))

;;; Datatypes ------------------------------

(define-generics storage
  (storage:entity? storage eid)
  (storage:for-entity storage eid)
  (storage:update storage eid data)
  (storage:for-entities storage eids)
  #:fallbacks
  [#;(define [storage:for-entities self eids]
     (for/list ([eid (in-list eids)])
       (let ([data (storage:for-entity self eid)])
         (when data data))))])

;; ^^^ FIXME: This doesn't work - I guess we can't use any methods - these
;; aren't interfaces. :\

(struct ComponentStorage (type))

(struct HashComponentStorage ComponentStorage ([data #:mutable])
  #:methods gen:storage
  [(define [storage:entity? self eid]
     (hash-has-key? (HashComponentStorage-data self) eid))
   (define [storage:for-entity self eid]
     (hash-ref (HashComponentStorage-data self) eid #f))
   (define [storage:update self eid comp]
     (hash-set! (HashComponentStorage-data self) eid comp))])

;; FlaggedComponentStorage is just a wrapper around another storage, with the
;; primary difference being that on insertion, updates, and deletion, an event
;; is fired, allowing systems to respond to these events.

;; How would that even work?? Could we proxy the component (to intercept
;; updates)? For now, we can just fire these events explicitly I guess.
(struct FlaggedComponentStorage ComponentStorage ([stg #:mutable]))

;; TODO: At some point we will add tag-based component storage (for components
;; with no data/properties). For now we keep them all the same.

(define world (make-parameter (make-hash)))

;;; Function Interface ---------------------



;;; Macro Interface ------------------------







