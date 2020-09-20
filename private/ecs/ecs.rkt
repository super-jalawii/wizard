#lang racket

(require racket/generic
         racket/stxparam
         (rename-in "../aux/transducer.rkt"
                    (map xform/map)
                    (filter xform/filter))
         (for-syntax syntax/parse
                     racket/syntax)
         (rename-in racket/base
                      (map racket/map)
                      (filter racket/filter)))

(provide define/component
         define/entity
         (struct-out Entity)
         has?
         has?*
         into-resultset
         with-component)

(define-generics storage
  (storage:entity? storage eid)
  (storage:for-entity storage eid)
  (storage:update storage eid data))

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

(define-generics component
  (component:type component))

(define-syntax [define/component stx]
  (syntax-parse stx
    [(_ name:id (fields:expr ...))
     #'(begin
         (struct name (fields ...)
           #:mutable
           #:transparent
           #:methods gen:component
           [(define [component:type self] (quote name))])
         (hash-set! (world)
                    (quote name)
                    (HashComponentStorage (quote name)
                                          (make-hash))))]))

(define world (make-parameter (make-hash)))
(define next-eid (make-parameter 0))

(define [storage:for-entities storage eids]
  (for/list ([eid (in-list eids)]
             #:when (storage:entity? storage eid))
    (storage:for-entity storage eid)))

(define [get-next-eid]
  (next-eid (add1 (next-eid)))
  (next-eid))

(define/component Entity (eid))

(define [define/entity . components]
  (let* ([eid (get-next-eid)]
         [ent (Entity eid)])
    (storage:update (hash-ref (world) 'Entity) eid ent)
    (map (λ (comp)
           (storage:update (hash-ref (world) (component:type comp))
                           eid
                           comp))
         components)
    ent))

(define [has? component-type]
  (let ([stg (hash-ref (world) component-type)])
    (xform/filter (λ (eid) (storage:entity? stg eid)))))

(define [has?* stgs]
  (apply compose (map has? stgs)))

(define [into-resultset]
         (xform/map (λ (eid) (cons eid '()))))

(define [with-component component-type]
  (let ([stg (hash-ref (world) component-type)])
    (xform/map (λ (ent)
                 (cons (car ent)
                       (cons (storage:for-entity stg (car ent))
                             (cdr ent)))))))

