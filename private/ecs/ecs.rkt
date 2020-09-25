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
         *world*
         *next-eid*
         has?
         has?*
         ecs-query
         (rename-out [simple-component-query ecs-simple-query])
         Component-entity?
         Component-for-entity
         Component-all
         Component-entities
         into-resultset
         with-component
         let/ecs)

(define [Component-entity? comp eid]
  (storage:entity? (get-storage comp)
                   eid))

(define [Component-for-entity comp eid]
  (storage:for-entity (get-storage comp)
                      eid))

(define [Component-all comp]
  (storage:components (get-storage comp)))

(define [Component-entities comp]
  (storage:entities (get-storage comp)))

(define-generics storage
  (storage:entity? storage eid)
  (storage:for-entity storage eid)
  (storage:update storage eid data)
  (storage:entities storage)
  (storage:components storage))

(struct ComponentStorage (type))

(struct HashComponentStorage ComponentStorage ([data #:mutable])
  #:methods gen:storage
  [(define [storage:entity? self eid]
     (hash-has-key? (HashComponentStorage-data self) eid))
   (define [storage:for-entity self eid]
     (hash-ref (HashComponentStorage-data self) eid #f))
   (define [storage:update self eid comp]
     (hash-set! (HashComponentStorage-data self) eid comp))
   (define [storage:entities self]
     (hash-keys (HashComponentStorage-data self)))
   (define [storage:components self]
     (hash-values (HashComponentStorage-data self)))])

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
    [(_ name:id (fields:expr ...)
        ;; We can "forward" additional struct attributes like this. Implemented
        ;; mostly for being able to implement generic interfaces.
        other-stuff ...)
     #'(begin
         (struct name (fields ...)
           #:mutable
           #:transparent
           #:methods gen:component
           [(define [component:type self] (quote name))]
           other-stuff ...)
         (hash-set! (*world*)
                    (quote name)
                    (HashComponentStorage (quote name)
                                          (make-hash))))]))

(define *world* (make-parameter (make-hash)))
(define *next-eid* (make-parameter 0))

(define [storage:for-entities storage eids]
  (for/list ([eid (in-list eids)]
             #:when (storage:entity? storage eid))
    (storage:for-entity storage eid)))

(define [get-next-eid]
  (*next-eid* (add1 (*next-eid*)))
  (*next-eid*))

(define/component Entity (eid))

;; FIXME: This ... is really broken. By doing this we can't have any other type
;; of component storage. A better solution might be to maintain the component
;; storage definitions separate from their particular instances in *world*.
;; Alternatively, the components themselves could store the appropriate storage
;; constructor as a struct-type-property. We could then ask the struct to
;; construct it's own self. Since we only have component names here, we would
;; still need to maintain a mapping...
(define [get-storage comp]
  (unless (hash-has-key? (*world*) comp)
    (hash-set! (*world*) comp (HashComponentStorage comp (make-hash))))
  (hash-ref (*world*) comp))

(define [define/entity . components]
  (let* ([eid (get-next-eid)]
         [ent (Entity eid)])
    (storage:update (get-storage 'Entity) eid ent)
    (map (λ (comp) (storage:update (get-storage (component:type comp)) eid comp))
         components)
    ent))

;; Transducer functions ---------------------------------------------

(define [has? component-type]
  (let ([stg (get-storage component-type)])
    (xform/filter (λ (eid) (storage:entity? stg eid)))))

(define [has?* stgs]
  (apply compose (map has? stgs)))

(define [into-resultset]
         (xform/map (λ (eid) (cons eid '()))))

(define [with-component component-type]
  (let ([stg (get-storage component-type)])
    (xform/map (λ (ent)
                 (cons (car ent)
                       (cons (storage:for-entity stg (car ent))
                             (cdr ent)))))))


;; Macro Interface --------------------------------------------------

;; TODO: This version hasn't yet been implemented.

#;(let/ecs xform ([pos Position]
                [_   Player]
                [(Display ch fg bg) Display])

         ...do stuff here...)

;; Both variants of these macros provide a list of component "bindings" which
;; can be used in the body of the let/ecs form. An important detail is in the
;; third component in the example. This demonstrates that we should be able to
;; use a destructuring bind to actually disassemble a component struct for more
;; convenience.

;; NOTE: This version of the macro is intentionally leaving out anything to do
;; with relations since it's unclear how to represent them as identifiers.

;; Maybe the name of the relation itself would be bound, and produce something
;; like: (('from . '(list of entities)) ('to . '(list of entities))) The idea
;; being that we would destructure this in the bind. Anyway - saving for later.

;; NOTE: #:init is the set of initial entities - since transducer pipelines are
;; opaque, we can't "look into it" to see what the first step is - nor can we
;; guarantee that the first step even involves storages.
(define [ecs-query
         xform
         #:init (init (storage:entities (get-storage 'Entity)))]
  (transduce xform (completing cons) '() init))

(define [simple-component-query
         comps
         #:skip-first (skip-first? #f)]
  (let ([xforms (list (into-resultset)
                      (apply compose
                             (map (λ (c) (with-component c))
                                  comps)))])
    (apply compose (if (and skip-first? (null? (cdr comps)))
                       xforms
                       (cons (has?* (if skip-first? (cdr comps) comps))
                             xforms)))))

(define-syntax [let/ecs stx]
  (syntax-parse stx
    [(_ ([bind:expr comp:id] ...) body ...)

     #:with comps #''(comp ...)
     #:with init  #'(storage:entities (get-storage (car comps)))
     #:with bindings #`#,(for/list ([c (in-list (syntax-e #'(comp ...)))]
                                    [b (in-list (reverse
                                                 (syntax-e #'(bind ...))))]
                                    [i (in-naturals)])
                           #`(#,b (list-ref ents #,(+ 1 i))))

     #`(let ([xform (simple-component-query comps #:skip-first #t)])
         (for/list ([ents (in-list (ecs-query xform #:init init))])
           (match-let (#,@#'bindings)
             body ...)))]))











