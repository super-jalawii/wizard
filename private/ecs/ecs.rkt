#lang racket

(require racket/generic
         racket/stxparam
         racket/syntax
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
         gen:storage
         prop:ctor
         (struct-out HashComponentStorage)
         (struct-out Entity)
         (struct-out Component)
         (struct-out IndexedComponent)
         *world*
         *next-eid*
         has?
         has?*
         ecs-query
         (rename-out [simple-component-query ecs-simple-query]
                     [get-storage Component-storage])
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

(define-values
  (prop:ctor ctor? ctor-ref)
  (make-struct-type-property 'ctor))
(define-values
  (prop:type type? component:type)
  (make-struct-type-property 'type))

(define (get-storage comp)
  (unless (hash-has-key? (*world*) (component:type comp))
    (hash-set! (*world*) (component:type comp) ((ctor-ref comp))))
  (hash-ref (*world*) (component:type comp)))

(struct Component ())
(struct IndexedComponent Component ([eid #:auto #:mutable]) #:auto-value 0)

(struct ComponentStorage ())

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

(define [make-HashComponentStorage]
  (HashComponentStorage (make-hash)))

(define-syntax [define/component stx]
  (syntax-parse stx
    #:disable-colon-notation
    [(_ name (fields ...)
        (~optional (~seq #:indexed indexed)
                   #:defaults ([indexed #'#f]))
        (~optional (~seq #:property (~literal prop:ctor) ctor-fn)
                   #:defaults ([ctor-fn #'make-HashComponentStorage]))
        ;; We can "forward" additional struct attributes like this. Implemented
        ;; mostly for being able to implement generic interfaces.
        other-stuff ...)
     #`(begin
         (struct name #,(if (syntax-e #'indexed) #'IndexedComponent #'Component) (fields ...)
           #:mutable
           #:transparent
           #:property prop:type (quote name)
           #:property prop:ctor ctor-fn
           other-stuff ...))]))

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

(define [define/entity . components]
  (let* ([eid (get-next-eid)]
         [ent (Entity eid)])
    (storage:update (get-storage struct:Entity) eid ent)
    (map (λ (comp)
           (when (IndexedComponent? comp)
             (set-IndexedComponent-eid! comp eid))
           (storage:update (get-storage comp) eid comp))
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
         #:init (init (storage:entities (get-storage struct:Entity)))]
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

(define/component Position (x y) #:indexed #t)
(define/component Player ())

(define-syntax [let/ecs stx]
  (syntax-parse stx
    [(n ([bind:expr comp:id] ...) body ...)

     #:with comps #'(comp ...)
     #:with structs (map (λ (id) (format-id #'n "struct:~a" id)) (syntax-e #'comps))
     #:with init  #`(storage:entities (get-storage #;struct:Entity #,(car (syntax-e #'structs))))
     #:with bindings #`#,(for/list ([c (in-list (syntax->list #'comps))]
                                    [b (in-list (reverse
                                                 (syntax-e #'(bind ...))))]
                                    [i (in-naturals)])
                           #`(#,b (list-ref ents #,(+ 1 i))))

     #`(let ([xform (simple-component-query #,(cons #'list #'structs) #:skip-first #t)])
         (for/list ([ents (in-list (ecs-query xform #:init init))])
           (match-let (#,@#'bindings)
             body ...)))]))

(define [test]

  (Position 10 10)
  (define/entity (Position 10 10)
    (Player
     ))
  (let/ecs ([(Position eid x y) Position]
            [_ Player])
           (printf "pos: ~a ~a" x y)))









