#lang racket

(require racket/generic
         racket/stxparam
         (rename-in "transducer.rkt"
                    (map xform/map)
                    (filter xform/filter))
         (for-syntax syntax/parse
                     racket/syntax)
         (rename-in racket/base
                      (map racket/map)
                      (filter racket/filter)))

(provide define/component
         define/entity
         define/relation
         (struct-out Entity)
         has?
         has?*
         into-resultset
         with-component)

;;; Datatypes ------------------------------

(define-generics storage
  (storage:entity? storage eid)
  (storage:for-entity storage eid)
  (storage:update storage eid data))

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

;; Here's an example:

(define/component Bunny ())
(define/component Carrot ())
(define/component Position (x y))
(define/component Armor (ac))
(define/component Item (name))
(define/component Display (ch))

(define/component Spell (icon cast-time))
(define/component Buff (time-remaining))



(define/entity
  (Bunny)
  (Display "@")
  (Position 10 10)
  (Armor 14))

(define/entity
  (Carrot)
  (Display "^")
  (Position 5 5)
  (Armor 4))

(define/entity
  (Carrot)
  (Position 7 13)
  (Display "^"))

(define/entity
  (Item "grenade")
  (Position 1 3)
  (Display "g"))

(define [xform/ent-by-pos x y]
  (compose (has? 'Display)
           (has? 'Position)
           (into-resultset)
           (with-component 'Position)
           (with-component 'Display)
           (xform/filter (λ (ent) (let ([pos (third ent)])
                                    (and (eq? (Position-x pos) x) (eq? (Position-y pos) y)))))
           (xform/map (λ (ent) (second ent)))))

;; ------------ DISPLAY -------------------------------

(define [display-grid eids]
  (for ([x (in-range 20)])
    (for ([y (in-range 20)])
      (let* ([disp (transduce (xform/ent-by-pos x y)
                              (completing cons)
                              '() eids)]
             [ch   (if (null? disp) "." (Display-ch (car disp)))])
        (printf " ~a " ch)))
    (printf "~n")))

;; Relations -------------------------------------------------

#|

Where a component describes a property of an entity in isolation, a relation
describes a property of an entity in the context of another entity. Containment
is an example from Inform of relations. An entity can be "contained by" another
entity, and conversely an entity can "contain" another entity.

The network of relations between entities forms a sort of directed graph, and we
can traverse that graph to answer questions about how an entity is in relation
to other entities.

|#

;; TODO: Maybe there are situations where relations might have a weight
;; associated with them.

;; TODO: How would something like directions be implemented using relations?
;; Where does the north/south/east/west come from?

;; TODO: Some relations are one-to-one. While we already automatically support
;; this in some ways, ideally we would want setting the relation to replace any
;; existing connection. In these relations there can be only one thing
;; satisfying the relation at any particular time.

;; TODO: For the directed variant, it is probably the case that it doesn't make
;; sense to have the reciprocal relation. Our functions could check this. The
;; macro could allow this behaviour to be elided.

;; TODO: Inform has the concept of relating groups of people to each other. The
;; example given uses the text: "Nationality relates people to each other in
;; groups." I'm not really sure how to interpret this.

;; TODO: Inform can express relations between values. This is mostly just a
;; party trick to make the text easier to read and I don't think we need it, but
;; I include it here as something to consider.

;; TODO: Inform allows a bit of static type checking with regards to the types
;; of the members of a relation. In some cases, a relation relates two different
;; types of data. It's hard for us to talk about what types participate in these
;; things since we basically have: things and values. But we could provide
;; predicates if we really wanted.

(require graph)

(define-syntax [define/relation stx]
  (syntax-parse stx
    [(_ relation:id #:by relname)
     #:with rel-pred (format-id #'relation "~a?" #'relname)
     #:with rel-get  (format-id #'relation "get-~a" #'relname)
     #:with rel-trav (format-id #'relation "traverse-by-~a" #'relname)
     #'(begin
         (define relation (unweighted-graph/undirected '()))
         (define [rel-pred x y]
           (for/or ([n (in-neighbors relation x)]) (eq? n y)))
         (define [rel-get x]
           (get-neighbors relation x))
         (define [relname x y]
           (add-edge! relation x y))
         (define [rel-trav x y]
           (fewest-vertices-path relation x y)))]
    [(_ relation:id #:from from-rel:id #:to to-rel:id)
     #:with from-pred (format-id #'relation "~a?" #'from-rel)
     #:with from-get  (format-id #'relation "get-~a" #'from-rel)
     #:with from-trav (format-id #'relation "traverse-by-~a" #'from-rel)
     #:with to-pred   (format-id #'relation "~a?" #'to-rel)
     #:with to-get    (format-id #'relation "get-~a" #'to-rel)
     #:with to-trav   (format-id #'relation "traverse-by-~a" #'to-rel)
     #'(begin
         (define relation (unweighted-graph/directed '()))
         (define [from-pred x y]
           (for/or ([n (in-neighbors relation x)]) (eq? n y)))
         (define [from-get x]
           (get-neighbors relation x))
         (define [from-rel x y]
           (add-directed-edge! relation x y))
         (define [from-trav x y]
           (fewest-vertices-path relation x y))
         (define [to-pred x y]
           (from-pred y x))
         (define [to-get x]
           (printf "Not yet implemented..."))
         (define [to-rel x y]
           (add-directed-edge! relation y x))
         (define [to-trav x y]
           (fewest-vertices-path relation y x)))]))


;; Note that since these relations define predicates, we should be able to add
;; them into transducer pipelines somewhat easily. The only time we will get
;; into trouble is when we want to get all of the entities of the relation
;; simply because we haven't worked out the "cat" transducer function.


