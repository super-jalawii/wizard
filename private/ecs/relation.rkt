#lang racket

(require graph
         (for-syntax syntax/parse
                     racket/syntax))

(provide define/relation)

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


