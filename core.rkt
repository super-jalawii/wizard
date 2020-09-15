#lang racket

(require "ecs.rkt")

;; Relations

;; In Inform, these relations are mutually exclusive. I don't know if we need to
;; handle that explicitly (or if it's even necessary), just mentioning it for
;; completeness.
(define/relation Containment   #:from contains      #:to contained-by)
(define/relation Support       #:from supports      #:to supported-by)
(define/relation Incorporation #:from incorportates #:to part-of)
(define/relation Carrying      #:from carries       #:to carried-by)
(define/relation Wearing       #:from wears         #:to worn-by)

(define/relation Possession    #:from possesses     #:to possessed-by)
; FIXME: This sounds similar to the carrying relation. What's the difference?
;; ^^^ The difference (from the Inform Book Ch. 13 Section 4) is that possession
;; is actually a shorthand for the union of carrying and wearing.

;; TODO: Adjacency in the Inform sense includes the direction as a sort of
;; parameter to the relation. What is that concept really?

;; ^^ Regarding this: Reading through the entire bit about relations, there is
;; nothing where relations have any kind of associated weight or value. If we
;; want to express this kind of thing, we could organize the directions into
;; their opposites and create relations for each. Then the adjacency relation
;; becomes a synthetic relation which just refers each directional relation.
(define/relation Adjacency     #:by   adjacent-to)
(define/relation Visibility    #:from can-see       #:to seen-by)
(define/relation Touchability  #:from can-touch     #:to touchable-by)

;; Defining directional relations:
(define/relation East-West     #:by east-of         #:to west-of)
(define/relation North-South   #:by north-of        #:to south-of)
(define/relation Up-Down       #:by above           #:to below)

;; ^^ We can do the diagonals as well, this is just the POC.

;; NOTE: Some relations aren't really proper relations like this, they're more
;; like predicates over existing relations. We can handle this somewhat easily,
;; but we may need a macro to make it feel natural.

;; The possession relation above is an example, and so we can define it like
;; this instead:

#;(define/relation Possession
  #:from possesses    (or (carries?)    (wears?))
  #:to   possessed-by (or (carried-by?) (worn-by?)))

;; ^^ The definition might need to be tweaked a little, but we're essentially
;; defining a predicate which tells us if a given entity participates in this
;; "synthetic" relation.

;; An important thing to note is that the Possession relation would be
;; read-only. We can't make it true because we would have to decide how to make
;; it true. In this case we could just pick I suppose but I imagine in most
;; cases that wouldn't really work.

