#lang racket

(require racket/generic
         racket/struct-info
         graph)

(define Distance (make-parameter (unweighted-graph/directed '())))
(define-edge-property (Distance) conversion-factor #:init 1) ;; Default factor is identity for multiplication

;; FIXME: How do we assign contracts to generic methods? It's probably not hard,
;; I just don't feel like doing it right now.
(define-generics uom (uom:value uom))

(define-values (prop:measure-of measure-of? measure-of-ref) (make-struct-type-property 'measure-of))
(define-values (prop:units      units?      units-ref)      (make-struct-type-property 'units))

(define [uom-printer self port mode]
  (write-string (format "~a~a" (UOM-value self) (units-ref self)) port))

(struct UOM ([value #:mutable])
  #:transparent
  #:property prop:measure-of #f
  #:property prop:units      #f
  #:methods gen:custom-write [(define write-proc uom-printer)])

(struct Meters UOM ()
  #:transparent
  #:property prop:measure-of Distance
  #:property prop:units      "m")

(struct Kilometers UOM ()
  #:transparent
  #:property prop:measure-of Distance
  #:property prop:units      "km")

(struct Inches UOM ()
  #:transparent
  #:property prop:measure-of Distance
  #:property prop:units      "in")

;; How do we ask for the units of a particular type?

#;(define/conversion meters -> kilometers : 1000)

(define [add-conversion-factor from-uom to-uom factor]
  (let* ([g ((measure-of-ref from-uom))])
    (add-directed-edge! g from-uom to-uom)
    (conversion-factor-set! from-uom to-uom (/ 1 factor))
    (add-directed-edge! g to-uom from-uom)
    (conversion-factor-set! to-uom from-uom factor)))

(add-conversion-factor struct:Meters struct:Kilometers 1000)
(add-conversion-factor struct:Inches struct:Meters     39.37008)

(define [pairwise-reduce fn lst]
  (printf "Not yet implemented"))

;; This might work better as a macro but it's hard to tell.
(define [convert-to val uom]
  (let-values ([(from-uom _) (struct-info val)])
    ;; FIXME: This can't be the right way to do this. Surely there's a real way
    ;; to get the constructor for a transparent struct type? Hey, this is our
    ;; code, if we want to, we can just assert that all of our UOM types have
    ;; constructors identical to their names.
    (let* ([ctor (struct-type-make-constructor uom)]
           [path (fewest-vertices-path (Distance) from-uom uom)])
      (for ([x (in-list path)]
            [y (in-list (cdr path))])
        (set-UOM-value! val (* (UOM-value val) (conversion-factor x y))))
      (ctor (UOM-value val)))))

(define [uom+ x y]
  (printf "Not yet implemented"))

;; Unit values should be able to be printed nicely without any input by the
;; developer outside of the initial definition. We shouldn't be afraid to
;; require a format string though. But we should use the Scheme format string
;; functions since I can't figure out where Racket keeps mechanisms like that.

;; This looks like defining a list, but I think we could modify each of the
;; elements to produce a "namespaced" version of each value like: color::red
;; Additionally, each value could evaluate to a symbol version of itself, and we
;; could maintain color itself as a way to know what the next value is in the
;; list. Is there any way for us to store metadata on our 'color::red symbol? So
;; we could for instance remember that it belongs to the color enum?
#;(define/value color '(red green blue))
