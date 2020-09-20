#lang racket

(require racket/generic
         racket/struct-info
         graph)

(provide add-conversion-factor
         convert-to
         uom+ uom-
         uom* uom/
         (struct-out UOM)

         ;; FIXME: These things should really be in another file.
         (struct-out Distance)
         (struct-out Meters)
         (struct-out Kilometers)
         (struct-out Feet)
         ->Feet
         (struct-out Inches))

;; TODO: Add macros to even out the rough edges.

(define-values (prop:conversions conversions? conversions-ref) (make-struct-type-property 'conversions))
(define-values (prop:units       units?       units-ref)       (make-struct-type-property 'units))

(define [uom-printer self port mode]
  (write-string (format "~a~a" (UOM-value self) (units-ref self)) port))

(struct UOM ([value #:mutable])
  #:transparent
  #:property prop:units      #f
  #:methods gen:custom-write [(define write-proc uom-printer)])

(struct Distance UOM ()
  #:property prop:conversions (unweighted-graph/directed '()))

(define-edge-property
  (conversions-ref struct:Distance)
  conversion-factor #:init 1) ;; Default factor is identity for multiplication

(struct Meters Distance ()
  #:transparent
  #:property prop:units "m")

(struct Kilometers Distance ()
  #:transparent
  #:property prop:units "km")

(struct Feet Distance ()
  #:transparent
  #:property prop:units "ft")

(define [->Feet x]
  (convert-to x struct:Feet))

#;(define/uom Feet
  #:measure-of Distance
  #:units "ft"
  #:conversions ([-> Inches : 1/12]
                 [... more conversions here if you want ...]))

(struct Inches Distance ()
  #:transparent
  #:property prop:units "in")

#;(define/conversion meters -> kilometers : 1000)

(define [add-conversion-factor from-uom to-uom factor]
  (let* ([g (conversions-ref from-uom)])
    (add-directed-edge! g from-uom to-uom)
    (conversion-factor-set! from-uom to-uom (/ 1 factor))
    (add-directed-edge! g to-uom from-uom)
    (conversion-factor-set! to-uom from-uom factor)))

(add-conversion-factor struct:Meters struct:Kilometers 1000)
(add-conversion-factor struct:Inches struct:Meters     39.37008)
(add-conversion-factor struct:Inches struct:Feet       12)

;; FIXME: This might work better as a macro but it's hard to tell.
(define [convert-to val uom]
  (let-values ([(from-uom _) (struct-info val)])
    ;; FIXME: This can't be the right way to do this. Surely there's a real way
    ;; to get the constructor for a transparent struct type? Hey, this is our
    ;; code, if we want to, we can just assert that all of our UOM types have
    ;; constructors identical to their names.
    (let* ([ctor (struct-type-make-constructor uom)]
           [factor (aggregate-conversion-factor from-uom uom)])
      (ctor (exact->inexact (* factor (UOM-value val)))))))

(define [smaller-of uom1 uom2]
  (if (> (aggregate-conversion-factor uom1 uom2) 1)
      uom2
      uom1))

(define [aggregate-conversion-factor from-uom to-uom]
  (let ([factor 1]
        [path   (fewest-vertices-path (conversions-ref from-uom)
                                      from-uom
                                      to-uom)])
    (for ([x (in-list path)      ]
          [y (in-list (cdr path))])
      (set! factor (* factor (conversion-factor x y))))
    factor))

(define [normalize-args uom1 uom2]
  ;; Returns uom1 and uom2 such that they share the same unit of measure,
  ;; performing conversions as necessary.
  (let-values ([(type:uom1 x) (struct-info uom1)]
               [(type:uom2 y) (struct-info uom2)])
    (let ([factor (aggregate-conversion-factor type:uom1 type:uom2)])
      (if (eq? (smaller-of type:uom1 type:uom2) type:uom1)
            (let ([ctor (struct-type-make-constructor type:uom1)])
              (values uom1
                      (ctor (* (/ 1 factor) (UOM-value uom2)))))
            (let ([ctor (struct-type-make-constructor type:uom2)])
              (values (ctor (* factor (UOM-value uom1)))
                      uom2))))))

(define [uom-primitive-op op x y]
  (let*-values ([(x y)    (normalize-args x y)]
                [(type _) (struct-info x)])
    ((struct-type-make-constructor type) (op (UOM-value x)
                                             (UOM-value y)))))

(define [uom+ x y] (uom-primitive-op + x y))
(define [uom- x y] (uom-primitive-op - x y))

(define [uom-scalar-op op x y]
  (let-values ([(type _) (struct-info x)])
    ((struct-type-make-constructor type) (op (UOM-value x) y))))

(define [uom* x y] (uom-scalar-op * x y))
(define [uom/ x y] (uom-scalar-op / x y))

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
