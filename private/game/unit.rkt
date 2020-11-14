#lang racket/base

(require graph
         "../aux/unit.rkt")

(provide (struct-out volume)
         (struct-out ounces)
         (struct-out gallons)
         (struct-out liters)
         (struct-out cubic-feet)
         ->cubic-feet)

(struct volume uom ()
  #:property prop:conversions (unweighted-graph/directed '()))

(struct ounces volume ()
  #:constructor-name $ounces
  #:transparent
  #:property prop:units "oz")

(struct gallons volume ()
  #:constructor-name $gallons
  #:transparent
  #:property prop:units "gal")

(struct liters volume ()
  #:constructor-name $liters
  #:transparent
  #:property prop:units "l")

(struct cubic-feet volume ()
  #:constructor-name $cubic-feet
  #:transparent
  #:property prop:units "ft^3")

(add-conversion-factor struct:liters struct:gallons 125000000/473176473)
(add-conversion-factor struct:gallons struct:cubic-feet 576/77)
(add-conversion-factor struct:ounces struct:gallons 1/128)

;; HACK: This is a hack to get around the fact that our unit of measure stuff can't
;; transcend measures, only units. If you use this wrong, your stuff will be wrong.
(add-conversion-factor struct:feet struct:cubic-feet 1)
;; HACK: This is becoming quickly more and more questionable...
(add-conversion-factor struct:inches struct:cubic-feet 1728)

(define (->cubic-feet x)
  (convert-to x struct:cubic-feet))

(module+ test
  (require rackunit)


  )
