#lang racket

(require (rename-in racket/base
                    (map racket/base/map)
                    (filter racket/base/filter)))

(provide map
         filter
         find
         completing
         reduced
         reduced?
         ensure-reduced
         unreduced
         transduce)

;; TODO: Implement a transducer which creates multiple values like cat
;; (mapcat?).
;; TODO: Implement a stateful transducer like take.

;; Define some basic transducers from list functions
(define map
  (case-lambda
    ([f] (λ [reducing-fn]
           (case-lambda
             ([] (reducing-fn))
             ([result] (reducing-fn result))
             ([input result]
              (reducing-fn (f input) result)))))
    ([proc . lst] (apply racket/base/map proc lst))))

(define filter
  (case-lambda
    ([pred] (λ [reducing-fn]
              (let ([prf (preserving-reduced reducing-fn)])
                (case-lambda
                  ([] (reducing-fn))
                  ([result] (reducing-fn result))
                  ([input result]
                   (if (pred input)
                       (prf input result)
                       result))))))
    ([pred lst] (racket/base/filter pred lst))))

(define find
  (λ [el]
    (λ [reducing-fn]
      (let ([prf (preserving-reduced reducing-fn)])
        (case-lambda
          ([] (reducing-fn))
          ([result] (reducing-fn result))
          ([input result]
           (if (eq? el input)
               (reduced (prf input result))
               result)))))))

;; Implement the "transducing context" for reduction

(define [completing fn]
  (case-lambda
    ([result] result)
    ([input result] (fn input result))))

(struct Reduced [x])

(define [reduced x]
  (Reduced x))

(define [reduced? x]
  (Reduced? x))

(define [ensure-reduced x]
  (if (Reduced? x)
      x
      (Reduced x)))

(define [unreduced x]
  (if (Reduced? x)
      (Reduced-x x)
      x))

;; This is used in the definition of other transducing functions - it just
;; forwards the result when it is reduced. Combined with cooperation from the
;; transduction context, this should allow us to terminate early.
(define [preserving-reduced xform]
  (λ [i r] (if (reduced? r) r (xform i r))))

(define transduce
  (case-lambda
    ([xform f coll] (transduce xform f (f) coll))
    ([xform f init coll]
     (let* ([xf (xform f)]
            [ret (foldl xf init coll)])
       (unreduced (xf (unreduced ret)))))))

