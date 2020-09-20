#lang racket

(provide (rename-out [make-vec3 vec3]) (except-out (all-defined-out) vec3 make-vec3))

(struct vec3 (x y z) #:transparent)

(define (make-vec3 x y z)
  (vec3 (+ 0. x)
        (+ 0. y)
        (+ 0. z)))

(define [vec3-scalar-op op v n]
  (vec3
   (op (vec3-x v) n)
   (op (vec3-y v) n)
   (op (vec3-z v) n)))

(define [vec3-scalar-subtract v n]
  (vec3-scalar-op - v n))


(define [vec3-scalar-divide v n]
  (vec3
   (/ (vec3-x v) n)
   (/ (vec3-y v) n)
   (/ (vec3-z v) n)))

(define [vec3-subtract v1 v2]
  (vec3
   (- (vec3-x v1) (vec3-x v2))
   (- (vec3-y v1) (vec3-y v2))
   (- (vec3-z v1) (vec3-z v2))))


(define [vec3-add v1 v2]
  (vec3
   (+ (vec3-x v1) (vec3-x v2))
   (+ (vec3-y v1) (vec3-y v2))
   (+ (vec3-z v1) (vec3-z v2))))

;; Returns true when v1 is closer to the origin than v2
(define [vec3-lt v1 v2]
  (and (< (vec3-x v1) (vec3-x v2))
       (< (vec3-y v1) (vec3-y v2))
       (< (vec3-z v1) (vec3-z v2))))

(struct bounding-box (min max) #:transparent)

(define [dimensions bbox]
  (vec3-subtract (bounding-box-max bbox)
                 (bounding-box-min bbox)))

(define [contains? bb1 bb2]
  (and (vec3-lt (bounding-box-min bb1) (bounding-box-min bb2))
       (vec3-lt (bounding-box-max bb2) (bounding-box-max bb1))))

