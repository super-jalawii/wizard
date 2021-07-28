#lang racket


(require wizard)

(provide (struct-out action)

         (struct-out thing)
         (struct-out container)
         (struct-out surface)
         (struct-out actor)
         (struct-out animal)
         (struct-out room)

         *current-actor*

         get

         ent/contents
         ent/name-of

         ;; TODO: Figure out how to export relations more easily.
         ;; Containment Relation
         contains
         contained-by
         not-contains
         not-contained-by
         contains?
         contained-by?
         get-contains
         get-contained-by
         traverse-by-contains
         traverse-by-contained-by

         ;; Support Relation
         supports
         supported-by
         not-supports
         not-supported-by
         supports?
         supported-by?
         get-supports
         get-supported-by
         traverse-by-supports
         traverse-by-supported-by

         )

(struct action (verb subject object) #:transparent)

(define/component thing (name desc))
(define/component actor ())
(define/component room (name desc)) ; thing and room are identical...
(define/component animal (skeleton))

(define/component container ())
(define/component surface ())

(define/relation containment #:from contains #:to contained-by)
(define/relation support #:from supports #:to supported-by)

(define *current-actor* (make-parameter #f))

(define (get ent comp)
  (component:for-entity comp ent))

(define (ent/contents ent)
  (append (get-contained-by ent)
          (get-supported-by ent)))

(define (ent/name-of obj)
  (if (obj . has? . struct:thing)
      (obj . get . struct:thing)
      (obj . get . struct:room)))

