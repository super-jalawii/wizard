#lang racket

(require graph
         (rename-in (only-in racket/base map filter)
                    (map racket/base/map)
                    (filter racket/base/filter))
         (except-in wizard contains?))
(require "../core.rkt")

(provide *map*

         ent/location
         ent/move-to
         ent/reachable
         ent/reachable?

         room/exits
         room/follow
         (rename-out [room/follow room/towards])

         east/west
         north/south

         connect
         follow-towards)

(define *map* (unweighted-graph/adj '()))
(define-edge-property *map* outbound-dir-name)
(define-edge-property *map* outbound-portal #:init #f)

(define (connect r1 r2 dir #:via [via #f] )
  (printf "~nConnecting rooms ~a and ~a via ~a" r1 r2 via)
  (add-directed-edge! *map* r1 r2)
  (add-directed-edge! *map* r2 r1)
  (outbound-dir-name-set! r1 r2 (car dir))
  (outbound-dir-name-set! r2 r1 (cdr dir))
  (when via
    (outbound-portal-set! r1 r2 via)
    (outbound-portal-set! r2 r1 via)))

(define east/west (cons 'east 'west))
(define north/south (cons 'north 'south))

(define (ent/location [ent (*current-actor*)])
  (car (get-contains ent)))

(define (ent/move-to ent [loc (ent/location)])
  (ent . not-contained-by . 'anything)
  (loc . contains . ent))

(define (room/follow room dir)
  (room . follow-towards . dir))

(define (follow-towards room dir)
  (for/or ([other-room (in-neighbors *map* room)])
    (if (eq? (outbound-dir-name room other-room #:default #f) dir)
        other-room
        #f)))

(define (room/exits room)
  (for/list ([e (in-neighbors *map* room)])
    (outbound-dir-name room e)))

;; Given an entity, return everything reachable by that entity. Reachable here
;; refers to everything in the same location as the entity in question.
;; Reachable includes things on surfaces, and in open containers, but excludes
;; things in closed containers.
(define (ent/reachable [ent (*current-actor*)])
  (let* ([loc (ent/location ent)]
         ;; Add things in the same room
         [things (get-contained-by loc)]
         ;; Add things on surfaces in the room.
         [things (append things
                         (apply append (racket/base/map get-supported-by things)))]
         ;; Add contents of containers in room.
         ;; TODO: Exclude inventories (containers on actor entities).
         [things (append things
                         (apply append (racket/base/map get-contained-by things)))]
         ;; Make sure the entity itself is not listed.
         [things (filter-not (λ (e) (eq? (entity-eid ent) (entity-eid e))) things)]
         ;; TODO: Add contents of inventory of this entity if the entity has an
         ;; inventory.
         )
    things))


(define (ent/reachable? [ent (*current-entity*)]
                    #:by [by (*current-actor*)])
  (member ent (ent/reachable by)))

