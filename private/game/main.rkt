#lang racket

(require racket/generic
         wizard)

(provide (struct-out Player)
         (except-out (struct-out Position) set-Position-x! set-Position-y!)
         (struct-out Solid)
         collision?
         Position-query-point
         Position-query-region
         set-Position!
         debug:draw-quadtree
         (rename-out [update-Position-x! set-Position-x!]
                     [update-Position-y! set-Position-y!]))

(struct SpatialComponentStorage HashComponentStorage (quadtree)
  #:methods gen:storage
  ;; FIXME: This is assuming position - but it's not supposed to...
  [(define [storage:entity? self eid]
     (hash-has-key? (HashComponentStorage-data self) eid))
   (define [storage:for-entity self eid]
     (hash-ref (HashComponentStorage-data self) eid #f))
   (define [storage:entities self]
     (hash-keys (HashComponentStorage-data self)))
   (define [storage:components self]
     (hash-values (HashComponentStorage-data self)))
   (define [storage:update self eid comp]
       (QuadTree-remove! (SpatialComponentStorage-quadtree self) comp)
       (QuadTree-insert! (SpatialComponentStorage-quadtree self) comp)
       (hash-set! (HashComponentStorage-data self) eid comp))])

;; FIXME: Parameterize the initial bounds. This is the maximum size of a map I
;; think.
(define [make-SpatialComponentStorage]
  (SpatialComponentStorage (make-hash)
                           (QuadTree (AABB 0 0 1028 1028))))

(define [spatial-query-point storage point]
  (QuadTree-at-point (SpatialComponentStorage-quadtree storage) point))

(define [spatial-query-region storage region]
  (QuadTree-in-region (SpatialComponentStorage-quadtree storage) region))

(define/component Player   ())
(define/component Solid    ())
(define/component Position (x y)
  #:indexed #t
  #:property prop:ctor make-SpatialComponentStorage
  #:methods gen:spatial
  [(define [spatial:get-origin self]
     (Point (Position-x self) (Position-y self)))])

(define [Position-query-point point]
  (spatial-query-point (Component-storage struct:Position) point))

(define [Position-query-region region]
  (spatial-query-region (Component-storage struct:Position) region))

(define [Position-quadtree]
  (SpatialComponentStorage-quadtree (Component-storage struct:Position)))

(define [update-Position-x! pos x]
  (QuadTree-remove! (Position-quadtree) pos)
  (set-Position-x! pos x)
  (QuadTree-insert! (Position-quadtree) pos))

(define [update-Position-y! pos x]
  (QuadTree-remove! (Position-quadtree) pos)
  (set-Position-y! pos x)
  (QuadTree-insert! (Position-quadtree) pos))


(define (debug:draw-quadtree)
  (local-require raylib)
  (let ([qt (Position-quadtree)])
    (tree:each qt (λ (node)
                    (let ([bounds (quadtree:bounds node)])
                      (draw-rect-lines (exact-truncate (* 20 (AABB-x bounds)))
                                       (exact-truncate (* 20 (AABB-y bounds)))
                                       (exact-truncate (* 20 (AABB-w bounds)))
                                       (exact-truncate (* 20 (AABB-h bounds)))
                                       WIZARDWHITE)
                      (for ([pt (in-list (tree:data node))])
                        (draw-circle (* 20 (Position-x pt))
                                     (* 20 (Position-y pt))
                                     2.
                                     RED)))))))

;; FIXME: I think we can make this alot more efficient with the quadtree implemented.
#;(define [collision? x y]
  (for/or ([ent (ecs-query (compose (has?* '(Solid Position))
                                    (into-resultset)
                                    (with-component struct:Position))
                           #:init (Component-entities struct:Position))])
    (let ([pos (list-ref ent 1)])
      (and (eq? (Position-x pos) x)
           (eq? (Position-y pos) y)))))

(define (collision? x y)
  (for/or ([ent (in-list (Position-query-point (Point x y)))])
    (Component-entity? struct:Solid (IndexedComponent-eid ent))))

;; FIXME: This is stupid.
(define [set-Position! pos x y]
  (set-Position-x! pos x)
  (update-Position-y! pos y))
