#lang racket

(require racket/generic
         wizard
         wizard/res)

(provide (struct-out Player)
         (except-out (struct-out Position) set-Position-x! set-Position-y!)
         (struct-out Solid)
         (struct-out Camera)
         *camera*
         *cam-scale-x* *cam-scale-y*
         *cam-offset-x* *cam-offset-y*
         collision?
         Position-query-point
         Position-query-region
         set-Position!
         draw-visible-sprites
         debug:draw-quadtree
         (rename-out [update-Position-x! set-Position-x!]
                     [update-Position-y! set-Position-y!]))

(struct SpatialComponentStorage HashComponentStorage (quadtree)
  #:methods gen:storage
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
;; think. Please make it a power of 2 or you'll get rounding errors.
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
  (QuadTree-remove! (Position-quadtree) pos)
  (set-Position-x! pos x)
  (set-Position-y! pos y)
  (QuadTree-insert! (Position-quadtree) pos))

(define/component Camera (viewport))

(define *camera* (make-parameter #f))
(define *cam-scale-x* (make-parameter 1))
(define *cam-scale-y* (make-parameter 1))
(define *cam-offset-x* (make-parameter 0))
(define *cam-offset-y* (make-parameter 0))

;; FIXME: This query isn't working - the struct:Blah stuff is probably to blame idk...
(define xform/visible-sprites
  (compose (has? struct:Sprite)
           (into-resultset)
           (with-component struct:Sprite)
           (with-component struct:Position)))

(define (draw-visible-sprites)
  (local-require raylib)
  (let ([origin (make-Vec2 0. 0.)])
    (let* ([pos (Position-query-region (AABB (*cam-offset-x*)
                                             (*cam-offset-y*)
                                             (AABB-w (Camera-viewport (*camera*)))
                                             (AABB-h (Camera-viewport (*camera*)))))]
           [visible-entities (map IndexedComponent-eid pos)])
      (for ([ent (ecs-query xform/visible-sprites
                            #:init visible-entities)])
        (match-let ([(list eid
                           (Position _ x y)
                           (Sprite sx sy w h sprs src-rect)) ent])
          (draw-tex-pro sprs
                        ;; Multiply by size of tiles in spritesheet.
                        src-rect
                        (make-Rect (+ (* (- x (*cam-offset-x*)) 10. (*cam-scale-x*)) 0.)
                                   (+ (* (- y (*cam-offset-y*)) 10. (*cam-scale-y*)) 0.)
                                   (+ (* w (config:get 'gfx-scale-x)) 0.)
                                   (+ (* h (config:get 'gfx-scale-y)) 0.))
                        origin
                        0.
                        ORANGE))))))
