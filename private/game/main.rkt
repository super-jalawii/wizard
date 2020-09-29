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


#;(define/event Camera ([TransferCamera camera-eid target-eid speed]))

;; Animation System - First thing is moving an entity from one point to another
;; - we're gonna use this for the camera first.

;; First, we need to define a set of easing functions. The first will be the
;; hardest. Then we can associate an easing function with the component we're
;; moving, (maybe the animation is itself a component).

(define (lerp v0 v1 t)
  (+ (* (- 1 t) v0) (* t v1)))

(define (ease:in-out-sine x)
  (* (- (cos (* pi x)) 1) -0.5))

;; FIXME: key-frames should possibly be a list of animations which are
;; simultanously ongoing. Otherwise, we can have only a single animation at a
;; time for a given entity. But today we start small.
(define/component Animation (key-frames))

;; FIXME: Effect should possibly be a list of effects to be applied
(struct KeyFrame (effect length [current #:auto #:mutable]) #:auto-value 0)

;; Effect should be a function that, given the current progress though the
;; key-frame, produces the appropriate changes.

(define (animation-system dt)
  ;; For each animation, get the current keyframe, and do the thing. If this was
  ;; the last bit of the current keyframe, then delete it (so the next one is
  ;; current). If there are no more keyframes left, delete the component.
  (let/ecs ([(and (var anim)
                  (Animation frames)) Animation]
            [(Entity eid)             Entity   ])
           ;; TODO: Delete the animation component when it's finished.
           (if (null? frames)
               (Component-remove struct:Animation eid)
               (let ([frame (car frames)])
                 (set-KeyFrame-current! frame (min (+ (KeyFrame-current frame) dt)
                                                   (KeyFrame-length frame)))
                 ((KeyFrame-effect frame) eid (/ (KeyFrame-current frame)
                                                 (KeyFrame-length frame)))
                 ;; If the frame is complete, drop it.
                 (when (<= (KeyFrame-length frame) (KeyFrame-current frame))
                   (set-Animation-key-frames! anim (cdr frames)))))))

(define (lerp:between-points from to [easing (λ (x) x)])
  (let ([x-lerp (compose (λ (t) (lerp (Point-x from) (Point-x to) t)) easing)]
        [y-lerp (compose (λ (t) (lerp (Point-y from) (Point-y to) t)) easing)])
    (λ (t) (Point (x-lerp t) (y-lerp t)))))

(define (effect:cycle handle indices)
  (let ([current indices])
    (λ (eid t)
      #;(printf "~n~a ~a ~a" t (* (- (length indices) (length current)) (/ 1 (length indices))) (car current))
      (Component-update (Sprite-by-idx handle (car current)) eid)
      (when (> t (* (- (length indices) (length current)) (/ 1 (length indices))))
        (if (null? (cdr current))
            (set! current indices)
            (set! current (cdr current)))))))

(define (effect:pan from to [easing (λ (x) x)])
  (let ([effect (lerp:between-points from to easing)])
    (λ (eid t)
      (let ([pos (Component-for-entity struct:Position eid)]
            [pt (effect t)])
        (set-Position! pos (Point-x pt) (Point-y pt))))))

#;(define (effect:phase to [easing (λ (x) x)])
  (λ (eid t)
    (let ([]))))

(provide animation-system
         lerp
         lerp:between-points
         ease:in-out-sine
         effect:pan
         effect:cycle
         (struct-out Animation)
         (struct-out KeyFrame))
;; ^^^ The above defines a *function* which will move from the Point "from" to
;; the Point "to", given a value of time.

;; FIXME: !!! So this is interesting ... We do not seem to observe any change in
;; the position component here. But when we move the let/ecs form to *inside*
;; the loop, we do. What does that mean?
#;(let ([current-pt (Point 0 0)]
      [dest-pt (Point 20 15)])
  (define/entity
    (Position 0 0)
    (Animation (list (KeyFrame (effect:pan current-pt dest-pt) 5))))
  (let/ecs ([_                Animation]
            [(Position _ x y) Position ])
           (for ([i (in-range 50)])
             (animation-system 0.1)
             (printf "~nCurrent Position: ~a ~a" x y))))

#;(let ([current-pt (Point 0 0)]
      [dest-pt (Point 20 15)])
  (define/entity
    (Position 0 0)
    (Animation (list (KeyFrame (effect:pan current-pt dest-pt) 5000))))
  (for ([i (in-range 5000)])
    (let/ecs ([_                Animation]
              [(Position _ x y) Position ])
             (animation-system 1)  ;; Time is specified in milliseconds.
             (printf "~nCurrent Position: ~a ~a" x y))))

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
