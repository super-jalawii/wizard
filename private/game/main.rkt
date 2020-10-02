#lang racket

(require racket/generic
         wizard
         wizard/res)

(provide (struct-out player)
         (except-out (struct-out position) set-position-x! set-position-y!)
         (struct-out solid)
         (struct-out camera)
         *camera*
         *cam-scale-x*
         *cam-scale-y*
         *cam-offset-x*
         *cam-offset-y*
         collision?
         position-query-point
         position-query-region
         set-position!
         draw/visible-sprites
         debug/draw-quadtree
         (rename-out [update-position-x! set-position-x!]
                     [update-position-y! set-position-y!]))

(struct spatial-component-storage hash-component-storage (quadtree)
  #:constructor-name $spatial-component-storage
  #:methods gen:storage
  [(define [storage:entity? self eid]
     (hash-has-key? (hash-component-storage-data self) eid))
   (define [storage:for-entity self eid]
     (hash-ref (hash-component-storage-data self) eid #f))
   (define [storage:entities self]
     (hash-keys (hash-component-storage-data self)))
   (define [storage:components self]
     (hash-values (hash-component-storage-data self)))
   (define [storage:update self eid comp]
     (quadtree-remove! (spatial-component-storage-quadtree self) comp)
     (quadtree-insert! (spatial-component-storage-quadtree self) comp)
     (hash-set! (hash-component-storage-data self) eid comp))])

;; TODO: Parameterize the initial bounds. This is the maximum size of a map I
;; think. Please make it a power of 2 or you'll get rounding errors.
(define [make-spatial-component-storage]
  ($spatial-component-storage (make-hash)
                              (quadtree ($aabb 0 0 1028 1028))))

(define [spatial-query-point storage point]
  (quadtree-at-point (spatial-component-storage-quadtree storage) point))

(define [spatial-query-region storage region]
  (quadtree-in-region (spatial-component-storage-quadtree storage) region))

(define/component player   ())
(define/component solid    ())
(define/component position (x y)
  #:indexed #t
  #:property prop:ctor make-spatial-component-storage
  #:methods gen:spatial
  [(define [spatial:get-origin self]
     ($point (position-x self) (position-y self)))])

(define [position-query-point point]
  (spatial-query-point (component:storage struct:position) point))

(define [position-query-region region]
  (spatial-query-region (component:storage struct:position) region))

(define [position-quadtree]
  (spatial-component-storage-quadtree (component:storage struct:position)))

(define [update-position-x! pos x]
  (quadtree-remove! (position-quadtree) pos)
  (set-position-x! pos x)
  (quadtree-insert! (position-quadtree) pos))

(define [update-position-y! pos x]
  (quadtree-remove! (position-quadtree) pos)
  (set-position-y! pos x)
  (quadtree-insert! (position-quadtree) pos))

(define (debug/draw-quadtree)
  (local-require raylib)
  (let ([qt (position-quadtree)])
    (tree:each qt (λ (node)
                    (let ([bounds (quadtree-bounds node)])
                      (draw-rect-lines (exact-truncate (* 20 (aabb-x bounds)))
                                       (exact-truncate (* 20 (aabb-y bounds)))
                                       (exact-truncate (* 20 (aabb-w bounds)))
                                       (exact-truncate (* 20 (aabb-h bounds)))
                                       WIZARDWHITE)
                      (for ([pt (in-list (tree:data node))])
                        (draw-circle (exact-truncate (* 20 (position-x pt)))
                                     (exact-truncate (* 20 (position-y pt)))
                                     2.
                                     RED)))))))

(define (collision? x y)
  (for/or ([ent (in-list (position-query-point ($point x y)))])
    (component:entity? struct:solid (indexed-component-eid ent))))

(define [set-position! pos x y]
  (quadtree-remove! (position-quadtree) pos)
  (set-position-x! pos x)
  (set-position-y! pos y)
  (quadtree-insert! (position-quadtree) pos))

(define/component camera (viewport))

(define *camera* (make-parameter #f))
(define *cam-scale-x* (make-parameter 1))
(define *cam-scale-y* (make-parameter 1))
(define *cam-offset-x* (make-parameter 0))
(define *cam-offset-y* (make-parameter 0))

(define (lerp v0 v1 t)
  (+ (* (- 1 t) v0) (* t v1)))

(define (ease/in-out-sine x)
  (* (- (cos (* pi x)) 1) -0.5))

;; FIXME: key-frames should possibly be a list of animations which are
;; simultanously ongoing. Otherwise, we can have only a single animation at a
;; time for a given entity. But today we start small.
(define/component animation (key-frames))

;; FIXME: Effect should possibly be a list of effects to be applied
(struct key-frame (effect length [current #:auto #:mutable])
  #:constructor-name $key-frame
  #:auto-value 0)

;; Effect should be a function that, given the current progress though the
;; key-frame, produces the appropriate changes.

(define (system/animation dt)
  ;; For each animation, get the current keyframe, and do the thing. If this was
  ;; the last bit of the current keyframe, then delete it (so the next one is
  ;; current). If there are no more keyframes left, delete the component.
  (let/ecs ([(and (var anim)
                  (animation frames)) animation]
            [(entity eid)             entity   ])
           (if (null? frames)
               (component:remove struct:animation eid)
               (let ([frame (car frames)])
                 (set-key-frame-current! frame (min (+ (key-frame-current frame) dt)
                                                    (key-frame-length frame)))
                 ((key-frame-effect frame) eid (/ (key-frame-current frame)
                                                  (key-frame-length frame)))
                 ;; If the frame is complete, drop it.
                 (when (<= (key-frame-length frame) (key-frame-current frame))
                   (set-animation-key-frames! anim (cdr frames)))))))

(define (lerp/between-points from to [easing (λ (x) x)])
  (let ([x-lerp (compose (λ (t) (lerp (point-x from) (point-x to) t)) easing)]
        [y-lerp (compose (λ (t) (lerp (point-y from) (point-y to) t)) easing)])
    (λ (t) ($point (x-lerp t) (y-lerp t)))))

(define (effect/cycle handle indices)
  (let ([current indices])
    (λ (eid t)
      (component:update (sprite-by-idx handle (car current)) eid)
      (when (> t (* (- (length indices) (length current)) (/ 1 (length indices))))
        (if (null? (cdr current))
            (set! current indices)
            (set! current (cdr current)))))))

(define (effect/pan from to [easing (λ (x) x)])
  (let ([effect (lerp/between-points from to easing)])
    (λ (eid t)
      (let ([pos (component:for-entity struct:position eid)]
            [pt (effect t)])
        (set-position! pos (point-x pt) (point-y pt))))))

(provide system/animation
         lerp
         lerp/between-points
         ease/in-out-sine
         effect/pan
         effect/cycle
         (struct-out animation)
         (struct-out key-frame))

;; FIXME: !!! So this is interesting ... We do not seem to observe any change in
;; the position component here. But when we move the let/ecs form to *inside*
;; the loop, we do. What does that mean?
#;(let ([current-pt (Point 0 0)]
        [dest-pt (Point 20 15)])
    (define/entity
      (Position 0 0)
      (Animation (list ($key-frame (effect:pan current-pt dest-pt) 5))))
    (let/ecs ([_                Animation]
              [(Position _ x y) Position ])
             (for ([i (in-range 50)])
               (animation-system 0.1)
               (printf "~nCurrent Position: ~a ~a" x y))))

#;(let ([current-pt (Point 0 0)]
        [dest-pt (Point 20 15)])
    (define/entity
      (Position 0 0)
      (Animation (list ($key-frame (effect:pan current-pt dest-pt) 5000))))
    (for ([i (in-range 5000)])
      (let/ecs ([_                Animation]
                [(Position _ x y) Position ])
               (animation-system 1)  ;; Time is specified in milliseconds.
               (printf "~nCurrent Position: ~a ~a" x y))))

;; FIXME: This query isn't working - the struct:Blah stuff is probably to blame idk...
(define xform/visible-sprites
  (compose (has? struct:sprite)
           (into-resultset)
           (with-component struct:sprite)
           (with-component struct:position)))

(define (draw/visible-sprites)
  (local-require raylib)
  (let ([origin (make-Vec2 0. 0.)])
    (let* ([pos (position-query-region ($aabb (*cam-offset-x*)
                                              (*cam-offset-y*)
                                              (aabb-w (camera-viewport (*camera*)))
                                              (aabb-h (camera-viewport (*camera*)))))]
           [visible-entities (map indexed-component-eid pos)])
      (for ([ent (ecs-query xform/visible-sprites
                            #:init visible-entities)])
        (match-let ([(list eid
                           (position _ x y)
                           (sprite sx sy w h sprs src-rect)) ent])
          (draw-tex-pro sprs
                        ; Multiply by size of tiles in spritesheet.
                        src-rect
                        (make-Rect (+ (* (- x (*cam-offset-x*)) 10. (*cam-scale-x*)) 0.)
                                   (+ (* (- y (*cam-offset-y*)) 10. (*cam-scale-y*)) 0.)
                                   (+ (* w (config-ref 'gfx-scale-x)) 0.)
                                   (+ (* h (config-ref 'gfx-scale-y)) 0.))
                        origin
                        0.
                        ORANGE))))))
