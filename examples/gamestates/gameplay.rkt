#lang racket

(require racket/random
         raylib
         wizard
         wizard/game
         wizard/res)

(provide (struct-out gamestate/gameplay))

(define/component stats (hp stamina))

(define (init gs)
  (trace-log log::info "Loading assets...")
  (load-spritesheet "tiles.png" 'retrodays 160 1450 10 10)
  (trace-log log::info (format "   tiles.png loaded as \"~a\"." 'retrodays))
  (trace-log log::info "Loading entities...")
  (load-entities)
  (trace-log log::info "gamestate/gameplay starting"))

(define (handle-input gs)
  (let/ecs ([_            player]
            [(entity eid) entity])
           (when (key-pressed? key::one) (gs:toggle gs 'debug:show-quadtree))
           (when (key-pressed? key::two) (gs:toggle gs 'debug:show-fps))
           (when (key-pressed? key::three)
             (let/ecs ([_                  player  ]
                       [(position eid x y) position])
                      (component:add (@animation
                                      (list ($key-frame (effect/cycle 'retrodays
                                                                      (sequence->list (in-range 400 410)))
                                                        2000)
                                            ($key-frame (effect/pan ($point x y)
                                                                    ($point x (+ 5 y))) 1500)
                                            ($key-frame (effect/pan ($point x (+ 5 y))
                                                                    ($point (+ 10 x) (+ 5 y))) 1500)
                                            ($key-frame (effect/pan ($point (+ 10 x) (+ 5 y))
                                                                    ($point x y)) 1500)))
                                     eid)))
           (when (key-pressed? key::up)    (send! (&move::adjacent eid  0 -1)))
           (when (key-pressed? key::down)  (send! (&move::adjacent eid  0  1)))
           (when (key-pressed? key::left)  (send! (&move::adjacent eid -1  0)))
           (when (key-pressed? key::right) (send! (&move::adjacent eid  1  0)))))

(define (update gs)
  (system/animation (*delta-time*))
  (let/ecs ([_                   player  ]
            [(and (position _ x y)
                  (var pos))     position])
           ;; FIXME: We're currently ignoring the eid in the message and
           ;; assuming it's the player who's trying to move.
           (recv &move
                 [(&move::adjacent _ dx dy)
                  (let ([x2 (+ x dx)]
                        [y2 (+ y dy)])
                    (unless (collision? x2 y2)
                      (set-position! pos (+ x dx) (+ y dy))))]
                 [(&move::teleport _ x y)
                  (unless (collision? x y)
                    (set-position! pos x y))])
           (clear! &move)))

(define (gs:toggle gs state)
  (hash-set! (gamestate/gameplay-state gs)
             state
             (not (hash-ref (gamestate/gameplay-state gs) state #f))))

(define (gs:set? gs state)
  (hash-ref (gamestate/gameplay-state gs) state #f))

(define [draw gs]
  (let/ecs ([(and (camera (aabb cx cy cw ch))
                  (var camera)) camera]
            [(position _ target-x target-y) position])
           (parameterize ([*cam-scale-x* (/ (config-ref 'window-width) (* cw 10))]
                          [*cam-scale-y* (/ (config-ref 'window-height) (* ch 10))]
                          [*cam-offset-x* (+ target-x cx)]
                          [*cam-offset-y* (+ target-y cy)]
                          [*camera* camera])
             (draw-begin)

             (clear-background BLACK)

             #;(draw-sprites)
             (draw/visible-sprites)

             (when (gs:set? gs 'debug:show-quadtree)
               (debug/draw-quadtree))

             ;; FIXME: Broke this with the camera stuff...
             #;(draw/tooltips)
             (draw/statusbars)

             (when (gs:set? gs 'debug:show-fps)
               (draw-fps 10 10))

             (draw-end))))

(define [cleanup gs]
  (unload-spritesheet 'retrodays))

(struct gamestate/gameplay gamestate/default ([state #:auto #:mutable])
  #:auto-value (make-hash)
  #:methods gen:gamestate
  [(define gamestate:init         init)
   (define gamestate:handle-input handle-input)
   (define gamestate:update       update)
   (define gamestate:draw         draw)
   (define gamestate:cleanup      cleanup)])

(define/event &move ([adjacent eid dx dy]
                     [teleport eid  x  y]))

(define (load-entities)
  (define/entity
    (@position 10 10)
    (sprite-by-idx 'retrodays 1444)
    (@player)
    (@solid)
    (@tooltip "Player" 1 -1)
    (let ([cwidth (/ (config-ref 'window-width) 10. (config-ref 'gfx-scale-x))]
          [cheight (/ (config-ref 'window-height) 10. (config-ref 'gfx-scale-y))])
      (@camera ($aabb (* -1 (/ cwidth 2))
                      (* -1 (/ cheight 2))
                      cwidth
                      cheight)))
    (@stats ($range 7 10) ($range 2 10)))
  (define/entity
    (@window-pos 10 400)
    (@status-bar "HP: ~a/~a"
                 ($target 1 (target-value stats hp))
                 ($gradient RED (vec4->color (make-Vec4 0.2 0. 0. 1.)))))
  (define/entity
    (@window-pos 10 460)
    (@status-bar "STAM: ~a/~a"
                 ($target 1 (target-value stats stamina))
                 ($gradient LIME (vec4->color (make-Vec4 0. 0.2 0. 1.)))))
  (for ([i (in-range 700)])
    (define/entity
      (@position (random 0 160) (random 0 90))
      (sprite-by-idx 'retrodays (random-ref (in-range 1684 1710)))
      (@solid))))
