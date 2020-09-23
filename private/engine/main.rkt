#lang racket/base

(require raylib
         "state.rkt"
         wizard/ecs)

(provide (rename-out [make-Window Window])
         run)

(struct Window (width height title fullscreen show-logo))

(define [make-Window width height title
                     #:fullscreen [fullscreen #f]
                     #:show-logo [show-logo #f]]
  (Window width height title fullscreen show-logo))

(define [Window-init win]
  (init-window (Window-width  win)
               (Window-height win)
               (Window-title  win)))

(define *gsm* (make-parameter (GameStateManager)))

(define [gameloop ticks-per-second max-frameskip]
  (define ticks-per-milli (ticks-per-second . / . 1000))
  (define skip-ticks ticks-per-milli)
  (let outer-loop ([next-game-tick (elapsed-time)])
    (unless (window-should-close?)
      (gsm:handle-input (*gsm*))

      (let gamestep ([step 0])
        (when (and (> (elapsed-time) next-game-tick)
                   (< step max-frameskip))

          (gsm:update (*gsm*))

          (set! next-game-tick (+ next-game-tick skip-ticks))
          (gamestep (add1 step))))

      (gsm:draw (*gsm*))

      (outer-loop next-game-tick))))

(define [run win
             #:init-gs [init-gs #f]
             #:ticks-per-second [ticks-per-second 60]
             #:max-frameskip [max-frameskip 10]]
  (parameterize ([*gsm* (GameStateManager (list init-gs))]
                 [*world* (make-hash)]
                 [*next-eid* 0])
    (Window-init win)
    (gameloop ticks-per-second max-frameskip)))
