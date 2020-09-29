#lang racket/base

(require raylib
         "state.rkt"
         "../config.rkt"
         wizard/ecs)

(provide (rename-out [make-Window Window])
         *delta-time*
         run)

(struct Window (width height title fullscreen))

(define (make-Window width height title #:fullscreen [fullscreen #f])
  (Window width height title fullscreen))

(define (Window-init win)
  (if (Window-fullscreen win)
      (begin
        (printf "Fullscreen enabled")
        (set-config-flags! 'FLAG_FULLSCREEN_MODE)
        (init-window (screen-width)
                     (screen-height)
                     (Window-title win)))
      (begin
        (printf "Fullscreen disabled")
        (set-config-flags! 'FLAG_NONE)
        (init-window (Window-width  win)
                     (Window-height win)
                     (Window-title  win)))))

(define Window-close close-window)

(define *gsm* (make-parameter (GameStateManager)))
(define *delta-time* (make-parameter 0))

(define (gameloop)
  (let* ([ticks-per-second (config:get 'ticks-per-second)]
         [max-frameskip    (config:get 'max-frameskip)   ]
         [ticks-per-milli  (ticks-per-second . / . 1000) ]
         [skip-ticks       ticks-per-milli               ]
         [last-frame       (elapsed-time)                ])
    (let outer-loop ([next-game-tick (elapsed-time)])
      (unless (window-should-close?)
        (parameterize ([*delta-time* (* 1000 (- (elapsed-time) last-frame))])
          (set! last-frame (elapsed-time))
          (gsm:handle-input (*gsm*))

          (let gamestep ([step 0])
            (when (and (> (elapsed-time) next-game-tick)
                       (< step max-frameskip))

              (gsm:update (*gsm*))

              (set! next-game-tick (+ next-game-tick skip-ticks))
              (gamestep (add1 step))))

          (gsm:draw (*gsm*))

          (outer-loop next-game-tick))))))

;; FIXME: Run is essentially implementing its own gamestate (where the window is
;; initialized, and closed). Can we make this work like a gamestate too?
(define (run window initial-gamestate)
  (parameterize ([*gsm* (GameStateManager (list initial-gamestate))]
                 #;[*world* (make-hash)]
                 [*next-eid* 0])
    (Window-init window)
    (gameloop)
    (Window-close)))
