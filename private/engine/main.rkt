#lang racket/base

(require raylib
         "state.rkt"
         wizard/config
         wizard/ecs)

(provide (rename-out [make-window window])
         *delta-time*
         run)

(struct window (width height title fullscreen)
  #:constructor-name window$)

(define (make-window width height title #:fullscreen [fullscreen #f])
  (window$ width height title fullscreen))

(define (window-init win)
  (if (window-fullscreen win)
      (begin
        (trace-log log::info "Fullscreen enabled")
        (set-config-flags! window::fullscreen)
        (init-window (screen-width)
                     (screen-height)
                     (window-title win)))
      (init-window (window-width  win)
                   (window-height win)
                   (window-title  win))))

(define window-close close-window)

(define *gsm* (make-parameter (gamestate-manager)))
(define *delta-time* (make-parameter 0))

(define (gameloop)
  (let* ([ticks-per-second (config-ref 'ticks-per-second)]
         [max-frameskip    (config-ref 'max-frameskip)   ]
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
  (parameterize ([*gsm* (gamestate-manager (list initial-gamestate))]
                 #;[*world* (make-hash)]
                 [*next-eid* 0])
    (window-init window)
    (gameloop)
    (window-close)))
