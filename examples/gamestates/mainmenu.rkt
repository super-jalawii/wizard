#lang racket

(require raylib
         wizard
         "gameplay.rkt")

(provide (struct-out gamestate/main-menu))

(struct gamestate/main-menu gamestate/default ()
  #:methods gen:gamestate
  [(define (gamestate:init    self) (trace-log log::info "gamestate/main-menu started"))
   (define (gamestate:cleanup self) (trace-log log::info "gamestate/main-menu cleanup invoked"))
   (define (gamestate:draw    self)
     (draw-begin)

     (clear-background BLACK)
     (draw-text (format (config-ref 'game-title)) 20 20 100 WIZARDWHITE)

     (draw-end))
   (define (gamestate:handle-input self)
     (when (key-pressed? key::space)
       (trace-log log::info "Loading next game state...")
       (send! (state::into (gamestate/gameplay)))))])
