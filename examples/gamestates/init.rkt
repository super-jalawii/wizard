#lang racket

(require raylib
         wizard
         "mainmenu.rkt")

(provide (struct-out gamestate/init))

(struct gamestate/init gamestate/default ()
  #:methods gen:gamestate
  [(define (gamestate:init self)
     (set-trace-log-level! log::info)
     (trace-log log::info "gamestate/init starting")
     (set-target-fps! (config-ref 'target-fps)))
   (define (gamestate:update self)
     (send! (state::push (gamestate/main-menu))))])
