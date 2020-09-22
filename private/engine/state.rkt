#lang racket

(require wizard/ecs
         racket/generic
         racket/provide
         raylib)

(provide (struct-out GameState)
         (matching-identifiers-out #rx"GameState::.*" (all-defined-out))
         gen:game-state
         gsm:handle-input
         gsm:update
         gsm:draw
         (rename-out [make-GameStateManager GameStateManager]))

(define/event GameState ([Push ele]
                         [Pop]
                         [TransitionTo ele]))

(struct Stack (data) #:mutable)

(define [push! stack ele]
  (set-Stack-data! stack (cons ele (Stack-data stack))))

(define [pop! stack]
  (let ([state (car (Stack-data stack))])
    (set-Stack-data! stack (cdr (Stack-data stack)))))

(define [replace! stack ele]
  (pop! stack)
  (push! stack ele))

(define [current gsm]
  (car (Stack-data (GameStateManager-stack gsm))))

(define [do-nothing gs] (void))

(define-generics game-state
  (game-state:init          game-state)
  (game-state:cleanup       game-state)
  (game-state:pause         game-state)
  (game-state:resume        game-state)
  (game-state:handle-input  game-state)
  (game-state:update        game-state)
  (game-state:draw          game-state)
  #:fallbacks
  [(define game-state:init         do-nothing)
   (define game-state:cleanup      do-nothing)
   (define game-state:pause        do-nothing)
   (define game-state:resume       do-nothing)
   (define game-state:handle-input do-nothing)
   (define game-state:update       do-nothing)
   (define game-state:draw         do-nothing)])

(struct GameState () #:methods gen:game-state [])
(struct IdleGameState GameState ()
  #:methods gen:game-state
  [(define [game-state:draw self]
     (draw-begin)
     (clear-background WIZARDWHITE)
     (draw-text "Idle." 20 20 20 BLACK)
     (draw-end))])

(struct GameStateManager (stack) #:mutable)

(define [gsm:update gsm]
  (let ([stack (GameStateManager-stack gsm)]
        [curr  (current gsm)])
    ;; Check for GameState events and respond to them as needed.
    (recv GameState
          [(GameState::Push state) (game-state:pause curr)
                                   (push! stack state)
                                   (game-state:init state)]
          [(GameState::Pop) (game-state:cleanup curr)
                            (pop! stack)
                            (game-state:resume (current stack))]
          [(GameState::TransitionTo next) (game-state:cleanup curr)
                                          (replace! stack next)
                                          (game-state:init next)]))
  (clear GameState)
  (game-state:update (current gsm)))

(define [gsm:handle-input gsm] (game-state:handle-input (current gsm)))
(define [gsm:draw         gsm] (game-state:draw         (current gsm)))

(define [make-GameStateManager [initial-states (list (IdleGameState))]]
  (GameStateManager (Stack initial-states)))
