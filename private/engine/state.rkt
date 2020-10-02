#lang racket

(require wizard/ecs
         racket/generic
         racket/provide
         raylib)

(provide (struct-out gamestate/default)
         (matching-identifiers-out #rx"state::.*" (all-defined-out))
         gen:gamestate
         gsm:handle-input
         gsm:update
         gsm:draw
         (rename-out [make-gamestate-manager gamestate-manager]))

(define/event state ([push ele]
                     [pop]
                     [into ele]))

(struct stack (data)
  #:constructor-name stack$
  #:mutable)

(define (push! stack ele)
  (set-stack-data! stack (cons ele (stack-data stack))))

(define (pop! stack)
  (let ([state (car (stack-data stack))])
    (set-stack-data! stack (cdr (stack-data stack)))))

(define (replace! stack ele)
  (pop! stack)
  (push! stack ele))

(define (current gsm)
  (car (stack-data (gamestate-manager-stack gsm))))

(define (do-nothing gs) (void))

(define-generics gamestate
  (gamestate:init          gamestate)
  (gamestate:cleanup       gamestate)
  (gamestate:pause         gamestate)
  (gamestate:resume        gamestate)
  (gamestate:handle-input  gamestate)
  (gamestate:update        gamestate)
  (gamestate:draw          gamestate)
  #:fallbacks
  [(define gamestate:init         do-nothing)
   (define gamestate:cleanup      do-nothing)
   (define gamestate:pause        do-nothing)
   (define gamestate:resume       do-nothing)
   (define gamestate:handle-input do-nothing)
   (define gamestate:update       do-nothing)
   (define gamestate:draw         do-nothing)])

(struct gamestate/default ()
  #:methods gen:gamestate [])

(struct gamestate/idle gamestate/default ()
  #:methods gen:gamestate
  [(define (gamestate:draw self)
     (draw-begin)
     (clear-background WIZARDWHITE)
     (draw-text "Idle." 20 20 20 BLACK)
     (draw-end))])

(struct gamestate-manager (stack)
  #:constructor-name gamestate-manager$
  #:mutable)

(define [gsm:update gsm]
  (let ([stack (gamestate-manager-stack gsm)]
        [curr  (current gsm)])
    ;; Check for state events and respond to them as needed.
    (recv state
          [(state::push state) (gamestate:pause curr)
                               (push! stack state)
                               (gamestate:init state)]
          [(state::pop) (gamestate:cleanup curr)
                        (pop! stack)
                        (gamestate:resume (current stack))]
          [(state::into next) (gamestate:cleanup curr)
                              (replace! stack next)
                              (gamestate:init next)]))
  (clear! state)
  (gamestate:update (current gsm)))

(define (gsm:handle-input gsm) (gamestate:handle-input (current gsm)))
(define (gsm:draw         gsm) (gamestate:draw         (current gsm)))

(define (make-gamestate-manager [initial-states (list (gamestate/idle))])
  (gamestate-manager$ (stack$ initial-states)))
