#lang racket/base

(require raylib ;  NOTE: This is just so we can call close-window and stuff
         wizard/engine
         "config.rkt"
         "gamestates/init.rkt")

(define (make-window)
  (window (config-ref 'window-width)
          (config-ref 'window-height)
          (config-ref 'window-title)
          #:fullscreen (config-ref 'fullscreen?)))

(define (start)
  (parameterize ([*config* config])
    (run (make-window)
         (gamestate/init))))

