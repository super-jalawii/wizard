#lang racket/base

(provide config-ref
         config-set!
         *config*)

(define default-config (make-hash
                        '((target-fps . 60)
                          (window-width . 1600)
                          (window-height . 800)
                          (window-title . "Wizard Engine")
                          (fullscreen? . #f)
                          (ticks-per-second . 24)
                          (max-frameskip . 10)
                          (gfx-scale-x . 4)
                          (gfx-scale-y . 4))))

(define *config* (make-parameter default-config))

(define (config-ref key)
  (hash-ref (*config*) key))

(define (config-set! key val)
  (hash-set! (*config*) key val))
