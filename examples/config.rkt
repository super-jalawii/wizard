#lang racket

(provide (all-defined-out))

(define config (make-hash '((target-fps . 60)
                            (window-width . 1600)
                            (window-height . 800)
                            (window-title . "Hello")
                            (fullscreen? . #f)
                            (ticks-per-second . 24)
                            (max-frameskip . 10)
                            (game-title . "Untitled~nGame~nGame")
                            (gfx-scale-x . 4)
                            (gfx-scale-y . 4))))
