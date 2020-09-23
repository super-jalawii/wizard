#lang racket

(require wizard
         raylib)

(define/component Position (x y))
(define/component Tooltip (text))

(define [draw-tooltips]
  (let/ecs ([(Tooltip text) Tooltip ]
            [(Position x y) Position])
           (draw-text text x y 20 WIZARDWHITE)))

