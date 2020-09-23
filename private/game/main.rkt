#lang racket

(require wizard)

(provide (struct-out Position))

(define/component Position (x y))
