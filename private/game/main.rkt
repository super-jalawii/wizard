#lang racket

(require wizard)

(provide (struct-out Player)
         (struct-out Position)
         (struct-out Solid)
         collision?
         set-Position!)

(define/component Player   ())
(define/component Position (x y))
(define/component Solid    ())

(define [collision? x y]
  (for/or ([ent (ecs-query (compose (has?* '(Solid Position))
                                    (into-resultset)
                                    (with-component 'Position))
                           #:init (Component-entities 'Position))])
    (let ([pos (list-ref ent 1)])
      (and (eq? (Position-x pos) x)
           (eq? (Position-y pos) y)))))

(define [set-Position! pos x y]
  (set-Position-x! pos x)
  (set-Position-y! pos y))
