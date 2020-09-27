#lang racket/base

(provide config:get
         config:set
         *config*)

(define *config* (make-parameter (make-hash)))

(define (config:get key)
  (hash-ref (*config*) key))

(define (config:set key val)
  (hash-set! (*config*) key val))
