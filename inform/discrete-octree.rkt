#lang racket

(require wizard)

(require graph)

(define *map* (unweighted-graph/adj '()))
(define-edge-property *map* outbound-dir-name)
(define-edge-property *map* outbound-portal #:init #f)

(define (connect r1 r2 dir #:via [via #f] )
  (printf "Connecting rooms ~a and ~a via ~a" r1 r2 via)
  (add-directed-edge! *map* r1 r2)
  (add-directed-edge! *map* r2 r1)
  (outbound-dir-name-set! r1 r2 (car dir))
  (outbound-dir-name-set! r2 r1 (cdr dir))
  (when via
    (outbound-portal-set! r1 r2 via)
    (outbound-portal-set! r2 r1 via)))

(define room1 "room1")
(define room2 "room2")

(define portal "door")

(define east/west (cons 'east 'west))
(define north/south (cons 'north 'south))
(connect room1 room2 east/west #:via portal)
