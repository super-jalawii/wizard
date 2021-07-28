#lang racket

(require graph
         (except-in wizard contains?))
(require "../core.rkt")

(provide *map*

         ent/location
         ent/move-to

         room/exits
         room/follow

         east/west
         north/south

         connect
         follow-towards)

(define *map* (unweighted-graph/adj '()))
(define-edge-property *map* outbound-dir-name)
(define-edge-property *map* outbound-portal #:init #f)

(define (connect r1 r2 dir #:via [via #f] )
  (printf "~nConnecting rooms ~a and ~a via ~a" r1 r2 via)
  (add-directed-edge! *map* r1 r2)
  (add-directed-edge! *map* r2 r1)
  (outbound-dir-name-set! r1 r2 (car dir))
  (outbound-dir-name-set! r2 r1 (cdr dir))
  (when via
    (outbound-portal-set! r1 r2 via)
    (outbound-portal-set! r2 r1 via)))

(define east/west (cons 'east 'west))
(define north/south (cons 'north 'south))

(define (ent/location [ent (*current-actor*)])
  (car (get-contains ent)))

(define (ent/move-to ent [loc (ent/location)])
  (ent . not-contained-by . 'anything)
  (loc . contains . ent))

(define (room/follow room dir)
  (room . follow-towards . dir))

(define (follow-towards room dir)
  (for/or ([other-room (in-neighbors *map* room)])
    (if (eq? (outbound-dir-name room other-room #:default #f) dir)
        other-room
        #f)))

(define (room/exits room)
  (for/list ([e (in-neighbors *map* room)])
    (outbound-dir-name room e)))

