#lang racket/base

(require racket/generic
         graph
         (except-in wizard filter)
         (rename-in wizard [filter xform/filter]))

(define ( TODO: [msg "Not yet implemented"]) (error (string-append "TODO: " msg)))

; TODO: Implement an enum type to make this less cumbersome.
(define resource::power 'RESOURCE_POWER)
(define resource::petrol 'RESOURCE_PETROL)

(struct m-socket (id res bw amt)
  #:constructor-name $m-socket)

(struct m-socket/in m-socket ()
  #:constructor-name $m-socket/in
  #:transparent)

;; Identifier
;; Resource
;; Bandwidth (Maximum throughput per step)
;; Amount - The amount of the resource currently stored
(struct m-socket/out m-socket ()
  #:constructor-name $m-socket/out
  #:transparent)

(define/component m-part (inputs outputs ready-fn process-fn))


(define/component m-consumer (inputs))
(define/component m-producer (outputs))
(define/component m-source ())

;; Control Nodes
(define/component m-consumer/ctl (inputs))
(define/component m-producer/ctl (outputs))
(define/component m-source/ctl ())

(define *socket-id* (make-parameter 0))

(define (next-socket-id)
  (*socket-id* (add1 (*socket-id*)))
  (*socket-id*))

(define (make-m-socket/in res bw)
  (let ([id (next-socket-id)])
    ($m-socket/in id res bw 0)))

(define (make-m-socket/out res bw)
  (let ([id (next-socket-id)])
    ($m-socket/out id res bw 0)))

(define conduit::hose 'CONDUIT_HOSE)

; TODO: Remove this vvv I think we don't need to differentiate which machine
; something belongs to.
(struct machine (graph) #:constructor-name $machine)
(struct conduit (ctype len amt)
  #:constructor-name $conduit
  #:transparent)

(define (make-conduit ctype len)
  ($conduit ctype len 0))

(define mach-graph (unweighted-graph/directed '()))
(define-edge-property mach-graph conduit-between)
(define-edge-property mach-graph socket/out)
(define-edge-property mach-graph socket/in)

(define *mach-graph* (make-parameter mach-graph))

(define (add-mach-part part)
  (add-vertex! (*mach-graph*) part))

(define (connect from-part from-socket
                 to-part to-socket
                 via-conduit)
  (add-directed-edge! (*mach-graph*) from-part to-part)
  (conduit-between-set! from-part to-part via-conduit)
  (socket/out-set! from-part to-part from-socket)
  (socket/in-set! from-part to-part to-socket))

(define (connected-sockets part #:input [input? #f] #:output [output? #f])
  (let* ([mc (and input?  (component:for-entity struct:m-consumer part))]
         [mp (and output? (component:for-entity struct:m-producer part))]
         [sockets (append (if mc (m-consumer-inputs mc) '())
                          (if mp (m-producer-outputs mp) '()))]
         [connections (for/list ([n (in-neighbors (*mach-graph*) part)])
                        (cons (socket/in part n) (socket/out part n)))])
    ; Return the list of sockets filtered by the list of sockets which appear in
    ; a connection.
    (filter (λ (s) (map (λ (c) (and (not (eq? (m-socket-id s) (m-socket-id (car c))))
                                    (not (eq? (m-socket-id s) (m-socket-id (cdr c))))))
                        connections))
            sockets)))

(define xform/no-connected-inputs
  (xform/filter (λ (p) (null? (connected-sockets p #:input #t)))))

(define (process-machines)
  ; Start with the set of all machine parts which have no connected input
  ; sockets.
  (let ([starting-nodes (ecs-query xform/no-connected-inputs)]
        [finished-nodes '()])

    ; Run each starting node
    (map ??? starting-nodes)

    ; For each connected output socket of a starting node, run the node that
    ; it's connected to, but only if that node is not waiting on any additional
    ; inputs (ie if this is the last input it was waiting for).

    ( TODO:)))

(define (process-part part completed-sockets)
  ; Given a part, run that part, and try running its children (recursively). A
  ; part should refuse to run when it's inputs haven't already run.
  ; As part of running, a part needs to signal to each of its output sockets
  ; that it is finished.
  )

;; 
;; ------------ TESTING ---------------------------------------------------------------
;; 
;; TODO: It would be cool if we had a macro which defined machine parts, but which
;; also returned the identifiers for each input and output socket a la
;; define-values.
;; 

(define gas-tank/output1 (make-m-socket/out resource::petrol ($inches 1/4)))
(define gas-tank (define/entity
                   (@m-source)
                   (@m-producer (list gas-tank/output1))))

(define engine/input1 (make-m-socket/in resource::petrol ($inches 1/8)))
(define engine (define/entity (@m-consumer (list engine/input1))))

(add-mach-part (entity-eid gas-tank))
(add-mach-part (entity-eid engine))

(connect (entity-eid gas-tank) gas-tank/output1
         (entity-eid engine)   engine/input1
         (make-conduit conduit::hose ($feet 4)))

;; Let's talk about connecting a socket to another. From the gameplay and
;; simulation standpoint, there is a conduit involved, and we need a "staging"
;; area to put resources emitted by the "parent" node.

;; What happens when a machine component doesn't use all of the resources it
;; receives during a step? It seems that we may need a way to signal that a child
;; node can't receive any more from that particular node. If possible - it might
;; make for a simpler API if the parent node could just consider the maximum
;; possible resource contribution when emitting.

;; Does the conduit actually contain resources? If so, does the capacity depend
;; on the length? Does it vary?

;;                                         vvv I don't know what goes here...
;; Gas Tank -> hose -> Engine -> shaft -> Gear Assembly -> axle -> Tire
;;                                                              -> Tire
;;                                                      -> axle -> Tire
;;                                                              -> Tire

;; Gas Tank is a m-source and an m-producer. It's m-socket/out has a diameter
;; which in turn determines both the maximum volume held by conduit, and its
;; throughput at a particular pressure. The length of the conduit is the other
;; factor in this.

;; The volume of a cylinder is found using: pi * (radius ^ 2) * length

;; We can use this to approximate the amount that would be stored in the conduit
;; itself, given a length of conduit.

;; We would need to calculate the diameter using the smaller of the start and end
;; socket diameters, and assume that the conduit is adapted down to the smaller
;; side pretty much immediately.

;; I'm not sure that all of that will be particularly fun... but anyways - let's
;; continue.

;; The Gas Tank is a m-producer which produces an amount of gasoline per step
;; based on:
;; - The amount of fuel left - specifically, it produces no more fuel than it
;;     has.
;; - The number of output sockets actually connected to something - the fuel
;;     should be divided evenly among the outputs.
;; - For each socket, it produces the lesser of:
;;     + The maximum that socket can handle per step (including because of "backpressure").
;;     + The amount of fuel remaining which can be allocated to this socket (see
;;         above).

;; In graph terms, the gas tank is connected to the engine directly, and the
;; conduit is represented as a property of the edge. Additionally, the input and
;; output sockets are also represented as edge properties since they're
;; associated with a specific in/out socket pair.

;; <gas tank: node> -> <fuel hose: conduit> -> <engine: node>

;; We can't just connect components together - in fact we don't connect
;; components together. We connect inputs to outputs. Since we have no way to
;; address components specifically, how can we handle this?

;; One option would be to generate an id assigned to the input and output
;; sockets. Combined with the eid, we could uniquely identify what we're
;; connecting to. We could make inputs and outputs a map between this identifier
;; and the actual socket.

;; There is - I think - a difference between a socket, and what that socket
;; provides. They could be stored in the same structure, but the idea is that the
;; socket is not only what you plug things into, it's also a place for resources
;; to be routed through. I think it's somewhat important that we "automate" the
;; flow of resources between machine parts - a machine part should only have to
;; intervene in special circumstances. Alternatively, we could define a part of
;; the interface which asks: "How much of this resource is sent out at this
;; step?"

