#lang racket/base

;; 
;; Fabricator Machine System
;; 
;; Created 10/03/2020
;; 

;; 
;; Summary:
;; 
;; Defines the subset of the Fabricator system which deals with machinery and how
;; resources propagate through machines over the course of a game step. This
;; module does not define the structural aspects of the system, only the logical
;; flow.
;; 

;; 
;; TODO: Implement an enum macro to make resource and conduit type definitions less
;; cumbersome.

(require racket/generic
         graph
         (except-in wizard filter map)
         (rename-in wizard
                    [filter xform/filter]
                    [map xform/map]))


(define ( TODO: [msg "Not yet implemented!"]) (error (format "~nTODO: ~a" msg)))

;; 
;; Part I: Sockets and Resources ---------------------------------------------------
;; 
;; Machine parts don't connect directly to each other, instead, an output socket
;; from one part is connected to a corresponding input socket on another part.
;; Thus a socket defines what types of connections are valid for a particular
;; resource.
;; Originally, a socket was directly tied to a resource type, but that
;; limits the types of things you can do with, for example, a gas tank. It's
;; really just a container, and it could carry anything which can fit through
;; that type of socket. This points out another issue, that perhaps it would make
;; more sense for resources to be something like resource::combustible rather
;; than resource::petrol. Perhaps then, resources have attributes such as their
;; state (in the sense of "state of matter"), how much energy a unit of this
;; resource is equivalent to (as a sort of theoretical maximum).
;; 
;; The socket types defined below are essentially constants.
;; 
(define socket::data 'SOCKET_DATA)
(define socket::valve 'SOCKET_VALVE)
(define socket::outlet 'SOCKET_ELECTRICAL)
(define socket::mechanical 'SOCKET_MECHANICAL)
;;      ^^^ This is like some kind of gearbox which takes mechanical energy as
;;      input or output.
;; 
;; Resources are associated with a particular socket type - no resource can be
;; used with multiple socket types.
;; 
(struct resource (id stype) #:constructor-name $resource)
;; 
;; These resource declarations are essentially constants.
;; 
(define resource::power ($resource 'RESOURCE_ELECTRICAL socket::outlet))
(define resource::petrol ($resource 'RESOURCE_PETROL socket::valve))
(define resource::water ($resource 'RESOURCE_WATER socket::valve))
(define resource::data ($resource 'RESOURCE_DATA socket::data))
;; 
;; A socket-spec is the specification for a particular input or output port on a
;; machine part. The socket specification decides what can be connected to what,
;; but doesn't represent any specific connection involving that socket. Instances
;; of socket-spec should be reusable among components.
;; 
;; Size refers to the physical size (generally diameter) of the socket's physical
;; opening. For some conduit types, this will indicate (along with the length of
;; conduit), how much of the resource can enter before things are considered to
;; be backed up.
;; 
(struct socket-spec (stype size) #:constructor-name $socket-spec)
;; 
;; UOM Declaration for Power, Voltage, etc ---------------------------------------
;; 
(struct power uom ()
  #:constructor-name $power
  #:property prop:conversions (unweighted-graph/directed '()))

(struct volts power ()
  #:constructor-name $volts
  #:transparent
  #:property prop:units "v") ; This is of course a potentially problematic
                             ; simplification but we can't get into the weeds to
                             ; far at this point. We just want to be able to say
                             ; whether a socket is compatible with the thing it's
                             ; being connected to. We can thus cause electrical
                             ; appliances which require 240v to not work with
                             ; 120v even if the socket type is (approximately)
                             ; correct.
;; 
;; And here are some sample socket specifications:
;; 
(define socket-spec::power-outlet ($socket-spec socket::outlet ($volts 120)))
(define socket-spec::tap ($socket-spec socket::valve ($inches 1/4)))

