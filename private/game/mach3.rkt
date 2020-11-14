#lang racket/base

(require racket/generic
         racket/math
         racket/pretty
         graph
         threading
         (except-in wizard filter map)
         (rename-in wizard
                    [filter xform/filter]
                    [map xform/map])
         "unit.rkt"
         "packet.rkt")

(define ( TODO: [msg "Not yet implemented!"]) (error (format "~nTODO: ~a" msg)))

;; Categorizing sockets is actually really hard due to the lack of good
;; terminology (at least that I know), and so I'm finding it hard to get past the
;; parts where I need to name the socket types. So we aren't - not until later,
;; we can always come back and refactor.

;; Socket represents a specific "port" on a machine, either an input or an
;; output. This is more of a specification for a socket than any specific socket.
;; For example, a USB port socket might be able to be used across several actual
;; sockets on a machine component (sockets should be immutable).

(struct socket-type (all-res size)
  #:constructor-name $socket-type
  #:transparent) ;  TODO: For now, we just keep a list of the resources allowed,
                 ;  but ideally we would be able to say things like "all fluids"
                 ;  or any data resource.

(struct socket (id stype [incoming #:mutable])
  #:constructor-name $socket
  #:transparent)

(define direction::in 'DIRECTION_IN)
(define direction::out 'DIRECTION_OUT)

; FIXME: Is it possible that we don't need the direction for anything? It seems to
; be known from the context in most cases.
(define (make-socket stype
                     #:id [id (gensym "socket_")]
                     #:dir [dir direction::in])
  (let ([ctor (if (eq? dir direction::in)
                  $socket/in
                  $socket/out)])
    (ctor id stype '() #f)))

(struct socket/in socket ([upstream #:mutable])
  #:constructor-name $socket/in
  #:transparent)

(struct socket/out socket ([downstream #:mutable])
  #:constructor-name $socket/out
  #:transparent)

(define/component machine-part (inputs outputs ready-fn process-fn) #:indexed #t)

(define machine-graph (unweighted-graph/directed '()))
(define-edge-property machine-graph conduit-between)
(define-vertex-property machine-graph processed?)

(struct packet (res [amt #:mutable])
  #:constructor-name $packet
  #:transparent)

;; These are just some helpers because packet arithmetic will be common - we want
;; to make it both easy and obvious that these are the right things to be doing.

(define (packet/+! p amt) (set-packet-amt! p (uom+ (packet-amt p) amt)) p)
(define (packet/-! p amt) (packet/+! p (uom* amt -1))) ; I'm lazy ok?
;; TODO: Rename this to packet-transfer!
;; TODO: Write immutable versions of these packet functions.
;; TODO: This function feels a lot more complicated than it should be.
;; FIXME: The semantics of this function are somewhat confusing.
(define (packet-combine! p1 p2 #:capacity [capacity #f])
  (when (not (eq? (packet-res p1) (packet-res p2)))
    (error "Packets must contain the same type of resource to be combined."))
  ; Smooshes p1 into p2, returning the packet containing the leftovers if a
  ; capacity was specified, or a packet containing zero otherwise.
  (let*-values ([(p1-val p2-val) (normalize (packet-amt p1)
                                            (packet-amt p2))]
                [(capacity _) (if capacity
                                  (normalize capacity p1-val)
                                  (values #f #f))])
    (let* ([p1+p2 (uom+ p1-val p2-val)]
           ;  FIXME: It would be nice to have a "uom way" to do comparisons, even if
           ;  it just did this under the hood.
           [rem (uom- p1+p2 (if (and capacity
                                     ((uom-value capacity) . < . (uom-value p1+p2)))
                                ; This deals with situations where the receiving
                                ; packet already contains more than the capacity.
                                (if ((uom-value capacity) . < . (uom-value p2-val))
                                    p2-val
                                    capacity)
                                p1+p2))]
           [amt (uom- p1-val rem)])
      (packet/-! p1 amt) ; Transfer resources from the old packet to the new.
      (packet/+! p2 amt)
      ($packet (packet-res p1) rem))))

;; ready? should return #t when the part is ready to start operating from the
;; standpoint of running the machine. This should return #t when all upstream
;; machine parts have been run, regardless of whether this part actually received
;; the resources necessary to perform its function.
(define (consumer:ready? m-part)
  ; For each input socket, if there is an upstream-eid, ask if that entity has
  ; already processed.
  (for/and ([isock (in-list (machine-part-inputs m-part))])
    (or (not (socket/in-upstream (car isock)))
        (processed? (socket/in-upstream (car isock))))))

(define (machine:socket m-part sockid)
  ; Given a part and a sockid, return the corresponding socket.
  (or (hash-ref (machine-part-inputs m-part) sockid #f)
      (hash-ref (machine-part-outputs m-part) sockid #f)))

(define (machine:socket-downstream socket)
  (machine:socket
   (~>> socket
        socket/out-downstream
        car
        (component:for-entity struct:machine-part))
   (cdr (socket/out-downstream socket))))

(define (machine:push! m-part socket packet)
  ; Adds the given packet to the specified socket. If the socket (more accurately
  ; the conduit) can't accept the entire packet, the packet containing the
  ; remainder will be returned. If the entire packet is accepted, an empty packet
  ; will be returned.
  (let* ([conduit (machine:conduit m-part socket)]
         [volume (~>> socket
                     socket-stype
                     socket-type-size
                     (conduit-volume conduit))]
         ;  FIXME: Think of how much lighter this interface would be if we could
         ;  have just passed "downstream" directly to machine:socket...
         [downstream-socket (machine:socket-downstream socket)]
         [ds-no-packet? (null? (socket-incoming downstream-socket))]
         ;  FIXME: We clearly need a way to create a new UOM from an existing one -
         ;  remember we don't know the type.
         [downstream-current (if ds-no-packet?
                                 (begin
                                   (let ([packet
                                          ($packet (packet-res packet)
                                                   (uom- (packet-amt packet)
                                                         (packet-amt packet)))])
                                     (set-socket-incoming! downstream-socket packet)
                                     packet))
                                 (socket-incoming downstream-socket))]
         [rem (packet-combine! packet
                               downstream-current
                               #:capacity volume)])
    ; If we didn't do this, then an empty container could never be filled with
    ; anything else.
    (when (eq? 0 (packet-amt rem))
      (set-socket-incoming! downstream-socket '()))
    ;  TODO: If we have a way to get the UOM type from a uom, then we can convert
    ;  the result back into the type of the deposited resource - or if there was
    ;  already something in "incoming" we may elect to keep that.
    rem))

; TODO: Combine part and socket into a dotted pair.
(define (machine:conduit part socket)
  (let ([socket-conn (car ((if (socket/in? socket)
                               socket/in-upstream
                               socket/out-downstream) socket))])
  (conduit-between (indexed-component-eid part) socket-conn)))

(struct conduit (ctype len) #:constructor-name $conduit)

;; FIXME: This function is more convoluted than it should be because we don't have
;; a way to multiply units of measure (we can multiply them only as scalars). If
;; there was a way to say what unit of measure would result, then it might be ok.
(define (conduit-volume conduit diameter)
  ; Given a conduit and its diameter (really a property of the conduit I guess
  ; but it's a currently a property of the socket.)
  ; Normalize arguments to a measure which we know what the corresponding volume
  ; will be:
  (let ([len (uom-value (convert-to (conduit-len conduit) struct:feet))]
        [dia (uom-value (convert-to diameter struct:feet))])
    ; Do calculation with raw values, converting back into a uom before
    ; returning.
    ($cubic-feet (* len pi (expt (/ dia 2) 2)))))


; FIXME: It would make more sense to group the part/socket pairs like we do in the
; upstream and downstream specificactions.
(define (machine:connect from-part from-socket to-part to-socket conduit)
  (let ([from-eid (indexed-component-eid from-part)]
        [to-eid (indexed-component-eid to-part)])
    ; Set the upstream and downstream sockets
    (set-socket/out-downstream! from-socket (cons to-eid (socket-id to-socket)))
    (set-socket/in-upstream! to-socket (cons from-eid (socket-id from-socket)))
    ; Insert into the machine graph, and connect the part nodes
    (add-directed-edge! machine-graph from-eid to-eid)
    ; Add an edge property representing the conduit
    (conduit-between-set! from-eid to-eid conduit)))

(define (machine:push-all! part packet)
  ; Attempts to push the specified resource to all compatible output sockets.
  ; Generally resources are pushed at the same rate to all output sockets. This
  ; function should be prepared to handle the situation where a socket doesn't
  ; accept the entire packet. As with machine:push!, the packet containing the
  ; remainder is returned.
  (let* ([res (packet-res packet)]
         [sockets (filter (λ (sock)
                            (pretty-print (socket/out-downstream sock))
                            (and (socket/out-downstream sock)
                                 (~>> sock
                                     socket-stype
                                     socket-type-all-res
                                     (member res))))
                          (hash-values (machine-part-outputs part)))]
         #;[ds-sockets (map machine:socket-downstream sockets)])
    (pretty-print sockets)
    (let again ([next-sockets sockets]
                [per-socket-dist (uom/ (packet-amt packet) (length sockets))])
      (pretty-print next-sockets)
      (let ([next-sockets (for/list ([sock (in-list next-sockets)]
                                     #:break ((uom-value (packet-amt packet)) . <= . 0))
                            (printf "~nSocket: ~a" sock)
                            (packet/-! packet per-socket-dist)
                            (let ([rem (machine:push! part sock
                                                      ($packet (packet-res packet) per-socket-dist))])
                              (packet/+! packet (packet-amt rem))
                              (unless ((uom-value (packet-amt rem)) . > . 0)
                                sock)))])
        (if (length next-sockets)
            (again next-sockets
                   (uom/ (packet-amt packet) (length next-sockets)))
            packet)))))

(define (machine:pull! m-part socket)
  ( TODO: "How are sockets in a machine even identified?"))

(define (machine:available-resources m-part)
  ; Given a machine part, return a map of packets by resource type, representing
  ; the resources currently available to use. Since a machine will generally
  ; expect resources of each type supplied by an input socket, the returned map
  ; should contain entries for each resource even when the amount contained in
  ; the packet would be zero.
  ;  FIXME: This doesn't really work at all - we don't combine any resources or
  ; anything.
  (for/hash ([input (in-list (machine-part-inputs m-part))])
    (let ([p (socket-incoming input)])
      (cons (packet-res p) p))))

;; Given a set of packets containing resources - perhaps stored as a hash-map by
;; resource type, do something.
(define (consumer:process m-part)
  (let ([res (machine:available-resources m-part)])
    ( TODO:))
  ( TODO:))

;; NOTE: This is mostly intended to be a shim to create machine parts a little
;; easier. Once we know what this whole thing looks like we can design a better
;; function.
(define (make-machine-part inputs outputs)
  (let ([inputs (for/hash ([i (in-list inputs)])
                  (let ([sock (if (pair? i)
                                  (make-socket (cdr i) #:id (car i) #:dir direction::in)
                                  (make-socket i #:dir direction::in))])
                    (values (socket-id sock) sock)))]
        [outputs (for/hash ([o (in-list outputs)])
                   (let ([sock (if (pair? o)
                                   (make-socket (cdr o) #:id (car o) #:dir direction::out)
                                   (make-socket o #:dir direction::out))])
                     (values (socket-id sock) sock)))])
    (@machine-part inputs outputs
                   (λ () #f)
                   (λ () #f))))

(module+ test
  (require rackunit
           threading)

  
  (test-case
      "conduit-volume : Calculating volume of a cylinder where both dimensions are the same"
    (let ([conduit ($conduit 'HOSE ($feet 1))]
          [diameter ($feet 2)])
      (check-= (uom-value (conduit-volume conduit diameter))
               3.1416
               0.0001)))

  (test-case
      "conduit-volume : Calculating volume of a cylinder where the dimensions use different units"
    (let ([conduit ($conduit 'HOSE ($feet 6))]
          [diameter ($inches 6)])
      (check-= (uom-value (convert-to (conduit-volume conduit diameter) struct:cubic-feet))
               1.1781
               0.0001)))

  (test-case
      "machine:push! : Pushing a volume of petrol into a conduit big enough to receive it"
    ; part socket packet
    (let* ([tank (make-machine-part '() (list (cons 'TANK_OUT_1 ($socket-type resource-cat::fluid ($inches 4)))))]
           [engine (make-machine-part (list (cons 'ENGINE_IN_1 ($socket-type resource::petrol ($inches 4)))) '())])
      (define/entity tank)
      (define/entity engine)
      (machine:connect tank (machine:socket tank 'TANK_OUT_1)
                       engine (machine:socket engine 'ENGINE_IN_1)
                       ($conduit 'HOSE ($feet 6)))
      (let ([socket (machine:socket tank 'TANK_OUT_1)]
            [packet ($packet resource::petrol ($gallons 3.4))])
        (check-= (~> (machine:push! tank socket packet)
                     packet-amt
                     (convert-to struct:gallons)
                     uom-value)
                 0.0
                 0.0001
                 "Didn't receive the correct amount?"))))

  #;(test-case
      "machine:push! : Pusing a volume of petrol into a conduit too small to\
       receive the entire thing"

      )
  (test-case
      "machine:push-all! : Pushing a volume of petrol to all sockets that can receive it"
    ; part socket packet
    (let* ([tank (make-machine-part '()
                                    (list (cons 'TANK_OUT_1 ($socket-type resource-cat::fluid ($inches 4)))
                                          (cons 'TANK_OUT_2 ($socket-type (list resource::water) ($inches 4)))
                                          (cons 'TANK_OUT_3 ($socket-type resource-cat::fluid ($inches 4)))))]
           [engine1 (make-machine-part (list (cons 'ENGINE_IN_1 ($socket-type resource::petrol ($inches 4)))) '())]
           [engine2 (make-machine-part (list (cons 'ENGINE_IN_1 ($socket-type resource::petrol ($inches 4)))) '())]
           [engine3 (make-machine-part (list (cons 'ENGINE_IN_1 ($socket-type resource::petrol ($inches 4)))) '())]
           )
      (define/entity tank)
      (define/entity engine1)
      (define/entity engine2)
      (define/entity engine3)
      (machine:connect tank (machine:socket tank 'TANK_OUT_1)
                       engine1 (machine:socket engine1 'ENGINE_IN_1) ($conduit 'HOSE ($feet 6)))
      (machine:connect tank (machine:socket tank 'TANK_OUT_2)
                       engine2 (machine:socket engine2 'ENGINE_IN_1) ($conduit 'HOSE ($feet 6)))
      (machine:connect tank (machine:socket tank 'TANK_OUT_3)
                       engine3 (machine:socket engine3 'ENGINE_IN_1) ($conduit 'HOSE ($feet 6)))
      (let ([socket (machine:socket tank 'TANK_OUT_1)]
            [packet ($packet resource::petrol ($gallons 50))])
        (check-= (~> (machine:push-all! tank packet)
                     packet-amt
                     (convert-to struct:gallons)
                     uom-value)
                 0.0
                 0.0001
                 "Didn't receive the correct amount?"))))
  )
