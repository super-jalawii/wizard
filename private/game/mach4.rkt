#lang racket/base

(require (except-in wizard filter map)
         (rename-in wizard
                    [filter xform/filter]
                    [map xform/map])
         "packet.rkt"
         graph
         threading)


(define type::electrical 'TYPE_ELECTRICAL)
(define type::liquid 'TYPE_LIQUID)

(struct socket (type [connected-to #:mutable])
  #:constructor-name $socket
  #:transparent)

(struct conduit (type)
  #:constructor-name $conduit
  #:transparent)

(define/component machine-part (inputs outputs)
  #:indexed #t)

(define *machine-graph* (make-parameter (unweighted-graph/directed '())))
(define *current-scale* (make-parameter 1))

(define (machine:for-socket socket-spec)
  (hash-ref (machine-part-inputs (component:for-entity struct:machine-part (car socket-spec)))
            (cdr socket-spec)))

(define (process-machine graph)
  ; Given a machine graph, follow the processing rules.
  ; Get the list of unprocessed machine parts which have no unprocessed inputs
  (define-vertex-property graph processed?)
  (define unprocessed-parts (xform/filter (lambda (part) (not (processed? part #:default #f)))))
  ;  FIXME: This is confused I think - about the fact that machine-part-inputs is a
  ;  list of sockets, not parts...
  (define no-pending-inputs (xform/filter (lambda (part) (andmap processed? (machine-part-inputs part)))))

  (ecs-query (compose unprocessed-parts
                      ; Convert the part ID into a proper component so that we
                      ; don't have to keep doing the lookup.
                      (xform/map (λ (part-id) (component:for-entity struct:machine-part part-id)))
                      no-pending-inputs
                      (xform/map (λ (part) (printf "~nUnprocessed and ready to process: ~a" part))))
             #:init (component:entities struct:machine-part))
  #f)

(module+ test
  (require rackunit)

  (parameterize ([*machine-graph* (unweighted-graph/directed '())]
                 [*current-scale* 1])

    ;; Build a test machine:
    (define mach1 (define/entity (@machine-part '() '())))
    (define mach2 (define/entity (@machine-part (list ($socket type::electrical)
                                                      ($socket type::electrical)) '())))

    


    ;; Test it...
    (process-machine (*machine-graph*))
    #f
    ))
