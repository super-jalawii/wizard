#lang racket

(require graph
         (for-syntax syntax/parse
                     racket/syntax))

;; Planner
;; Uses a Domain and
;; World State to build a
;; Sequence of Tasks called a
;; Plan.

;; The Plan is run by the
;; Plan Runner.

;; World State is updated by
;; the NPC's Sensors.

(define world-state (make-parameter (make-hash)))

(define [update-ws-from-sensors ws]
  ws)

(struct Task ())
(struct PrimitiveTask Task (preconditions effects operation))
(struct CompoundTask Task (methods))
(struct Method (preconditions tasks))

;; Effects update the World State
(define [an-effect ws]
  ws)

;; The Operation is the actual thing performed. To decouple the implementations
;; from the HTN, we will most likely use a message/event, where the event
;; represents the decision, which will be executed next time this actor is able
;; to act.

(define-syntax [define/primitive stx]
  (syntax-parse stx
    ;; FIXME: What happens to the task-args??
    [(_ [task-name:id task-args ...]
        (~optional (~seq #:preconditions preconditions)
                   #:defaults ([preconditions #'(list)]))
        #:operation (op-name:expr op-args ...)
        (~optional (~seq #:effects effects)
                   #:defaults ([effects #'(list)])))

     #'(define task-name
         (PrimitiveTask (preconditions
                         (λ () (op-name op-args ...))
                         effects)))]))


;; Task Hierarchies are called domains.

;; A domain is basically a graph. There's a designated top-level node that we
;; use as a starting point, and then we do a depth first search to try and find
;; a valid plan based on the current world state and the preconditions
;; associated with each task.

;; Methods are lists of tasks - evaluated in order. This can still be
;; represented in the graph. So if we reimagine the HTN as a graph, and work on
;; inserting specific types of nodes and edges, and then define a method of
;; traversal (this is where we check preconditions and apply effects), we should
;; end up with a functioning HTN.

















;; FIXME: This doesn't work right now - it's not important yet, so moving on.
#;(define-syntax [define/compound stx]
  (syntax-parse stx
    [(_ [task-name:id task-args ...]
        ;; NOTE: We don't strictly need to include the keyword argument here,
        ;; but I think it helps to keep things organized in the code. Less
        ;; looking up what the parameters are, or guessing what's what.
        #:methods ((#:when preconditions #:then subtasks) ...+))

     #:with methods (for/list ([m (in-list (syntax-e
                                            #'((preconditions . subtasks) ...)))])
                      #'(Method (car m) (cdr m)))

     #'(printf "~a" methods)]))

