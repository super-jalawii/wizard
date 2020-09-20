#lang racket

(require graph)

(define current-domain (make-parameter (unweighted-graph/directed '())))
(define current-task-dictionary (make-parameter (make-hash)))
(define current-world-state (make-parameter (make-hash)))

(struct Task (name) #:transparent)
(struct CompoundTask Task (methods) #:transparent)
(struct PrimitiveTask Task (preconditions effects) #:transparent)
(struct Method (preconditions subtasks) #:transparent)

(define [add-primitive-task task]
  (let ([domain (current-domain)]
        [dict (current-task-dictionary)])
    (hash-set! dict (Task-name task) task)
    (add-vertex! domain (Task-name task))))

(define [add-compound-task task]
  (let ([domain (current-domain)]
        [dict   (current-task-dictionary)])

    ;; Add compound task to task network.
    (hash-set! dict (Task-name task) task)
    (add-vertex! domain (Task-name task))

    ;; Link each method to the compound task
    (for ([method (in-list (CompoundTask-methods task))])
      (add-vertex! domain method)
      (add-directed-edge! domain (Task-name task) method))))

(define [add-task task]
  (if (CompoundTask? task)
      (add-compound-task task)
      (add-primitive-task task)))

;; That's all there is to building the domain itself. Next we build the planner
;; itself. This is essentially a depth-first-search of the graph. We could use
;; the built-in functionality to do this, but it will be easier on us if we just
;; work it out manually I think.

(define [plan-from root-task]

  ;; TODO: Build planner.

  (let ([domain (current-domain)])

    ;; FIXME: What happens when root-task is not part of the current domain?
    ;; FIXME: We should make domains know what their root task is.

    (let loop ([tasks-to-process (list root-task)]
               [world-state      (current-world-state)]
               [plan             (list)])

      (printf "~nTasks to Process: ~a~nCurrent World State: ~a~n"
              tasks-to-process
              world-state)

      ;; This is the main recursive loop thingy
      (for ([task (in-list tasks-to-process)])

        (if (CompoundTask? task)
            (let ([subtasks (decompose-task task world-state)])
              (if subtasks
                  (begin
                    (printf "~nAdding Subtasks: ~a" subtasks)
                    (loop (append subtasks (cdr tasks-to-process))
                          world-state
                          plan))
                  (begin
                    (printf "~nNo Subtasks. Moving on..?")
                    (loop (cdr tasks-to-process) world-state plan))))
            (if (preconditions-met? task world-state)
                (begin
                  (printf "~nPrimitive Task preconditions met, adding to plan: ~a" plan)
                  (loop (cdr tasks-to-process)
                        (apply-effects task world-state)
                        (append plan (list task))))

                ;; TODO: Else rollback??

                (begin
                  (printf "~nFIXME: We need to rollback but we (I) don't know how...")
                  #f)))))))

(define [decompose-task task world-state]
  (for/or ([method (in-list (CompoundTask-methods task))])
    (try-method method world-state)))

(define [try-method method world-state]
  (if (null? (Method-preconditions method))
      (Method-subtasks method)
      (if (for/and ([p? (in-list (Method-preconditions method))]) (p? world-state))
          (Method-subtasks method)
          #f)))

(define [preconditions-met? task world-state]
  (if (null? (PrimitiveTask-preconditions task))
      #t
      (for/and ([p? (in-list (PrimitiveTask-preconditions task))])
        (p? world-state))))

(define [apply-effects task world-state]
  (foldl (λ (ws e) (e ws))
         world-state
         (PrimitiveTask-effects task)))

;; NOTE: Plans need to be built when:
;; The NPC finishes their current plan, or the current plan fails
;; The NPC has no plan
;; The world-state changes as observed through sensors
