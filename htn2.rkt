#lang racket

(require graph)

#;(define current-domain (make-parameter (unweighted-graph/directed '())))

;; Primitive tasks are just sort of...there. They aren't themselves connected to
;; anything. Later our compound task methods will reference them. Any primitive
;; task which can't be reached from the root node (ignoring preconditions and
;; effects) can be culled in the runtime form.


#;(define-vertex-property (current-domain) task)

#;(define [add-primitive-task task #:domain [domain (current-domain)]]
  (let ([task-name (gensym "primitive_")])
    (add-vertex! (current-domain) task-name)
    (task-set! task-name task)))

#;(define [add-compound-task methods #:domain [domain (current-domain)]]
  (let ([ctask-name (gensym "compound_")])
    (add-vertex! (current-domain) ctask-name)
    (for ([method (in-list methods)])
      (let ([method-name (gensym "method_")]
            [subtask-name (gensym "subtask_")])
        (add-vertex! (current-domain) method-name)
        (add-directed-edge! (current-domain) ctask-name method-name)

        ;; For each task in this method, we need to build out the chain further.
        ;; The key is that subtasks can themselves be compound or primitive
        ;; tasks.... I'm seeing an issue with our strategy here..

        ))))


;; Ok, here's why this ^^^^ is slightly off. We need to maintain a dictionary of
;; tasks separate from the domain graph. Think of it like our lexicon of known
;; "actions". The graph itself contains only references to what needs to be done.

(define current-domain (make-parameter (unweighted-graph/directed '())))
(define current-task-dict (make-parameter (make-hash)))

(struct Task (name))
(struct CompoundTask Task (methods))

(define [add-primitive-task task #:domain [domain (current-domain)]]
  (hash-set! (current-task-dict) (Task-name task))
  (add-vertex! (current-domain) (Task-name task)))

(define [add-compound-task task #:domain [domain (current-domain)]]
  (hash-set! (current-task-dict) (Task-name task))
  (add-vertex! (current-domain) (Task-name task))

  ;; For compound tasks, we add each method as a child. Methods aren't tasks,
  ;; they're lists of tasks.
  (for ([method (in-list (CompoundTask-methods task))])

    (let ([method-name (gensym "method_")])

      ;; Connect each method back to the parent compound task.
      (add-directed-edge! (current-domain) (Task-name task) method-name)

    )




  )

(define [add-task task #:domain [domain (current-domain)]]
  (if (CompoundTask? task)
      (add-compound-task task)
      (add-primitive-task task)))


