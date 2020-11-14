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

(define (pop lst)
  (values (car lst) (cdr lst)))

(define (bfs graph start fn)
  (define-vertex-property graph reached?)
  (reached?-set! start #t)
  (let loop ([frontier (list start)])
    (unless (empty? frontier)
      (let-values ([(current frontier) (pop frontier)])
        (fn current)
        (for ([next (in-neighbors graph current)])
          (unless (reached? next #:default #f)
            (set! frontier (append frontier (list next)))
            (reached?-set! next #t)))
        (loop frontier)))))

(define (dfs graph start fn)
  (define-vertex-property graph reached?)
  (reached?-set! start #t)
  (let loop ([frontier (list start)])
    (unless (empty? frontier)
      (let-values ([(current frontier) (pop frontier)])
        (fn current)
        (for ([next (in-neighbors graph current)])
          (unless (reached? next #:default #f)
            (reached?-set! next #t)
            (loop (append frontier (list next)))))))))



(add-vertex! (current-domain) 1)
(add-vertex! (current-domain) 2)
(add-vertex! (current-domain) 3)
(add-vertex! (current-domain) 4)
(add-vertex! (current-domain) 40)
(add-directed-edge! (current-domain) 1 2)
(add-directed-edge! (current-domain) 2 3)
(add-directed-edge! (current-domain) 3 4)
(add-directed-edge! (current-domain) 4 40)
(add-vertex! (current-domain) 1.5)
(add-vertex! (current-domain) 2.5)
(add-directed-edge! (current-domain) 1 1.5)
(add-directed-edge! (current-domain) 2 2.5)
; (bfs (current-domain) 1)

(require data/heap)

(define (priority-queue [init '()] [fn <=])
  (let ([queue (make-heap fn)])
    (apply heap-add! queue init)
    queue))

(define (graph<= x y)
  (<= (cdr x) (cdr y)))

(define insert! heap-add!)
(define (remove! q)
  (let ([elt (heap-min q)])
    (heap-remove-min! q)
    elt))

(define (default-heuristic a b)
  (abs (- a b)))

(define (default-heuristic-2d a b)
  (+ (abs (- (car a) (car b)))
     (abs (- (cadr a) (cadr b)))))

(define (a* graph start goal #:heuristic [heuristic default-heuristic])
  (let ([came-from (make-hash (list (cons start #f)))]
        [cost-so-far (make-hash (list (cons start 0)))])
    (let loop ([frontier (priority-queue (list (cons start 0)) graph<=)])

      ; We're done when the frontier is empty - that means there's nothing left
      ; to explore.
      (unless (empty? frontier)

        ; Start with the next element in the frontier. Remember that the frontier
        ; is a priority queue, so this is not just a random element, but the
        ; element with the lowest cost.
        (let ([current (remove! frontier)])

          ; If this is our goal, we're done. Remember that current is actually a
          ; tuple containing the node as well as its cost.
          (unless (equal? (car current) goal)

            (for ([next (in-neighbors graph (car current))])

              ; Calculate the new cost for each neighboring node, adding them to
              ; the frontier. In dijkstra's algorithm, we add the cost of
              ; reaching this node from the start to the cost of traversing this
              ; node from our current location.
              (let ([new-cost (+ (hash-ref cost-so-far (car current))
                                 (edge-weight graph (car current) next))])
                (when (or (not (hash-has-key? cost-so-far next))
                          (< new-cost (hash-ref cost-so-far next)))
                  (hash-set! cost-so-far next new-cost)
                  (insert! frontier (cons next (+ new-cost (heuristic next goal))))
                  (hash-set! came-from next current))))
            (loop frontier)))))
    (build-path came-from start goal)))

(define (build-path came-from start goal)
  (let loop ([current (cons goal 0)]
             [path '()])
    (if current
      (loop (hash-ref came-from (car current))
            (cons (car current) path))
      path)))

;  NOTE: This version of dijkstra's algorithm stops when it finds the goal. It
;  might be desirable to calculate the cost for all tiles (forming a "dijkstra
;  map").
(define (dijkstra graph start end)
  (a* graph start end
      #:heuristic (λ (a b) 0)))

(define test-weighted-graph (weighted-graph/directed '()))
(add-vertex! test-weighted-graph 1)
(add-vertex! test-weighted-graph 2)
(add-vertex! test-weighted-graph 3)
(add-vertex! test-weighted-graph 4)
(add-vertex! test-weighted-graph 40)
(add-directed-edge! test-weighted-graph 1 2 1)
(add-directed-edge! test-weighted-graph 2 3 1)
(add-directed-edge! test-weighted-graph 3 4 1)
(add-directed-edge! test-weighted-graph 4 40 1)
(add-vertex! test-weighted-graph 1.5)
(add-vertex! test-weighted-graph 2.5)
(add-directed-edge! test-weighted-graph 1 1.5 1)
(add-directed-edge! test-weighted-graph 2 2.5 1)

(dijkstra test-weighted-graph 1 4)
(a* test-weighted-graph 1 4)

;  TODO: Implement gen:graph yourself since grids can be stored more efficiently.
(define test-weighted-grid (weighted-graph/directed '()))


(for* ([x (in-range 100)]
       [y (in-range 100)])
  (add-vertex! test-weighted-grid `(,x ,y))
  (add-directed-edge! test-weighted-grid `(,x ,y) `(,(+ x 1) ,y) 1)
  (add-directed-edge! test-weighted-grid `(,x ,y) `(,(- x 1) ,y) 1)
  (add-directed-edge! test-weighted-grid `(,x ,y) `(,x ,(+ y 1)) 1)
  (add-directed-edge! test-weighted-grid `(,x ,y) `(,x ,(- y 1)) 1)

  ; Diagonals

  (add-directed-edge! test-weighted-grid `(,x ,y) `(,(+ x 1) ,(+ y 1)) 1)
  (add-directed-edge! test-weighted-grid `(,x ,y) `(,(+ x 1) ,(- y 1)) 1)
  (add-directed-edge! test-weighted-grid `(,x ,y) `(,(- x 1) ,(+ y 1)) 1)
  (add-directed-edge! test-weighted-grid `(,x ,y) `(,(- x 1) ,(- y 1)) 1)

  )

; (dijkstra test-weighted-grid `(1 1) `(50 10))

(define (draw-grid grid width height #:path [path '()])
  (define-vertex-property grid char #:init #\.)
  (for ([node (in-list path)])
    (char-set! node #\x))
  (for ([y (in-range height)])
    (for ([x (in-range width)])
      (printf "~a" (char `(,x ,y))))
    (printf "~n")))
