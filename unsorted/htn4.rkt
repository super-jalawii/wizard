#lang racket/base


#; (define (execute-plan plan)
  ...)

; enemy-range: melee in-view out-of-view

(define (decompose task ws)
  (if (primitive-task? task)
      ; TODO: Test primitive-task preconditions
      ; TODO: This is where we do the heavy lifting.
      ))

; Given a domain and the current world state, find a plan.
(define (find-plan domain ws)

  ; For now, the "domain" is actually a single compound task which contains the
  ; entire network of possible subtasks the agent can take.
  (let loop ([tasks-to-process `(,domain)]
             [world-state ws]
             [plan '()])

    ; TODO: We probably need to check that tasks-to-process isn't empty here.
    (let* ([current-task (car tasks-to-process)])

      (if (primitive-task? current-task)
          ; Test the current task, if it's good, then we're good, otherwise
          ; "rollback" (whatever that ends up meaning).
          (when (valid-task? current-task world-state)
            ))


           #; [subtasks (decompose current-task world-state)])

      ; If subtasks is empty, that means it was either a compound task with no
      ; valid methods (for the current world state), or it was a primitive task
      ; for which the preconditions weren't met.
      ; If that's the case, then we want to unwind (by returning nothing I
      ; guess).
      (unless (empty? subtasks)
        ; Add decomposed tasks.
        (loop (append subtasks (cdr tasks-to-process))
              )))))


(define world-state (make-hash `((can-see-enemy #f)
                                 (enemy-range #f)
                                 (health #f)
                                 (is-tired? #f)
                                 (location #f))))

(struct task ())

; Operator is the actual thing
; Effects are the assumed outcomes of the task (in terms of world state transitions)
; Preconditions are the things which must be true in terms of world state in
;   order for this task to be valid
(struct primitive-task task (name terms operator effects preconditions))

(struct compound-task task (name terms methods))
(struct method (terms preconditions subtasks))

(define (animated-attack-operator anim-name)
  (println (format "Performing attack animation: ~a" anim-name)))

(define (navigate-to-operator dest)
  (println (format "Navigating to: ~a" dest)))

(define (choose-bridge-to-check-operator)
  (println "Choosing bridge to check..."))

(define (check-bridge-operator anim-name)
  (println (format "Checking bridge with animation: ~a" anim-name)))

(define do-trunk-slam (primitive-task 'do-trunk-slam '()
                                      (λ () (animated-attack-operator 'trunk-slam-anim-name))
                                      '() '()))

(define navigate-to-enemy (primitive-task 'navigate-to-enemy '()
                                          (λ () (navigate-to-operator 'enemy-loc-ref))
                                          (list (λ (ws) (hash-set ws 'location 'enemy-loc-ref)))
                                          '()))

(define choose-bridge-to-check (primitive-task 'choose-bridge-to-check '()
                                               (λ () (choose-bridge-to-check-operator))
                                               '() '()))

(define navigate-to-bridge (primitive-task 'navigate-to-bridge '()
                                           (λ () (navigate-to-operator 'next-bridge-loc-ref))
                                           (list (λ (ws) (hash-set ws 'location 'next-bridge-loc-ref)))
                                           '()))

(define check-bridge (primitive-task 'check-bridge '()
                                     (λ () (check-bridge-operator 'search-anim-name))
                                     '() '()))

(define be-trunk-thumper (compound-task 'be-trunk-thumper '()
                                        (list
                                         (method '()
                                                 (list (λ (ws) (hash-ref ws 'can-see-enemy)))
                                                 (list navigate-to-enemy do-trunk-slam))
                                         (method '() '()
                                                 (list choose-bridge-to-check navigate-to-bridge check-bridge)))))
