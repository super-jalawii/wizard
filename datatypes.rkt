#lang racket


;;; Defined here are the various types of data used to represent the
;;; story-world.

(require (prefix-in ecs/ "../ecs/ecs.rkt"))

;; Kinds are just components. Kinds are generally supposed to have a hierarchy too.

(ecs/component ecs/world Room [conns])
(ecs/component ecs/world Container [contents])
(ecs/component ecs/world Object [name desc attrs])
(ecs/component ecs/world Global [turn-counter])


(ecs/entity ecs/world (Global 0))

;; Actions are ... systems?? ... "event handlers"

;; "Instead of" rulebook ...
;; It's a list of predicates (Instead of doing a thing only
;;  triggers when "Is doing a thing" returns true)
;; A predicate to determine if it applies, then a function to
;;  follow the rulebook and perhaps produce an outcome.

;; Rulebooks are callible as functions.

;; Rulebooks are lists of rules.
;; Rules have names (so they can be listed and unlisted).
;; Rules can be rearranged somehow.

;; There are some primitive systems which define basic functionality.

(define actions (make-hash))

(struct Command [action] #:transparent)

(define [system/do-action ecs cmd]
  ;; For the given action run its associated rulebooks in order. Abort if any of the rules fail.
  (let ([action (hash-ref actions (Command-action cmd) (hash-ref actions 'action/invalid-command))])
    (apply (hash-ref action 'pre) '(ecs cmd))
    (apply (hash-ref action 'def) '(ecs cmd))
    (apply (hash-ref action 'post) '(ecs cmd))))

(define action/describe (make-hash))
(hash-set! action/describe 'pre  (λ (e c) (printf "~nPre-Describe: do nothing")))
(hash-set! action/describe 'def  (λ (e c) (printf "~nDescribe: do nothing")))
(hash-set! action/describe 'post (λ (e c) (printf "~nPost-Describe: do nothing")))

(define action/invalid-command (make-hash))
(hash-set! action/invalid-command 'pre  (λ (e c) e))
(hash-set! action/invalid-command 'def  (λ (e c) (printf "~nInvalid Command")))
(hash-set! action/invalid-command 'post (λ (e c) e))

(hash-set! actions 'action/describe action/describe)
(hash-set! actions 'action/invalid-command action/invalid-command)

;; -----------------

;; Ask for input and then do something about it.
(define [prompt]
  (printf "~n# ")
  (read-line))

(define [eval]
  (printf "~nEval Phase begin")
  #;(system/do-action ecs/world (Command 'action/describe))
  (for ([r (in-list rules)])
    (resolve-rulebook r ecs/world (Command 'action/sleep)))
  (printf "~nEval Phase end"))

#;(let loop ([cmd (prompt)])
  (eval)
  (loop (prompt)))
