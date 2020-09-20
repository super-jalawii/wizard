#lang racket

(require "rules.rkt"
         (rename-in "transducer.rkt"
                    (map xform/map)
                    (filter xform/filter)))

#|

Actions are really a few things - first, they're the primitive understood by the
parser. That is - if the parser can't understand the player's input to match an
action, then the input is invalid. Actions are also the collection of rules
invoked when one of those commands is completed.

In Inform, there are (up to) 6 rulebooks which are invoked for a given action:

+ Instead
+ Before
+ Check
+ Carry Out
+ After
+ Report

|#

;; Datatypes -------------------------------------------

(struct Action (verb subject object) #:transparent)

(define [perform-action action]

  ;; Based on the current action, perform the necessary rulebooks. This itself
  ;; is most likely a rulebook...

  (abide-rulebook 'instead   #:param action)
  (abide-rulebook 'before    #:param action)
  (abide-rulebook 'check     #:param action)
  (abide-rulebook 'carry-out #:param action)
  (abide-rulebook 'after     #:param action)
  (abide-rulebook 'report    #:param action))


;; Transducer function which combines rulebook execution with early termination
;; when a decision is made.

(define [abide action]
  (compose (xform/map (λ (rb) (printf "~nAttempting rule with action: ~a~nAgainst basis: ~a~n" action (Rule-basis rb)) rb))
           (xform/map (λ (rb) (abide-rulebook rb #:param action)))
           (xform/map (λ (outcome) (if (decision-made? outcome)
                                       (reduced outcome)
                                       outcome)))))






