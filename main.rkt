#lang racket/base

(module+ test
  (require rackunit))

;; Notice
;; To install (from within the package directory):
;;   $ raco pkg install
;; To install (once uploaded to pkgs.racket-lang.org):
;;   $ raco pkg install <<name>>
;; To uninstall:
;;   $ raco pkg remove <<name>>
;; To view documentation:
;;   $ raco docs <<name>>
;;
;; For your convenience, we have included LICENSE-MIT and LICENSE-APACHE files.
;; If you would prefer to use a different license, replace those files with the
;; desired license.
;;
;; Some users like to add a `private/` directory, place auxiliary files there,
;; and require them in `main.rkt`.
;;
;; See the current version of the racket style guide here:
;; http://docs.racket-lang.org/style/index.html

;; Code here


(require "rules.rkt"
         "ecs.rkt")

#|


From the Inform Recipe Book:

A road is a kind of room. Definition: A room is offroad if it is not a road.

Instead of going by a vehicle (called the auto) to somewhere offroad:
    say "You can't drive [the auto] off-road."

Frafalgar Square is a road. "The Square is overlooked by a pillared statue of
Admiral Lord Horatio Nelson (no relation), naval hero and convenience to pigeons
since 1812."

The National Gallery is north of Trafalgar Square. The Strand is east of
Trafalgar Square. The Strand is a road.

The car is a vehicle in Trafalgar Square. The ignition is a device. The ignition
is part of the car.

Instead of going by the car when the ignition is switched off:
    say "The ignition is off at the moment."

Instead of switching on the car, try switching on the ignition.
Instead of switching off the car, try switching off the ignition.

|#

;; A room is actually a node in a network (a relation). For now we implement it
;; the dumbest way we can get away with until a good abstraction is obvious.
(define/component Room (exits))
(define/component Road)
(define/component Described (name desc))
(define/component Vehicle)
(define/component Device (state))
(define/component Container (contents))
(define/component Part (of))

(define [turn-on device]
  (set-Device-state! device #t))
(define [turn-off device]
  (set-Device-state! device #f))
(define [on? device]
  (Device-state device))

(define [make-room . exits]
  (let ([exits (for/hash ([x (in-list exits)])
                 (values (car x) (cdr x)))])
    (Room exits)))

(define [offroad?]
  (compose (has? 'Room)
           ; FIXME: This feels particularly clumsy - we can't negate has? except
           ; by calling it manually (outside of the transducer context).
           (xform/filter (λ (eid)
                           (not ((has? 'Road) eid))))))

;; Actions ... are kind of weird for us right now. They're like rules. But they
;; have explicit phases, and order matters (which is a problem for our rule
;; system right now).

;; An additional thing to consider is that rules seem to have contextual
;; predicates which must be true for the rule to even execute. For instance, in
;; the above text there is a rule which only applies when the action is "going",
;; and the first and second nouns are "vehicle" and "somewhere offroad".

;; These feel somewhat like match statements or other destructuring forms - the
;; rule is actually something like:

;; (going vehicle somewhere-offroad)

;; In this case, vehicle and somewhere-offroad are themselves predicates which
;; would be matched against the more specific action taken by the player.

;; The key will be figuring out what the most specific rule is. Do we only
;; perform the most specific rule?


(define the-car
  (define/entity
    (Container '())
    (Described "The Car" "")))

;; TODO: We will have to eventually do some work on the text substitution part
;; of this project. For now we're just writing articles and stuff willy-nilly
;; but at some point we will need to formalize.
(define/entity ignition
  (Described "The Ignition")
  (Device #f)
  ;; FIXME: Parts are really relations. This keeps coming up - we need a way to
  ;; describe relations here. The relation here is "incorporation". Relations
  ;; are just a special case of component which could benefit from a few extra
  ;; functions specific to the use-case - traversing the graph for example.
  (Part the-car))


(define the-strand
  (define/entity
    (Road)
    ;; FIXME: Room connections are generally mutual - we can't specify them like
    ;; this because we are guaranteed that one of the two rooms in each
    ;; connection haven't been created yet. (I've quoted them for the moment so we can compile)
    (make-room ('West 'trafalgar-square))
    ;; FIXME: Many described people/places/things won't have descriptions. We
    ;; can enhance clarity by providing keyword arguments. Would there ever not
    ;; be a name?
    (Described "The Strand" "")))

(define trafalgar-square
  (define/entity
    (Road)
    (make-room ('East 'the-strand))
    (Described "Trafalgar Square"
               "The Square is overlooked by a pillared statue of Admiral Lord\
Horatio Nelson, naval hero and convenience to pigeons since 1812.")))

(module+ test
  ;; Any code in this `test` submodule runs when this file is run using DrRacket
  ;; or with `raco test`. The code here does not run when this file is
  ;; required by another module.

  (check-equal? (+ 2 2) 4))

(module+ main
  ;; (Optional) main submodule. Put code here if you need it to be executed when
  ;; this file is run using DrRacket or the `racket` executable.  The code here
  ;; does not run when this file is required by another module. Documentation:
  ;; http://docs.racket-lang.org/guide/Module_Syntax.html#%28part._main-and-test%29

  (require racket/cmdline)
  (define who (box "world"))
  (command-line
    #:program "my-program"
    #:once-each
    [("-n" "--name") name "Who to say hello to" (set-box! who name)]
    #:args ()
    (printf "hello ~a~n" (unbox who))))
