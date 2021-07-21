#lang racket

(require wizard)


(define/component thing (name desc))

;; instead of listing the contents directly, we can store the eid of the
;; container here, and then look up the contents via the relation.
(define/relation containment #:from contains #:to contained-by)
(define/component container () #:indexed #t)

(define (container-contents c)
  (get-contains (indexed-component-eid c)))

#|

How can we handle "scope" and figuring out what the user is referring to? Can we
generate a set of aliases for objects? What order do we search for things in,
and when do we decide whether to ask for disambiguation vs assuming we've got
the correct thing?

Everything in the current area.
Everything on the player's person.

For some kinds of actions, we might want to talk about other things. Perhaps it
doesn't matter whether there is line of sight for a particular
action (discussing it perhaps?).

For every "thing", we can build an index based on the words in "name". We can
define aliases. Certain types of components may themselves act as adjectives.
Consider a "container". That's a component which, when added to something imbues
it with the ability to contain things. The player may refer to the thing in
question as a container regardless of what it is actually called. Our "chest"
entity might legitimately be called "container" by the player.

If there's an adjective-providing component, we may want to surpress that
adjective on a case-by-case basis, in the event that the container-ness is an
implementation detail for the entity in question.

So a score is calculated like this:

Full name match is worth the most.

Individual words are worth a fraction based on how many words were given.

Adjectives contributed otherwise are worth less still.

Multipliers are applied based on the proximity of the entity to the player (or
actor).

How long/far do we search? Until we find something reasonably good? We can't
know we're done unless we search everywhere or maintain an index of everything.

|#


#|

How do locations work?? Do we have that quadtree?

|#

(struct action (verb subject object))
(define look 'look)

(define (do-look-action action)
  (let ([instead-outcome (abide-rulebook 'look/instead-of #:param action)])
    (when (not (decision-made? instead-outcome))
      (for/or ([rulebook (in-list '(look/before look/carry-out look/report look/after))])
        (printf "~nFollowing rulebook ~a" rulebook)
        (failure? (abide-rulebook rulebook #:param action))))))


(define/rule
  #:for look/report
  #:basis (action look _ _)
  #:rule ((action _ obj _)
          (let/entity obj ([(thing name desc) thing])
                      (printf "~n~a :: ~a" name desc)

                      (let ([container (component:for-entity struct:container
                                                             (entity-eid obj))])
                        (when container
                          (let ([contents (container-contents container)])
                            (unless (empty? contents)
                              (printf "~n~nContents:")
                              (for ([item (in-list (container-contents
                                                    (component:for-entity
                                                     struct:container
                                                     (entity-eid obj))))])
                                (let/entity item ([(thing name desc) thing])
                                            (printf "~n  * ~a :: ~a" name desc))))))))))

(define chest (define/entity
                (@thing "Chest" "This is our example container.")
                (@container)))

