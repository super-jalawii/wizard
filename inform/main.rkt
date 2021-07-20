#lang racket

(require wizard)


(define/component thing (name desc))

;; instead of listing the contents directly, we can store the eid of the
;; container here, and then look up the contents via the relation.
(define/relation containment #:from contains #:to contained-by)
(define/component container () #:indexed #t)

(define (container-contents c)
  (get-contains (indexed-component-eid c)))






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
          (printf "~nLooking...")))

(define chest (define/entity
                (@thing "Chest" "This is our example container.")
                (@container)))

