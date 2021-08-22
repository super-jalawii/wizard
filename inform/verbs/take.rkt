#lang racket

(require (except-in wizard contains?))
(require "../core.rkt"
         "../systems/map.rkt")

(define/rule
  #:for take/before
  #:basis (action 'take (not (? reachable?)) #f)
  #:rule ((action _ obj _)
          ;; TODO: This message should be different depending on whether the
          ;; entity can see the object and whether they "know about" the object.
          (printf "~nYou can't reach the ~a from here.~n" (ent/name-of obj))
          (fail)))

;; TODO: The entity being taken needs to be inside of the thing being taken
;; from.
(define/rule
  #:for take/before
  #:basis (action 'take _ (? reachable))
  #:rule ((action _ obj cont)
          (unless (or (cont . contains . obj)
                      (cont . supports . obj)
                      (cont . traverse-by-contains . obj)
                      (cont . traverse-by-supports . obj))
            (printf "~nYou aren't able to find that in here.~n")
            (fail))))

(define/rule
  #:for take/carry-out
  #:rule ((action _ obj _) (gib obj)))

(define/rule
  #:for take/report
  #:rule ((action _ obj _)
          (printf "~nTaken.~n~n")))

