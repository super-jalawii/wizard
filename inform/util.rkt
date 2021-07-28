#lang racket

(require (except-in wizard contains?))
(require "core.rkt")

(provide symbol-append
         do-action)

(define (symbol-append sym str)
  (string->symbol (format "~a~a" (symbol->string sym) str)))

;; A small helper function which, given a symbol representing a verb, returns a
;; list of symbols representing the standard rulebooks for that verb.
(define (verb-rulebooks verb)
  (list (symbol-append verb "/instead-of")
        (symbol-append verb "/before")
        (symbol-append verb "/carry-out")
        (symbol-append verb "/report")
        (symbol-append verb "/after")))

(define (do-action action)
  ;; Fire off the appropriate events / abide by the appropriate rulebooks?
  (abide-rulebook 'event/before-turn #:param action)

  (abide-rulebook 'event/every-turn #:param action)

  (when action
    (let* ([rulebooks (verb-rulebooks (action-verb action))]
           [instead-outcome (abide-rulebook (car rulebooks) #:param action)])
      (when (not (decision-made? instead-outcome))
        (for/or ([rulebook (in-list (cdr rulebooks))])
          (failure? (abide-rulebook rulebook #:param action))))))

  (abide-rulebook 'event/after-turn #:param action))

