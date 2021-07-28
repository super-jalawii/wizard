#lang racket

(require racket/provide
         gregor
         gregor/period
         (except-in wizard contains?)
         "../core.rkt")

(provide (struct-out timepiece)
         *current-time*
         trigger-event
         (matching-identifiers-out #rx"&time.*$" (all-defined-out)))

(define/component timepiece (fmt))
(define/event &time ([advance amt]))

(define *current-time* (make-parameter (datetime 1200)))

(define (trigger-event evt)
  (abide-rulebook 'event/before #:param evt)
  (abide-rulebook 'event #:param evt)
  (abide-rulebook 'event/after #:param evt))

(define (pre-format-datetime t fmt)
  ;; Basically this function's job is to patch up unimplemented format
  ;; specification thingies.
  (string-replace fmt "b" (let ([t (->hours t)])
                            (cond
                              [(and (< 0  t) (< t 11)) "'in the morning'"]
                              [(and (< 11 t) (< t 5))  "'in the afternoon'"]
                              [(and (< 5  t) (< t 10)) "'in the evening'"]
                              [else                    "'at night'"]))))

(define/rule
  #:for event/before-turn
  #:rule (_ (trigger-event (&time::advance 5))))

(define/rule
  #:for event/before
  #:basis (&time::advance _)
  #:rule ((&time::advance amt)
          (*current-time* (+period (*current-time*)
                                   (period (minutes amt))))))

(define/rule
  #:for look/report
  #:basis (action 'look (? (ent/has? struct:timepiece)) #f)
  #:rule ((action _ obj _)
          (let/entity obj ([(timepiece fmt) timepiece])
                      (let* ([current-time (*current-time*)]
                             [fmt (pre-format-datetime current-time fmt)]
                             [formatted (~t current-time fmt)])
                        (printf "~nIt is currently ~a." formatted)))
          (succeed)))

