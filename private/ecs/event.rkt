#lang racket

(require racket/generic
         racket/match
         (for-syntax racket/syntax
                     syntax/parse))

(provide define/event
         send
         recv
         recv-all
         clear
         clear-all)

;; TODO: How can we delete a message to prevent it from propogating to other
;; systems. Is that something we should even allow?

(define-generics event
  (event-type event))

(define events (make-parameter (make-hash)))

(define-syntax [define/event stx]
  (syntax-parse stx
    [(_ name:id ([type:id params:id ...] ...+))
     #`(begin
         (hash-set! (events) (quote name) '())
         #,@(for/list ([t  (in-list (syntax-e #'(type ...)))]
                       [ps (in-list (syntax-e #'((params ...) ...)))])
              (with-syntax ([struct-id (format-id t "~a::~a" #'name t)])
                #`(struct #,(datum->syntax t #'struct-id) #,ps
                    #:transparent
                    #:methods gen:event
                    [(define [event-type self] (quote name))]))))]))

(define [send msg]
  (hash-set! (events)
             (event-type msg)
             (cons msg (hash-ref (events) (event-type msg)))))

(define-syntax [recv-all stx]
  (syntax-parse stx
    [(_ type:id)
     #'(hash-ref (events) (quote type))]))

(define-syntax [clear stx]
  (syntax-parse stx
    [(_ type:id)
     #'(hash-set! (events) (quote type) '())]))

(define [clear-all]
  (hash-clear! (events)))

;; FIXME: Figure out how to represent the "else" branch
(define-syntax [recv stx]
  (syntax-parse stx
    [(_ name:id [(type:id params:id ...) body ...+] ...+)
     #'(for/list ([msg (recv-all name)])
         (match msg
           [(type params ...) body ...]
           ...))]))

