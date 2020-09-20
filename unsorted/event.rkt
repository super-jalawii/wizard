#lang racket

(require racket/generic
         racket/match
         (for-syntax racket/syntax
                     syntax/parse))

(provide define/event
         send
         process)

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

(define-syntax [read-events stx]
  (syntax-parse stx
    [(_ type:id)
     #'(hash-ref (events) (quote type))]))

;; ;;; Things to figure out

;; ;; Events - publish messages - subscribe to messages. Each subscriber reads the
;; ;; message one time, then it is deleted.

;; FIXME: Figure out how to represent the "else" branch
(define-syntax [process stx]
  (syntax-parse stx
    [(_ name:id [(type:id params:id ...) body ...+] ...+)
     #'(for/list ([msg (read-events name)])
         (match msg
           [(type params ...) body ...]
           ...))]))

;; Some test data to make sure things are working alright
(define/event Move ([Teleport ent x y]
                    [Adjacent ent x y]))

(send (Move::Teleport 1 100 100))
(send (Move::Adjacent 1  10  10))
(send (Move::Teleport 2  15  40))

(process Move
          [(Move::Teleport ent x y)
           (printf "Ent ~a teleported to ~a ~a\n" ent x y)]
          [(Move::Adjacent ent x y)
           (printf "Ent ~a moved to ~a ~a\n" ent x y)])

;; TODO: Can we treat messages as another type of entity storage? So that we
;; can join to receive only messages for entities which satisfy the criteria (in
;; terms of components present?)

