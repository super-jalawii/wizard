#lang racket

(require racket/contract/region
         racket/stxparam
         (for-syntax syntax/parse
                     racket/syntax))

(provide struct-update!
         abide-rulebook
         define/rule
         succeed
         fail
         success?
         failure?
         decision-made?
         (struct-out Action)
         (struct-out Determination)
         (struct-out Rule))

;;; TODO: Insert rules in order of declaration by default (currently reversed).

;;; TODO: Implement a macro for executing rules/rulebooks or else another way to
;;; execute them where the user doesn't have to quote the forms (since we allow
;;; them to pretend they're identifiers when they create them.)

;;; TODO: Rulebooks can have their own "local" variables which rules can use to
;;; communicate with each other.

;;; TODO: Rulebook execution follows the transducer pattern (at least for
;;; "abide").

;;; TODO: If we move the logic into the rule macro, we can "precalculate" the
;;; match statement for each rule, which will avoid the use of eval.

(define-syntax [struct-update! stx]
  (syntax-parse stx
    [(_ type:id field:id inst:expr update-fn:expr)
     #:with setter #`#,(format-id #'type "set-~a-~a!" #'type #'field)
     #:with getter #`#,(format-id #'type "~a-~a" #'type #'field)
     #'(setter inst (update-fn (getter inst)))]))

;;; Datatypes ----------------------------------

(struct Rule (name basis rule) #:transparent)

(struct Determination (outcome value) #:transparent)
(define [succeed [value #f]] (Determination 'success value))
(define [fail [value #f]] (Determination 'failure value))
(define [success? det] (eq? (Determination-outcome det) 'success))
(define [failure? det] (eq? (Determination-outcome det) 'failure))
(define [decision-made? det] (not (eq? (Determination-outcome det) 'no-outcome)))

;; NOTE: Technically there is a third determination: "decide on no outcome", but
;; we consider that to be the default for any rule which doesn't return one of
;; the other two determinations.

;; NOTE: There are other determinations which can be made. Some rulebooks can
;; define their own "named outcomes". For those rulebooks, we allow them to
;; simply provide their own outcome symbols, and optional constructors and
;; predicates.

;; Notably, in Inform, these other outcomes are still considered to be either
;; success or failure. We will have issues with our current design with this,
;; but this is one of those "cross that bridge when you get there" types of
;; problems.

;;; Data ---------------------------------------

(define rule-database (make-parameter (make-hash)))

;;; Function interface -------------------------

(define/contract [add-rule rule #:under [rulebook #f]]
  (->* (Rule?) (#:under (or/c symbol? #f)) void)
  (let* ([rulebook (if rulebook rulebook (Rule-name rule))]
         [rules    (hash-ref (rule-database) rulebook '())])
    (hash-set! (rule-database) rulebook (cons rule rules))))

(define/contract [remove-rule rule-name #:from [rulebook #f]]
  (->* (symbol?) (#:from (or/c symbol? #f)) void)
  (if rulebook
      (begin
        (hash-update! (rule-database)
                      rulebook
                      (λ (el) (remove* (list rule-name) el
                                      (λ (x y) (eq? x (Rule-name y))))))
        ; FIXME: This doesn't seem to do what we want it to do.
        (when (empty? (hash-ref (rule-database) rulebook '()))
          (hash-remove! (rule-database) rule-name)))
      (hash-remove! (rule-database) rule-name)))

(define [abide-rulebook rulebook #:param [param #f]]

  ;; Get the rules associated with the rulebook in question
  (let ([rules   (hash-ref (rule-database) rulebook '())]
        [outcome (Determination 'no-outcome #f)])

    (for/or ([rule (in-list rules)])
      (let ([o (try-rule rule param)])
        (if (Determination? o)
            (set! outcome o)
            (set! outcome (Determination 'no-outcome o)))
        (decision-made? outcome)))
    outcome))

;;; Macro interface ----------------------------

;; FIXME: I don't think we need to say "name" here...
#;(define/rule
  #:name update-turn-counter
  #:rule (_ (ecs/select-one ([g Global])
                              (struct-update! Global turn-counter g add1))))

#;(define/rule
  #:for   actions/carry-out
  #:basis (Action 'take light-source? something?)
  #:rule  ([Action _ obj cont]
           (printf "You take the ~a from the ~a." obj cont)))

(define-syntax [define/rule stx]
  (syntax-parse stx
    [(_ (~optional (~seq #:name name:id)
                   #:defaults ([name #`#,(format-id #f "~a" (gensym "rule"))]))
        (~optional (~seq #:for rulebook:id)
                   #:defaults ([rulebook #'#f]))
        (~optional (~seq #:basis basis)
                   #:defaults ([basis #'x]))
        #:rule (binding rule-stmts ...+))

     #'(begin
         (define name
           (Rule (quote name)
                 #'basis
                 (λ (x) (match-let ([binding x])
                          rule-stmts ...))))
         (add-rule name #:under (quote rulebook)))]))

(define [try-rule rule action]
  (let ([pat (Rule-basis rule)]
        [fn  (Rule-rule  rule)])
    (eval #`(match #,action
              [#,pat (#,fn #,action)]
              [_ #f]))))

;; FIXME: This is really just for testing - real action implementation still
;; hasn't been written...
(struct Action (verb noun sec) #:transparent)
