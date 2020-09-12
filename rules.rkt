#lang racket

(require racket/contract/region
         racket/stxparam
         (for-syntax syntax/parse
                     racket/syntax))

(provide struct-update!
         exec-rule
         exec-rulebook
         define/rule
         define/rulebook
         (struct-out Rule))

;;; TODO: Insert rules in order of declaration by default (currently reversed).
;;; TODO: Implement a macro for executing rules/rulebooks or else another way to
;;; execute them where the user doesn't have to quote the forms (since we allow
;;; them to pretend they're identifiers when they create them.)

;;; TODO: Params are currently hard coded into the rule macros - Ideally we want
;;; to require some core set of parameters (dealing with world context etc), but
;;; allow rules to require additional parameters - In Inform, rules can take a
;;; parameter.

;;; TODO: We could consider moving those parameters out into global parameters
;;; like we do for the rule database.

(define-syntax [struct-update! stx]
  (syntax-parse stx
    [(_ type:id field:id inst:expr update-fn:expr)
     #:with setter #`#,(format-id #'type "set-~a-~a!" #'type #'field)
     #:with getter #`#,(format-id #'type "~a-~a" #'type #'field)
     #'(setter inst (update-fn (getter inst)))]))

;;; Datatypes ----------------------------------

(struct Rule (name rule) #:transparent)

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

(define/contract [exec-rule rule #:params [params '()]]
  (->* ((or/c symbol? Rule?)) (#:params (listof any/c)) any)
  (let ([rule (if (Rule? rule)
                  rule
                  (car (hash-ref (rule-database) rule
                                 (error "Unknown rule"
                                        rule
                                        "invoked. Execution failed."))))])
    ; NOTE: We're assuming this function is called on specific named rules, not
    ; rulebooks. As written, there is no way to invoke a specific rule from a
    ; rulebook without invoking the entire rulebook. This is not a limitation of
    ; the system, just how it's currently written.
    (apply (Rule-rule rule) params)))

;; TODO: Rulebooks can return one of several sentinel values which indicate the
;; outcome of the rulebook as well as its value.
(define/contract [exec-rulebook rulebook #:params [params '()]]
  (->* (symbol?) (#:params (listof any/c)) any)
  (for/list ([rule (in-list (hash-ref (rule-database) rulebook '()))])
    (exec-rule rule #:params params)))

;;; Macro interface ----------------------------

#;(define/rule #:name rule/update-turn-counter
    (ecs/select-one
     ([g Global])
     (set-Global-turn-counter! g (add1 (Global-turn-counter g)))))

(define-syntax-parameter ecs
  (λ (stx)
    (raise-syntax-error (syntax-e stx) "ecs not bound.")))

(define-syntax-parameter cmd
  (λ (stx)
    (raise-syntax-error (syntax-e stx) "cmd not bound.")))

; (define/contract [add-rule rule #:under [rulebook #f]]
(define-syntax [define/rule stx]
  (syntax-parse stx
    [(_
      (~optional
       (~seq #:name name:id)
       #:defaults
       ([name #`#,(format-id #f "~a" (gensym "rule"))]))
      (~optional
       (~seq #:for rulebook:id) #:defaults ([rulebook #'#f]))
      body ...)
     #`(add-rule (Rule (quote name)
                       (λ (e c)
                         (syntax-parameterize
                             ([ecs (make-rename-transformer #'e)]
                              [cmd (make-rename-transformer #'c)])
                           body ...)))
                 #:under (quote rulebook))]))

;; Define a rulebook called core/every-turn with two rules. The first rule is
;; anonymous, while the second rule is called advance-time.
#;(define/rulebook core/every-turn
    ([(ecs/select-one ([g Global])
                      (struct-update! Global turn-counter g add1))]
     [advance-time
      (ecs/select-one ([g Global])
                      (struct-update! Global current-time g add1))]))

(define-syntax [define/rulebook stx]
  (syntax-parse stx
    [(_ rulebook:id
        ([(~optional rule-name:id) rule-body:expr ...+] ...))
     #:with rule-defs (for/list ([r  (in-list (attribute rule-name))]
                            [rb (in-list (attribute rule-body))])
                   (cons (if r r (format-id #f "~a" (gensym)))
                         rb))
     #`(begin
         #,@(for/list ([rule (in-list (syntax->datum #'rule-defs))])
           (printf "Rule: ~a~nBody: ~a~n" (car rule) (cadr rule))
           #`(define/rule
               #:name #,(car rule)
               #:for  rulebook
               #,(cadr rule))))]))


;; This file contains the original rulebook / rules system. The majority of it
;; is just plumbing to get rules and rulebooks set up as a data-structure. Now,
;; if possible, we need to merge the work we did in "rules2.rkt" into this.

;; When this is complete, we will be able to define rulebooks, rules for those
;; rulebooks, and we will be able to figure out which rules actually apply given
;; a specific action.

;; The last step will be updating rule-traversal so that rules can make a
;; decision.










































