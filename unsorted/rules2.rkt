#lang racket


(require (for-syntax syntax/parse
                     racket/syntax))

;; ----------------------------------- RULES - TAKE TWO --------------------------------------


#|


Rulebooks are groups of rules. Rules in a rulebook run *in order* until a rule
makes some kind of determination like "succeeded" or "failed" or whatever.

A rulebook is only applicable in specific circumstances. This is kind of like a
function in the sense that the rulebook is only applicable when the input
parameters match the function's signature, and the return value is kind of like
the function's "determination".

In Inform (and I guess in Wizard (where you are right now)), the most common
rulebooks deal with actions. An action is something the player is doing. It's
kind of like the canonical version of the command the player would input at the
prompt.

An example of an action would be something like:

"Taking the torch out of the chest."

That's the Inform-like phrasing, but here we can be a little more terse. The
main idea is that the action has between one and three (main) parts. The verb,
the subject, and the object - but I don't know jack about grammar, so it's
actually going to be the noun and the second noun. That's what Inform calls it
too (coincidence???) so let's not feel too bad.

Ok, back to rulebooks. In our function metaphor, rulebooks are polymorphic over
the type of action being performed, and even the specific parameters of that
action (like whether the noun is a person, for example). There are a few ways we
can accomplish something like that:

+ Manually iterate over rulebooks, testing the action against the rulebook to
see if it's applicable.
+ Try to rig up `match` to see if we can perform the above dispatch through
destructuring.
+ Use multimethods.

|#

;; Datatypes and Stuff ---------------------------------------------------

(struct Rulebook (rules))
(struct Rule (bindings fn))

;; Ok, so it turns out *rules* are the ones that have special dispatch
;; ...rules... It doesn't change anything but we almost screwed this up big
;; time.

;; Sketching out the new new define/rule macro


;; vvv This rule doesn't have a basis, but it has a name.
#;(define/rule
  #:name &advance-time
  #:for core/&every-turn
  #:rule ([] (ecs/select-one ([g Global])
                             (struct-update! Global current-time g add1))))

;; Some definitions used in the next rule.
#;(define [anything? x] #t)
#;(define nil '())

;; This rule has a basis, but no name. You can have both too.
#;(define/rule
  #:basis (Action 'open anything? nil)
  #:rule  ([Action verb noun sec]
           (printf "~nYou open the ~a" noun)))

(define-syntax [define/rule* stx]
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



(define-syntax [define/rule2 stx]
  (syntax-parse stx
    [(_  name:id
         #:for  match-binding
         #:rule match-let-binding rule-stmts ...+)

     #'(define name
         (Rule #'match-binding
               (λ (x)
                 (match-let ([match-let-binding x])
                   rule-stmts ...))))]))

(define [try-rule rule action]
  (let ([pat (Rule-bindings rule)]
        [fn  (Rule-fn       rule)])
    (eval #`(match #,action
              [#,pat (#,fn #,action)]
              [_ #f]))))

;; ^^^ Ok, so this sucks - rules are kind of part of the "inner loop" of the
;; game engine, so having all these match statements and evals is probably not
;; going to work out that great in the end. But still - we can always write new
;; macros that specialize on Action-based rules, and handle them in a more
;; straight-forward way (hopefully without any other code needing to change).

(struct Action (verb noun sec) #:transparent)















