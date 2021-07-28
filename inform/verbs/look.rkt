#lang racket

(require (except-in wizard contains?))
(require "../core.rkt")


(define/rule
  #:for look/report
  #:basis (action look _ _)
  #:rule ((action _ obj _)
          (let/entity obj ([(thing name desc) thing])
                      (printf "~n~a :: ~a" name desc)
                      ; If this is a container, display its contents as well.
                      (let ([contents (ent/contents obj)])
                        (when contents
                          (printf "~n~nContents:")
                          (map (λ (i) (let/entity i ([(thing name desc) thing])
                                                  (printf "~n * ~a :: ~a"
                                                          name
                                                          desc)))
                               contents)
                          (printf "~n~n"))))))

(define (ent/has? ent [comp #f])
  (if comp
      (has? ent comp)
      (λ (e) (has? e ent))))

(define (::contents ent)
  (for/list ([ent (in-list (ent/contents ent))])
    (or (and (component:entity? struct:thing ent)
             (thing-name (component:for-entity struct:thing ent)))
        (and (component:entity? struct:room ent)
             (room-name (component:for-entity struct:room ent))))))

(define/rule
  #:for look/report
  #:basis (action 'look (? (ent/has? struct:container)) #f)
  #:rule ((action _ obj _)
          (printf "~n~a" (::contents obj))))

