#lang racket

(require (except-in wizard contains?))
(require "../core.rkt"
         "../systems/map.rkt")

(define/rule
  #:for go/before
  #:basis (action 'go _ _)
  #:rule ((action 'go dir _)
          (let* ([current-loc (ent/location)]
                 [dest-loc (current-loc . room/towards . dir)])
            (if dest-loc
                (begin
                  (printf "Going ~a from ~a towards the ~a"
                          dir
                          (ent/name-of current-loc)
                          (ent/name-of dest-loc))
                  (ent/move-to (*current-actor*) dest-loc))
                (printf "You can't go that way.")))))


