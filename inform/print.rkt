#lang racket


(require (except-in wizard contains?)
         "core.rkt"
         "systems/map.rkt")

(provide print/list
         print/room)

(define (print/list lst)
  (let ([lst (if (symbol? (car lst))
                 (map symbol->string lst)
                 lst)]
        [before-last (if (> (length lst) 2) ", and " " and ")])
    (string-join lst ", " #:before-last before-last)))

(define (print/room [r (ent/location)])
  (let/entity r ([(room name desc) room])
              (printf "~n~n~a:~n~n~a~n~nThere are exits ~a.~n"
                      name
                      desc
                      (print/list (room/exits r)))))


