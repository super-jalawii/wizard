#lang racket

(require racket/async-channel)
(require "util.rkt")

(provide *input-channel*
         start-repl)

(define tick-time 5000)

(define (start-repl #:input-channel [in-chan (*input-channel*)]
                    #:on-update     [update-fn (λ () #f)])
  (printf "Starting game thread.")
  (thread (λ ()
            (let loop ([ct (current-inexact-milliseconds)])
              (let ([next-tick (+ ct tick-time)])
                (update-fn)

                (let poll ([in (async-channel-try-get in-chan)])
                  (if in
                      (and (do-action in) (printf "~n~n>> "))
                      (unless (>= (current-inexact-milliseconds) next-tick)
                        (poll (async-channel-try-get in-chan)))))

                (loop (current-inexact-milliseconds)))))))


(define *input-channel* (make-parameter (make-async-channel)))

