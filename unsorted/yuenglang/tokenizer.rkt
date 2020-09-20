#lang br

(require "lexer.rkt" brag/support)

(provide make-tokenizer)

(define (make-tokenizer ip [path #f])
  ;; FIXME: I don't know if this part is BASIC specific..
  (port-count-lines! ip)
  (lexer-file-path path)
  (define (next-token) (yueng-lexer ip))
  next-token)

