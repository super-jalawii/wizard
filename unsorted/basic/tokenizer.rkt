#lang br

(require "lexer.rkt" brag/support)

(provide make-tokenizer)

(define (make-tokenizer ip [path #f])
  (port-count-lines! ip)
  (lexer-file-path path)
  (define (next-token) (basic-lexer ip))
  next-token)

