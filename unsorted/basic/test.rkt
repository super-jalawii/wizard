#lang br

(require "lexer.rkt" brag/support rackunit)

(define (lex str)
  (apply-port-proc basic-lexer str))


