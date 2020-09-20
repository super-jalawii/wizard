#lang br


(require "parser.rkt" "tokenizer.rkt" brag/support)

(define str #<<HERE

10 print "Hello" : print "World"
20 goto 9 + 10 + 11
30 end

HERE
  )

(parse-to-datum (apply-tokenizer make-tokenizer str))
