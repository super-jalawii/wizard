#lang br


(require "parser.rkt" "tokenizer.rkt" brag/support)

(define str #<<HERE

// This is a comment. Is it allowed?
// Here is another one.
// And another.

switch (5) {
    case 10: x = 10;
    case 11: x = 100; break;
    default: y = 1;
}

if (xyzzy < 7 && 5) {
   x = 1;
}

draw_text(x, y, "The fee is" + global.member ? "$2.00" : "$10.00");

HERE
  )

(parse-to-datum (apply-tokenizer make-tokenizer str))
