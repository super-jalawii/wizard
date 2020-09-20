#lang brag

w-script         : [w-statement]*
w-statement      : w-switch | w-if | w-assignment | w-block | w-expr-stmt
w-expr-stmt      : w-expr /";"
w-block-begin    : /"{"
w-block-end      : /"}"
w-block          : w-block-begin w-statement* w-block-end

w-if             : /"if" /["("] w-expr /[")"] w-statement [w-else-if]* [w-else]
w-else-if        : /"else" /"if" /["("] w-expr /[")"] w-statement
w-else           : /"else" w-statement

w-switch         : w-switch-decl /w-block-begin w-switch-case+ [w-switch-default] /w-block-end
w-switch-decl    : /"switch" /"(" w-expr /")"
w-switch-case    : /"case" w-expr /":" w-statement* [w-switch-break]
w-switch-break   : /"break" /";"
w-switch-default : /"default" /":" w-statement

w-lst-accessor   : w-identifier /"[" /"|" w-expr /"]"
w-arr-accessor   : w-identifier /"[" /"@" w-expr /"]"
w-map-accessor   : w-identifier /"[" /"?" w-expr /"]"
w-grd-accessor   : w-identifier /"[" /"#" w-expr /"]"

@w-expr : /"(" w-expr /")" | (w-value | w-func-app | w-unary | w-binary | w-ternary | w-accessor)

w-func-app       : IDENTIFIER /"(" w-arglist /")"
@w-arglist        : [w-expr] [/"," w-expr]*
w-ternary        : w-expr /"?" w-expr /":" w-expr
@w-cond-op        : "&&" | "||" | "==" | "<=" | ">=" | "!=" | "<" | ">"
@w-value          : w-identifier | INTEGER | DECIMAL | HEX-INT | STRING | BOOLEAN | "null"
w-unary          : ("!" | "-" | "~") w-expr
w-binary         : w-expr [w-bin-op w-expr]*
@w-bin-op         : "+" | "-" | "*" | "/" | "%" | w-cond-op
@w-identifier     : IDENTIFIER | w-special-ident
w-special-ident  : "global" | "self" | "noone"
w-accessor       : w-expr (/"." IDENTIFIER)+
w-var-assign     : ("var" | "globalvar") IDENTIFIER
w-upd-assign     : w-expr w-assign-op w-expr ";"
w-assignment     : w-var-assign | w-upd-assign
w-assign-op      : "=" | w-op-assign-op
@w-op-assign-op   : "+=" | "-=" | "*=" | "/="


