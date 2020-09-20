#lang br

(require brag/support)

(provide yueng-lexer)

(define [lex str]
  (apply-port-proc yueng-lexer str))

(define-lex-abbrev identifier (:seq (:/ "azAZ")
                                    (:+ (:? (:or "_" (:/ "azAZ09"))))))

(define-lex-abbrev digits (:+ (:/ "09")))
(define-lex-abbrev hexdigits (:seq "$" (:+ (:/ "afAF09"))))
(define-lex-abbrev sign (:or (:? "+") (:? "-")))

(define yueng-lexer
  (lexer-srcloc
   [whitespace            (token 'DELIM #:skip? #t)       ]
   [(from/to "//" "\n")   (token 'LINE-COMMENT #:skip? #t)]
   [(:or "++"     "--"        "&&"
         "||"     "<="        ">="
         "=="     "!="        "+="
         "-="     "*="        "/=")
                          (token lexeme        lexeme)    ]
   [(:or "var"    ";"         ","
         "."      "("         ")"
         "+"      "-"         "*"
         "&"      "|"         "^"
         "!"      "<"         ">"
         "|"      ":"         "?"
         "/"      "="         "div"
         "repeat" "with"      "static"
         "{"      "}"         "["
         "]"      "if"        "for"
         "else"   "until"     "begin"
         "global" "globalvar" "~"
         "other"  "self"      "all"
         "noone"  "enum"      "while"
         "switch" "case"      "do"
         "break"  "continue"  "null"
         "exit"   "function"  "%"
         "return" "default"   "constructor")
                          (token lexeme      lexeme)]
   [(:seq sign digits)    (token 'INTEGER    (string->number lexeme))]
   [(:seq "$" hexdigits)  (token 'HEX-INT    lexeme)]
   [(:seq sign digits "." digits)
    (token 'DECIMAL                          (string->number lexeme))]
   [(:or (from/to "\"" "\"")
         (from/to "\'" "\'"))
    (token 'STRING     (substring lexeme
                                   1 (sub1 (string-length lexeme))))]
   [(:or "false" "true")  (token 'BOOLEAN    lexeme)]
   [identifier            (token 'IDENTIFIER (string->symbol lexeme))]))

