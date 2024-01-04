type token =
  | DCONST of (string)
  | VARNAME of (string)
  | BCONST of (string)
  | OCONST of (string)
  | HCONST of (string)
  | EXP of (string)
  | AND
  | MUT
  | NEG
  | NOT
  | ASTK
  | EQ
  | PLUS
  | MINUS
  | DIV
  | MOD
  | LOR
  | LXOR
  | SLLI
  | SLRI
  | DEQ
  | NEQ
  | LT
  | LEQT
  | GT
  | GEQT
  | BWAND
  | BWOR
  | COMMENT
  | FORCEIDENT
  | AS
  | LBLSPEC
  | CONTINUE
  | BREAK
  | WHILE
  | LOOP
  | IF
  | ELSE
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | SCOLON
  | COMMA
  | OFTYPE
  | GIVES
  | DEC
  | TAG of (string)
  | NEWLINE
  | END
  | FN
  | TRUE
  | FALSE
  | ABSTRACT
  | ASYNC
  | AWAIT
  | BECOME
  | BOX
  | CONST
  | CRATE
  | DO
  | DYN
  | ENUM
  | EXTERN
  | FINAL
  | FOR
  | IMPL
  | IN
  | MACRO
  | MATCH
  | MOVE
  | OVERRIDE
  | PRIV
  | PUB
  | REF
  | SELF
  | SSELF
  | STATIC
  | SUPER
  | TRAIT
  | TYPE
  | TYPEOF
  | UNSAFE
  | UNSIZED
  | USE
  | VIRTUAL
  | WHERE
  | YIELD
  | ISUF of (string)
  | USUF of (string)
  | FSUF of (string)
  | BSUF of (string)
  | LET
  | RETURN

val s :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.ast
