type token =
  | DCONST of (int)
  | VARNAME of (string)
  | BCONST of (string)
  | OCONST of (string)
  | HCONST of (string)
  | EXP of (string)
  | REF
  | MUT
  | NEG
  | NOT
  | ASTK
  | PLUS
  | MINUS
  | DIV
  | MOD
  | LAND
  | LOR
  | LXOR
  | SLLI
  | SLRI
  | EQ
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
  | TAG
  | TPT
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
  | USETYPE of (string)
  | LET
  | RETURN

val s :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.ast
