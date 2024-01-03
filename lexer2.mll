{
  open Parser

  let get_rest str = String.sub str 2 ((String.length str) - 2)
}

  rule decoupe = parse
| [' ''\t''\n']+ { decoupe lexbuf }

(* Syntaxic elements *)
(*  | "//"[^'\n']*'\n' { decoupe lexbuf }
  | "/*"(_)*"*/" { decoupe lexbuf }
*)
  | ("r#"['a'-'z''A'-'Z''_']['a'-'z''A'-'Z''_''0'-'9']*) { VARNAME (get_rest (String.trim (Lexing.lexeme lexbuf))) }
  | ';' { SCOLON }
  | ',' { COMMA }
  | '{' { LBRACE }
     | '}' { RBRACE }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | ':' { OFTYPE }
  | "->" { GIVES }
  | "'" { TAG }
  | ':' { TPT }
  | eof { END }
  | '.' { DEC }

(* Types *)
  | 'i'("8"|"16"|"32"|"64"|"128"|"size") { ISUF (String.trim (Lexing.lexeme lexbuf)) }
  | 'u'("8"|"16"|"32"|"64"|"128"|"size") { USUF (String.trim (Lexing.lexeme lexbuf)) }
  | 'f'("32"|"64") { FSUF (String.trim (Lexing.lexeme lexbuf)) }
  | "bool" { BSUF (String.trim (Lexing.lexeme lexbuf)) }

 (* Consts *)
  | ['0'-'9']['_''0'-'9']* { DCONST (String.trim (Lexing.lexeme lexbuf)) }
  | "0b"(['0''1']('_')*)+ { BCONST (String.trim (Lexing.lexeme lexbuf)) }
  | "0o"(['0'-'7']('_')*)+ { OCONST (String.trim (Lexing.lexeme lexbuf)) }
  | "0x"(['0'-'9''a'-'f''A'-'F']('_')*)+ { HCONST (String.trim (Lexing.lexeme lexbuf)) }

  | ['e''E']['+''-']?(['0'-'9']('_')*)+ { EXP (String.trim (Lexing.lexeme lexbuf)) }

(* Uops *)
  | '&' { REF }

  | '*' { ASTK }
  | '-' { MINUS }
  | '!' { NOT }

(* Bops *)
  | '+' { PLUS }
  | '/' { DIV }
  | '%' { MOD }
  | '&' { LAND }
  | '|' { LOR }
  | '^' { LXOR }
  | "<<" { SLLI }
  | ">>" { SLRI }
  | '=' { EQ }
  | "!=" { NEQ }
  | '<' { LT }
  | "<=" { LEQT }
  | '>' { GT }
  | ">=" { GEQT }
  | "&&" { BWAND }
  | "||" { BWOR }


(* Keywords & reserved words *)
  | "as" { AS }
  | "break" { BREAK }
  | "continue" { CONTINUE }
  | "else" { ELSE }
  | "false" { FALSE }
  | "fn" { FN }
  | "if" { IF }
  | "let" { LET }
  | "loop" { LOOP }
  | "mut" { MUT }
  | "return" { RETURN }
  | "true" { TRUE }
  | "while" { WHILE }
  | "abstract" { ABSTRACT }
  | "async" { ASYNC }
  | "await" { AWAIT }
  | "become" { BECOME }
  | "box" { BOX }
  | "const" { CONST }
  | "crate" { CRATE }
  | "do" { DO }
  | "dyn" { DYN }
  | "enum" { ENUM }
  | "extern" { EXTERN }
  | "final" { FINAL }
  | "for" { FOR }
  | "impl" { IMPL }
  | "in" { IN }
  | "macro" { MACRO }
  | "match" { MATCH }
  | "mod" { MOD }
  | "move" { MOVE }
  | "override" { OVERRIDE }
  | "priv" { PRIV }
  | "pub" { PUB }
  | "ref" { REF }
  | "self" { SELF }
  | "Self" { SSELF }
  | "static" { STATIC }
  | "super" { SUPER }
  | "trait" { TRAIT }
  | "type" { TYPE }
  | "typeof" { TYPEOF }
  | "unsafe" { UNSAFE }
  | "unsized" { UNSIZED }
  | "use" { USE }
  | "virtual" { VIRTUAL }
  | "where" { WHERE }
  | "yield" { YIELD }


(* Identifiers *)
  | ['A'-'Z''a'-'z''_']['A'-'Z''a'-'z''0'-'9''_']* { VARNAME (String.trim (Lexing.lexeme lexbuf)) }



  | _ { begin Printf.printf "Unknown token: in %s\n" (Lexing.lexeme lexbuf);
		failwith "Not_found" end }
