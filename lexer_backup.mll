{
  (*open Parser*)
}

  rule decoupe = parse
| [' ''\t']+ { decoupe lexbuf }

(* Syntaxic elements *)
  | "//(.)*(\\n)" { Printf.printf "COMMENT" }
  | "/\*(.)*\*/" { Printf.printf "COMMENT" }

  | "r#" { Printf.printf "FORCEIDENT" }
  | ';' { Printf.printf "SCOLON" }

(* Uops *)
  | '&' { Printf.printf "REF" }


  | '-' { Printf.printf "MINUS" }

(* Bops *)
  | '+' { Printf.printf "PLUS" }
  | '*' { Printf.printf "TIMES" }
  | '/' { Printf.printf "DIV" }
  | '%' { Printf.printf "MOD" }
  | '&' { Printf.printf "LAND" }
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
