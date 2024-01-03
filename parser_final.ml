open Ast
let lexbuf = Lexing.from_channel stdin

let _ =
  while true do
    let ast = Parser.s (Lexer2.decoupe) lexbuf
    in
    Ast.affiche ast;
    print_newline ()
  done
