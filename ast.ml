type ast =
  | Empty
  | Type of string
  | Tag of string
  | Seq of ast * ast
  | Oftype of ast * ast
  (* Types *)
  | ICte of string
  | FCte of string
  | BCte of string
  | Id of string
  (* Uops *)
  | Ref of ast
  | Mut of ast
  | Deref of ast
  | Neg of ast
  | Not of ast
  (* Bops *)
  | Add of ast * ast
  | Sub of ast * ast
  | Mul of ast * ast
  | Div of ast * ast
  | Mod of ast * ast
  | Land of ast * ast
  | Lor of ast * ast
  | Lxor of ast * ast
  | Slli of ast * ast
  | Slri of ast * ast
  | Eq of ast * ast
  | Neq of ast * ast
  | Gt of ast * ast
  | Geqt of ast * ast
  | Lt of ast * ast
  | Leqt of ast * ast
  | Bwand of ast * ast
  | Bwor of ast * ast
  (* Exps sans blocs *)
  | Cast of ast * ast
  | Aff of ast * ast
  | Setf of ast * ast * ast
  | Call of ast * ast list
  | Continue of ast
  | Break of ast * ast
  | Return of ast
  | Paren of ast
  (* Exps avec blocs *)
  | Lblk of ast * ast
  | Loop of ast * ast
  | While of ast * ast * ast
  | If of ast * ast
  | Ifelse of ast * ast * ast
  (* Blocks *)
  | Bloc of ast

  | Defun of ast * ast * ast * ast


let print_sep l =
  List.iter print_string l

let rec print_sep_spec = function
  | [] -> ()
  | [x] -> print_string "|-"
  | x :: q -> begin print_string x;
                    print_sep_spec q end


let rec aff_aux indt ast =
  (* TODO this is VERY inefficient *)
  let print_void indt instr =
    print_string (instr ^ "\n")
  in
  let print_mono indt instr next = begin
      print_string (instr ^ "\n");
      print_sep (indt @ ["|\n"]);
      aff_aux (indt @ ["  "]) next end
  and print_duo indt instr left right = begin
      print_string (instr ^ "\n");
      print_sep (indt @ ["|\n"]);
      aff_aux (indt @ ["| "]) left;
      print_sep (indt @ ["|\n"]);
      aff_aux (indt @ ["  "]) right end
  in
  let print_trio indt instr left mid right = begin
      print_duo indt instr left mid;
      print_sep (indt @ ["|\n"]);
      aff_aux (indt @ ["  "]) right end
  in
  let print_four indt instr left a b right = begin
      print_trio indt instr left a b;
      print_sep (indt @ ["|\n"]);
      aff_aux (indt @ ["  "]) right end
  in
  let print_args indt li =
    List.fold_left (fun () elt -> print_mono indt "" elt) () li
  in
  
  print_sep_spec indt;
  match ast with
  | Empty -> print_void indt "(empty)"
  | Type(t) -> let hl = "type: " ^ t in
               print_void indt hl
  | Tag(t) -> let hl = "[tag " ^ t in
              print_void indt hl
  | Seq(al,ar) -> print_duo indt "Seq" al ar
  | Oftype(x,t) -> print_duo indt "Oftype" x t

  | ICte(n) -> let hl = "ICte(" ^ n ^ ")" in
               print_void indt hl
  | FCte(x) -> let hl = "FCte(" ^ x ^ ")" in
               print_void indt hl
  | BCte(x) -> let hl = "BCte(" ^ x ^ ")" in
               print_void indt hl
  | Id(str) -> let hl = "Id(" ^ str ^ ")" in
               print_void indt hl

  | Ref(a) -> print_mono indt "Ref" a
  | Mut(a) -> print_mono indt "Mut" a
  | Deref(a) -> print_mono indt "Deref" a
  | Neg(a) -> print_mono indt "Neg" a
  | Not(a) -> print_mono indt "Not" a
    
  | Add(al,ar) -> print_duo indt "Add" al ar
  | Sub(al,ar) -> print_duo indt "Sub" al ar
  | Mul(al,ar) -> print_duo indt "Mul" al ar
  | Div(al,ar) -> print_duo indt "Div" al ar
  | Mod(al,ar) -> print_duo indt "Mod" al ar
  | Land(al,ar) -> print_duo indt "Land" al ar
  | Lor(al,ar) -> print_duo indt "Lor" al ar
  | Lxor(al,ar) -> print_duo indt "Lxor" al ar
  | Slli(al,ar) -> print_duo indt "Slli" al ar
  | Slri(al,ar) -> print_duo indt "Slri" al ar
  | Eq(al,ar) -> print_duo indt "Eq" al ar
  | Neq(al,ar) -> print_duo indt "Neq" al ar
  | Gt(al,ar) -> print_duo indt "Gt" al ar
  | Geqt(al,ar) -> print_duo indt "Geqt" al ar
  | Lt(al,ar) -> print_duo indt "Lt" al ar
  | Leqt(al,ar) -> print_duo indt "Leqt" al ar
  | Bwand(al,ar) -> print_duo indt "Bwand" al ar
  | Bwor(al,ar) -> print_duo indt "Bwor" al ar

  | Cast(x,t) -> print_duo indt "Cast" x t
  | Aff(al,ar) -> print_duo indt "Aff" al ar
  | Setf(x,t,e) -> print_trio indt "Setf" x t e
  | Call(al,ar) -> begin
      print_mono indt "Call" al;
      print_args indt ar end
  | Continue(e) -> print_mono indt "Continue" e
  | Break(al,ar) -> print_duo indt "Break" al ar
  | Return(e) -> print_mono indt "Return" e
  | Paren(a) -> print_mono indt "()" a

  | Lblk(al,ar) -> print_duo indt "Lblk" al ar
  | Loop(al,ar) -> print_duo indt "Loop" al ar
  | While(al,am,ar) -> print_trio indt "While" al am ar
  | If(al,ar) -> print_duo indt "If" al ar
  | Ifelse(al,at,ae) -> print_trio indt "Ifelse" al at ae

  | Defun(f,arg,tgt,b) -> print_four indt "Defun" f arg tgt b

  | _ -> failwith "hej!"

let affiche = aff_aux []
