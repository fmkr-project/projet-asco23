open Ast
let lexbuf = Lexing.from_channel stdin

(* 3 *)
let clean_const str =
  (* Remove underscores *)
  String.fold_left
    (fun stac c -> let hl = if c = '_' then "" else String.make 1 c in stac ^ hl)
    ""
    str

let is_const ast = match ast with
  | ICte(_) | FCte(_) | BCte(_) -> true
  | _ -> false

let get_const ast = match ast with
  (* val get_const: Ast.ast -> string *)
  (* Get only the const part *)
  | ICte(k) | FCte(k) | BCte(k) -> clean_const(k)
  | _ -> failwith "Not_a_const"

let get_type ast = match ast with
  | Type(t) -> t
  | _ -> failwith "Not_a_type"

let untyped_const id k = print_string (id ^ " (" ^ k ^ ")\n")

let typed_const id t k = print_string (id ^ ": " ^ t ^ " (" ^ k ^ ")\n")

let rec print_consts ast = match ast with
  (* val print_consts: Ast.ast -> unit *)
  | Seq(al,ar) -> print_consts al; print_consts ar
  (* Cas ou le type n'est pas specifie *)
  | Setf(Id(x), somet, a) ->
     if not (is_const a)
     then ()
     else
       if somet = Empty
       then untyped_const x (get_const a)
       else typed_const x (get_type somet) (get_const a)

  | Lblk(_, b) -> print_consts b
  | Loop(_, b) -> print_consts b
  | If(_, b) -> print_consts b
  | Ifelse(_, bl, br) -> print_consts bl; print_consts br

  | Defun(_, _, _, b) -> print_consts b

  | _ -> ()


(* 4 *)
let rec get_args ast = match ast with
  (* val get_args: Ast.ast -> string list *)
  (* Return function arguments *)
  | Seq(al,ar) -> (get_args al) @ (get_args ar)
  | Oftype(Id(x),_) -> [x]
  | Defun(_,args,_,_) -> get_args args

  | _ -> []

let rec id_has_dec ast x = match ast with
  (* val id_has_dec: Ast.ast -> string -> bool *)
  (* Return true if there exists a declaration of x in ast *)
  | Seq(al,ar) -> (id_has_dec al x) || (id_has_dec ar x)
  | Setf(Id(var),_,_) -> var = x
  | Defun(Id(f),_,_,_) -> f = x
  | _ -> false

let rec get_def_ids ast = match ast with
(* val get_def_ids: Ast.ast -> string list *)
(* Return every id that has a definition *)
  | Seq(al,ar) -> (get_def_ids al) @ (get_def_ids ar)
  | Setf(Id(var),_,_) -> [var]
  | Defun(Id(f),_,_,_) -> f::(get_args ast)
  | _ -> []

let rec get_used_ids ast = match ast with
  (* val get_ids: Ast.ast -> string list *)
  (* Return a list of used ids with dupes *)
  | Seq(al,ar) -> (get_used_ids al) @ (get_used_ids ar)
  | Id(x) -> [x]
  | Ref(a) -> get_used_ids a
  | Mut(a) -> get_used_ids a
  | Deref(a) -> get_used_ids a
  | Neg(a) -> get_used_ids a
  | Not(a) -> get_used_ids a
  | Add(al,ar) -> (get_used_ids al) @ (get_used_ids ar)
  | Sub(al,ar) -> (get_used_ids al) @ (get_used_ids ar)
  | Mul(al,ar) -> (get_used_ids al) @ (get_used_ids ar)
  | Div(al,ar) -> (get_used_ids al) @ (get_used_ids ar)
  | Mod(al,ar) -> (get_used_ids al) @ (get_used_ids ar)
  | Land(al,ar) -> (get_used_ids al) @ (get_used_ids ar)
  | Lor(al,ar) -> (get_used_ids al) @ (get_used_ids ar)
  | Lxor(al,ar) -> (get_used_ids al) @ (get_used_ids ar)
  | Slli(al,ar) -> (get_used_ids al) @ (get_used_ids ar)
  | Slri(al,ar) -> (get_used_ids al) @ (get_used_ids ar)
  | Eq(al,ar) -> (get_used_ids al) @ (get_used_ids ar)
  | Neq(al,ar) -> (get_used_ids al) @ (get_used_ids ar)
  | Gt(al,ar) -> (get_used_ids al) @ (get_used_ids ar)
  | Geqt(al,ar) -> (get_used_ids al) @ (get_used_ids ar)
  | Lt(al,ar) -> (get_used_ids al) @ (get_used_ids ar)
  | Leqt(al,ar) -> (get_used_ids al) @ (get_used_ids ar)
  | Bwand(al,ar) -> (get_used_ids al) @ (get_used_ids ar)
  | Bwor(al,ar) -> (get_used_ids al) @ (get_used_ids ar)
  | Cast(al,ar) -> (get_used_ids al) @ (get_used_ids ar)
  | Aff(al,ar) -> (get_used_ids al) @ (get_used_ids ar)
  | Setf(al,_,ar) -> (get_used_ids al) @ (get_used_ids ar)
  | Call(a,all) ->
     let args = List.fold_left
                  (fun acc ast -> (get_used_ids ast) @ acc)
                  []
                  all
     in (get_used_ids a) @ args
  | Break(_,a) -> get_used_ids a
  | Return(a) -> get_used_ids a
  | Paren(a) -> get_used_ids a
  | Lblk(_,a) -> get_used_ids a
  | Loop(_,a) -> get_used_ids a
  | While(_,cd,a) -> (get_used_ids cd) @ (get_used_ids a)
  | If(cd,a) -> (get_used_ids cd) @ (get_used_ids a)
  | Ifelse(cd,al,ar) -> (get_used_ids cd) @ (get_used_ids al) @ (get_used_ids ar)
  | Bloc(a) -> get_used_ids a
  | Defun(f,_,_,_) -> get_used_ids f

  | _ -> []




let check_one ast =
  (* val check_one: Ast.ast -> bool *)
  let used_ids = List.fold_left (fun acc id -> if List.mem id acc then acc else id::acc)
                   []
                   (get_used_ids ast)
  and def_ids = List.fold_left (fun acc id -> if List.mem id acc then acc else id::acc)
                  []
                  (get_def_ids ast)    
  in
  
  List.fold_left (fun acc id -> acc && List.mem id def_ids)
    true used_ids

(*let rec check_scope ast =
  (* val check_scope: Ast.ast -> bool *)
  | Seq() -> 
 *)


(* 5 *)
let rec get_mutables ast = match ast with
  (* val get_mutables: Ast.ast -> string list *)
  | Seq(al,ar) -> (get_mutables al) @ (get_mutables ar)
  | Ref(id) -> get_mutables id
  | Mut(Id(x)) -> [x] (* Support for mut and &mut *)
  | Setf(id,_,_) -> get_mutables id
  | Defun(_,_,_,b) -> get_mutables b

  | _ -> []

let rec get_refmutables ast = match ast with
  (* val get_refmutables: Ast.ast -> string list *)
  | Seq(al,ar) -> (get_mutables al) @ (get_mutables ar)
  | Ref(Mut(id)) -> get_mutables id
  | Setf(id,_,_) -> get_mutables id
  | Defun(_,_,_,b) -> get_mutables b

  | _ -> []

let check_affect ast =
    (* val check_affect: Ast.ast -> bool *)
  let mutables = get_mutables ast
  and refmutables = get_refmutables ast in
  let rec is_refmut refd = match refd with
    (* refd has at least one Ref component *)
    | Id(x) -> List.mem x refmutables
    | Ref(t) -> is_refmut t
    | _ -> failwith "Not_a_ref"
  in
  let rec aux_affect ast = match ast with
    | Seq(al,ar) -> (aux_affect al) && (aux_affect ar)
    | Aff(Id(x),_) -> List.mem x mutables
    | Aff(hl,_) -> is_refmut hl
    | Defun(_,_,_,b) -> aux_affect b
    | _ -> true
  in
  aux_affect ast


(* 6 *)
let rec force_types ast = match ast with
  (* val force_types: Ast.ast -> Ast.ast *)
  (* Force typing of untyped (type = empty) constants *)
  | Seq(al,ar) -> Seq(force_types al, force_types ar)
  (* TODO cas x = 1+1; *)
  | Setf(x,somet,k) ->
     if (somet = Empty)
     then match k with
          | FCte(_) -> Setf(x,Type("f32"),k)
          | BCte(_) -> Setf(x,Type("bool"),k)
          | ICte(_) | _ -> Setf(x,Type("i32"),k)
     else ast
  | Lblk(lbl,b) -> Lblk(lbl,force_types b)
  | Loop(lbl,b) -> Loop(lbl,force_types b)
  | While(lbl,cd,b) -> While(lbl,cd,force_types b)
  | If(cd,b) -> If(cd,force_types b)
  | Ifelse(cd,bt,bf) -> Ifelse(cd,force_types bt, force_types bf)
  
  | Defun(f,args,dest,b) ->
     Defun(f,args,dest,force_types b)

  | _ -> ast





(* 7 *)
let _ =
  while true do
    let a = Parser.s (Lexer2.decoupe) lexbuf
    in
    let ast = force_types a in
    (*if (check_scope ast) then print_string "scope OK\n"
    else print_string "scope KO\n"; *)
    if (check_affect ast) then print_string "affect OK\n"
    else print_string "affect KO\n";
    print_consts ast;
    Ast.affiche ast;
    print_newline ()
  done
