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


val affiche : ast -> unit
