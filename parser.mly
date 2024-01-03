%{
    open Ast
  %}

/* Types */
%token<int> DCONST
%token<string> VARNAME BCONST OCONST HCONST EXP

%type<Ast.ast> e
%type<Ast.ast> d
%type<Ast.ast> s

/* Uops */
%token REF MUT NEG NOT ASTK
/* Bops */
%token PLUS MINUS DIV MOD LAND LOR LXOR SLLI SLRI EQ NEQ LT LEQT GT GEQT BWAND BWOR

%token COMMENT
%token FORCEIDENT AS LBLSPEC CONTINUE BREAK WHILE LOOP WHILE IF ELSE
%token LPAREN RPAREN LBRACE RBRACE SCOLON COMMA OFTYPE GIVES TAG TPT
%token NEWLINE
%token END

/* Keywords / reserved */
%token FN TRUE FALSE
%token ABSTRACT ASYNC AWAIT BECOME BOX CONST CRATE DO DYN ENUM EXTERN FINAL FOR IMPL IN MACRO MATCH MOD MOVE OVERRIDE PRIV PUB REF SELF SSELF STATIC SUPER TRAIT TYPE TYPEOF UNSAFE UNSIZED USE VIRTUAL WHERE YIELD
%token<string> USETYPE

/* Declarations */
%token LET RETURN


%start s
/* Precedence */
/* Order follows Rust doc */
%right EQ NEQ LT LEQT GT GEQT
%left SCOLON
%left PLUS MINUS SLLI SLRI LAND LXOR LOR BWAND BWOR
%left ASTSK DIV MOD
%right RBRACE
%left LBRACE

%%

s: fseq END { $1 }
fseq: f { $1 }
  | f fseq { Seq($1, $2) }

/* not very effective */
f: FN VARNAME LPAREN ps RPAREN vers b { Defun(Id($2), $4, $6, $7) }/*
  | FN VARNAME LPAREN RPAREN vers b { Defun(Id($2), Empty, $5, $6) }
  | FN VARNAME LPAREN ps RPAREN b { Defun(Id($2), $4, Empty, $6) }
  | FN VARNAME LPAREN RPAREN b { Defun(Id($2), Empty, Empty, $5) }*/

tag: TAG VARNAME { Tag($2) }

ps: /* empty */ { Empty }
  | pf COMMA { $1 }
  | pf { $1 }
  | pf COMMA pf { Seq($1, $3) }

pf: p OFTYPE t { Oftype($1, $3) }

b: LBRACE s RBRACE { $2 }
s: /* empty */ { Empty }
  | ds { $1 }
  | ds e { Seq($1, $2) }
  | esb { $1 }

ds: d { $1 }
  | d ds { Seq($1, $2) }

d: SCOLON { Empty }
  | e SCOLON { $1 }
  | LET p vers eo SCOLON { Setf($2, $3, $4) }
  | f { $1 }

eo: /* empty */ { Empty }
  | EQ e { $2 }

p: VARNAME { Id($1) }
  | MUT VARNAME { Mut(Id($2)) }

vers: /* empty */ { Empty }
  | GIVES t { $2 }

t: LPAREN t RPAREN { $2 }
  | USETYPE { Type($1) }

x: VARNAME { Id($1) }
  | ASTK x { Deref($2) }

e: esb { $1 }
  | eb { $1 }

esb: DCONST { ICte($1) }
  | VARNAME { Id($1) }

  | x EQ esb { Aff($1, $3) }
  | esb LPAREN ps RPAREN { Call($1, $3) }

  | REF esb { Ref($2) }
  | MUT esb { Mut($2) }
  | MINUS esb { Neg($2) }
  | NOT esb { Not($2) }
  | ASTK esb { Deref($2) }

  | esb PLUS esb { Add($1, $3) }
  | esb MINUS esb { Sub($1, $3) }
  | esb ASTK esb { Mul($1, $3) }
  | esb DIV esb { Div($1, $3) }
  | esb MOD esb { Mod($1, $3) }
  | esb LAND esb { Land($1, $3) }
  | esb LOR esb { Lor($1, $3) }
  | esb LXOR esb { Lxor($1, $3) }
  | esb SLLI esb { Slli($1, $3) }
  | esb SLRI esb { Slri($1, $3) }
  | esb EQ esb { Eq($1, $3) }
  | esb NEQ esb { Neq($1, $3) }
  | esb LT esb { Lt($1, $3) }
  | esb LEQT esb { Leqt($1, $3) }
  | esb GT esb { Gt($1, $3) }
  | esb GEQT esb { Geqt($1, $3) }
  | esb BWAND esb { Bwand($1, $3) }
  | esb BWOR esb { Bwor($1, $3) }

  | LPAREN esb RPAREN { Paren($2) }

  | esb AS t { Cast($1, $3) }

  | CONTINUE lo { Continue($2) }
  | BREAK lo eo { Break($2, $3) }
  | RETURN eo { Return($2) }

lo: /* empty */ { Empty }
  | TAG VARNAME { Tag($2) }

lob: /* empty */ { Empty }
  | TAG VARNAME TPT { Tag($2) }

eb: lob b { Lblk($1, $2) }
  | lob LOOP b { Loop($1, $3) }
  | lob WHILE e b { While($1, $3, $4) }
  | ifs { $1 }

ifs: IF e b { If($2, $3) }
  | IF e b ELSE bif { Ifelse($2, $3, $5) }

bif: b { $1 }
  | ifs { $1 }
