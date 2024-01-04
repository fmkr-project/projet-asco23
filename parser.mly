%{
    open Ast
  %}

/* Types */
%token<string> DCONST
%token<string> VARNAME BCONST OCONST HCONST EXP

%type<Ast.ast> e
%type<Ast.ast> d
%type<Ast.ast> s

/* Uops */
%token AND MUT NEG NOT ASTK EQ
/* Bops */
%token PLUS MINUS DIV MOD LOR LXOR SLLI SLRI DEQ NEQ LT LEQT GT GEQT BWAND BWOR

%token COMMENT
%token FORCEIDENT AS LBLSPEC CONTINUE BREAK WHILE LOOP WHILE IF ELSE
%token LPAREN RPAREN LBRACE RBRACE SCOLON COMMA OFTYPE GIVES DEC
%token<string> TAG
%token NEWLINE
%token END

/* Keywords / reserved */
%token FN TRUE FALSE
%token ABSTRACT ASYNC AWAIT BECOME BOX CONST CRATE DO DYN ENUM EXTERN FINAL FOR IMPL IN MACRO MATCH MOD MOVE OVERRIDE PRIV PUB REF SELF SSELF STATIC SUPER TRAIT TYPE TYPEOF UNSAFE UNSIZED USE VIRTUAL WHERE YIELD
/*%token<string> USETYPE*/
%token<string> ISUF USUF FSUF BSUF

/* Declarations */
%token LET RETURN


%start s
/* Precedence */
/* Order follows Rust doc */
%right EQ
%left BWOR
%left BWAND
%nonassoc DEQ NEQ LT LEQT GT GEQT
%left LOR
%left LXOR
%left LAND
%left SLLI SLRI
%left PLUS MINUS
%left ASTSK DIV MOD
%left AS

%left NOT REF MUT NEG

%%

s: fseq END { $1 }
fseq: f { $1 }
  | f fseq { Seq($1, $2) }

f: FN VARNAME LPAREN ps RPAREN vers b { Defun(Id($2), $4, $6, $7) }


ps: /* empty */ { Empty }
  | pf COMMA { $1 }
  | pf { $1 }
  | pf COMMA ps { Seq($1, $3) }

pf: p OFTYPE t { Oftype($1, $3) }

b: LBRACE s RBRACE { $2 }
s: /* empty */ { Empty }
  | esb { $1 }
  | ds esb { Seq($1, $2) }
  | ds { $1 }

ds: d { $1 }
  | ds d { Seq($1, $2) }

d: SCOLON { Empty }
  | e SCOLON { $1 }
  | LET p lvers eo SCOLON { Setf($2, $3, $4) }
  | f { $1 }

lvers: /* empty */ { Empty }
  | OFTYPE t { $2 }

eo: /* empty */ { Empty }
  | EQ e { $2 }

p: VARNAME { Id($1) }
  | MUT VARNAME { Mut(Id($2)) }

vers: /* empty */ { Empty }
  | GIVES t { $2 }

cte: icte { $1 }
  | fcte { $1 }
  | bcte { $1 }

intg: DCONST { $1 }
icte: intg someintsuf { ICte($1) }
  | BCONST someintsuf { ICte($1) }
  | OCONST someintsuf { ICte($1) }
  | HCONST someintsuf { ICte($1) }

fcte: intg FSUF { FCte($1) }
  | intg DEC { FCte($1 ^ ".") }
  | intg DEC intg somef { FCte($1 ^ "." ^ $3) }
  | intg pow somef { FCte($1 ^ $2) }
  | intg DEC intg pow somef { FCte($1 ^ "." ^ $3 ^ $4) }
/* l'information type est perdue */

somef: /* empty */ {}
  | FSUF {}

someintsuf: /* empty */ {}
  | intsuf {}

pow: EXP { $1 }

bcte: TRUE { BCte("true") }
  | FALSE { BCte("false") }

intsuf: ISUF { Type($1) }
  | USUF { Type($1) }

tsuf: intsuf { $1 }
  | FSUF { Type($1) }
  | BSUF { Type($1) }

t: LPAREN tsuf RPAREN { $2 }
  | tsuf { $1 }
  | AND tsuf { $2 }
  | AND MUT tsuf { $3 }
  | LPAREN RPAREN { Type("unit") }
/* Note : on perd l'information ici du REF / MUT */

x: VARNAME { Id($1) }
  | ASTK x { Deref($2) }

arg: /* empty */ { [] }
  | e COMMA { [$1] }
  | e { [$1] }
  | e COMMA arg { $1::$3 }

esb: cte { $1 }
  | VARNAME { Id($1) }

  | x EQ e { Aff($1, $3) }
  | e LPAREN arg RPAREN { Call($1, $3) }

  | AND MUT e { Mut($3) }
  | AND e { Ref($2) }
  | MINUS e %prec NEG { Neg($2) }
  | NOT e { Not($2) }
  | ASTK e { Deref($2) }

  | e PLUS e { Add($1, $3) }
  | e MINUS e { Sub($1, $3) }
  | e ASTK e { Mul($1, $3) }
  | e DIV e { Div($1, $3) }
  | e MOD e { Mod($1, $3) }
  | e AND e { Land($1, $3) }
  | e LOR e { Lor($1, $3) }
  | e LXOR e { Lxor($1, $3) }
  | e SLLI e { Slli($1, $3) }
  | e SLRI e { Slri($1, $3) }
  | e DEQ e { Eq($1, $3) }
  | e NEQ e { Neq($1, $3) }
  | e LT e { Lt($1, $3) }
  | e LEQT e { Leqt($1, $3) }
  | e GT e { Gt($1, $3) }
  | e GEQT e { Geqt($1, $3) }
  | e BWAND e { Bwand($1, $3) }
  | e BWOR e { Bwor($1, $3) }

  | LPAREN e RPAREN { Paren($2) }

  | e AS t { Cast($1, $3) }

  | CONTINUE lo { Continue($2) }
  | BREAK lo eosb { Break($2, $3) }
  | RETURN eosb { Return($2) }

eosb: /*empty */ { Empty }
  | e { $1 }

tag: TAG { Tag($1) }

lo: /* empty */ { Empty }
  | tag { $1 }

lob: /* empty */ { Empty }
  | tag OFTYPE { $1 }

eb: lob b { Lblk($1, $2) }
  | lob LOOP b { Loop($1, $3) }
  | lob WHILE e b { While($1, $3, $4) }
  | ifs { $1 }

ifs: IF e b { If($2, $3) }
  | IF e b ELSE bif { Ifelse($2, $3, $5) }

bif: b { $1 }
  | ifs { $1 }

e: esb { $1 }
  | eb { $1 }
