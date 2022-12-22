%{

  open Lexing
  open Mml

%}

%token PLUS STAR SLASH MINUS MOD NEG EGAL NEGAL INFSTR INFEGAL ESPESP BARBAR PV SUPSTR SUPEGAL
%token <int> CST
%token <string> IDENT
%token IF THEN ELSE
%token FUN ARROW REC
%token LET AFF IN TYPE
%token EOF
%token PAR_O PAR_F ACC_O ACC_F
%token <bool> BOOL
%token INTEGER BOOLEAN UNIT
%token DPOINTS
%token MUTABLE
%token POINT RARROW

%nonassoc IN
%nonassoc THEN
%nonassoc ELSE
%nonassoc ARROW RARROW

%left BARBAR
%left ESPESP

%left EGAL
%left NEGAL

%left MINUS PLUS
%left STAR SLASH MOD

%nonassoc INFSTR SUPSTR
%nonassoc INFEGAL SUPEGAL


%nonassoc NEG

%left CST IDENT BOOL PV PAR_O

%start program
%type <Mml.prog> program

%%

program:
| d=list(type_def) code=expression EOF { {types=[]; code} }  (*à changer*)
;

type_def:
| TYPE i=IDENT AFF ACC_O t=list(decl_type) ACC_F { t } (* à changer *)
;

decl_type:
| i=IDENT DPOINTS t=typ PV { (i,t) }
| MUTABLE i=IDENT DPOINTS t=typ PV { (i,t) } (*à changer*)

typ:
| INTEGER { TInt }
| BOOLEAN { TBool }
| UNIT { TUnit }
| t1=typ ARROW t2=typ { TFun(t1,t2) }
| t=IDENT { TStrct(t) }
;

simple_expression:
| n=CST { Int(n) }
| t=BOOL { Bool(t) }
| PAR_O PAR_F { Unit }
| i=IDENT { Var(i) }
| e=simple_expression POINT i=IDENT { GetF(e,i) }
(* { [ident = <expr> ;]+ } *)
| PAR_O e=expression PAR_F { e }
;

expression:
| e=simple_expression { e }
| op=unop e=expression { Uop(op,e) }
| e1=expression op=binop e2=expression { Bop(op, e1, e2) }
| e1=expression e2=simple_expression { App(e1,e2) }
| IF e1=expression THEN e2=expression { If(e1,e2,Unit) }
| IF e1=expression THEN e2=expression ELSE e3=expression { If(e1,e2,e3) }
| FUN PAR_O i=IDENT DPOINTS t=typ PAR_F ARROW e=expression { Fun(i,t,e) }
| LET f=IDENT tt=list(let_type_ann) AFF e1=expression IN e2=expression { Let(f,(mk_fun tt e1),e2) }
| LET REC f=IDENT t0=list(let_type_ann) t1=fun_type_ann AFF e1=expression IN e2=expression { Let(f,Fix(f,(mk_fun_type t0 t1),(mk_fun t0 e1)),e2) }
| LET f=IDENT tt=list(let_type_ann) AFF ACC_O e1=list(aff_type) ACC_F IN e2=expression { Let(f,Strct(e1),e2) }
| e1=expression PV e2=expression { Seq(e1,e2) }
| e1=simple_expression POINT i=IDENT RARROW e2=expression { SetF(e1,i,e2) }
;

let_type_ann:
| PAR_O i=IDENT DPOINTS t=typ PAR_F { (i,t) }

fun_type_ann:
| DPOINTS t=typ { t }

aff_type:
| i=IDENT AFF e=expression PV { (i,e) }

%inline unop:
| MINUS { Neg }
| NEG { Not }

%inline binop:
| PLUS { Add }
| STAR { Mul }
| MINUS { Sub }
| SLASH { Div }
| MOD { Mod }
| EGAL { Eq }
| NEGAL { Neq }
| INFSTR { Lt }
| INFEGAL { Le }
| ESPESP { And }
| BARBAR { Or }
| SUPSTR { NLt }
| SUPEGAL { NLe }
;