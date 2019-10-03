%{
  open Lexing
  open ART_SyntaxTree
%}

%token TEXT DATA
%token PRINT EXIT NOP
%token SEMI TWO_POINT
%token <int>INT
%token EOF
%token LP RP
%token <string>ID
%token <bool>BOOL
%token <char>UNOP
%token JUMP WHEN
%token AFFECT
%token LEFT_EXPR_STAR
%token ADD SUB MUL DIV MOD AND OR INF SUP INF_EQUAL SUP_EQUAL EQUAL NOT_EQUAL 

%left AND OR
%left SUP INF INF_EQUAL SUP_EQUAL EQUAL NOT_EQUAL
%left ADD SUB
%left MUL DIV MOD
%left UNOP

%start source
%type <ART_SyntaxTree.prog> source
%type <ART_SyntaxTree.instrs> instructions
%type <ART_SyntaxTree.instr> instruction
%type <ART_SyntaxTree.l_expr> l_express
%type <ART_SyntaxTree.expression> expression
%type <ART_SyntaxTree.datas> data_declarations
%type <ART_SyntaxTree.data> data_declaration
%type <ART_SyntaxTree.tag> tag

%%

source:
| TEXT text=instructions EOF
    { Prog text }
| TEXT text=instructions DATA data=data_declarations EOF
    { Prog_Data (text,data) }
| error { let pos = $startpos in
          let message =
            Printf.sprintf "Syntax error at %d, %d" pos.pos_lnum (pos.pos_cnum - pos.pos_bol)
          in
          failwith message }
;

instructions:
| (* empty *) { Empty_instr }
| i=instruction SEMI is=instructions { Instrs (i,is) }
| t=tag TWO_POINT is=instructions { Instrs_with_tag (t,is) }
;
  
tag:
|i=ID { Tag (i^":") }
;

instruction:
| PRINT LP e=expression RP { Print e }
| NOP {Nop}
| EXIT {Exit}
| JUMP l_e=l_express { Jump l_e }
| JUMP l_e=l_express WHEN e=expression { JumpWhen (l_e,e) }
| l_e=l_express AFFECT e=expression { Affect (l_e,e) }
;

l_express:
| i=ID { Id i }
| LEFT_EXPR_STAR e=l_express { L_star e }
;

expression:
| i=INT  { Int i }
| l_e=l_express { L_expr l_e }
| b=BOOL { Bool b }
| op=UNOP e=expression {
  if op = '-' then 
    Unop (MINUS,e)
  else if op = '!' then 
    Unop (NOT,e)
  else
    raise ( Failure ( Printf.sprintf "Unknown unary operator '%c'" op))
}
| LP e=expression RP { Expr_parenthese e }
| e1=expression ADD e2=expression { Binop (e1,ADD,e2) }
| e1=expression SUB e2=expression { Binop (e1,SUB,e2) }
| e1=expression DIV e2=expression { Binop (e1,DIV,e2) }
| e1=expression MUL e2=expression { Binop (e1,MULT,e2) }
| e1=expression INF e2=expression { Binop (e1,INF,e2) }
| e1=expression INF_EQUAL e2=expression { Binop (e1,INF_EQUAL,e2) }
| e1=expression SUP e2=expression { Binop (e1,SUP,e2) }
| e1=expression SUP_EQUAL e2=expression { Binop (e1,SUP_EQUAL,e2) }
| e1=expression MOD e2=expression { Binop (e1,REM,e2) }
| e1=expression EQUAL e2=expression { Binop (e1,EQUAL,e2) }
| e1=expression NOT_EQUAL e2=expression { Binop (e1,NOT_EQUAL,e2) }
| e1=expression AND e2=expression { Binop (e1,AND,e2) }
| e1=expression OR e2=expression { Binop (e1,OR,e2) }
;

data_declarations:
| d=data_declaration ds=data_declarations { Datas (d,ds) }
| { Empty_data }
;

data_declaration:
| t=ID TWO_POINT i=INT { Data (t,i) }
;
