%{
  open Lexing
%}

%token TEXT DATA
%token NOP PRINT EXIT GOTO
%token IF ELSE
%token WHILE FOR 
%token CONTINUE BREAK
%token STACKPOINTER
%token ASSIGN INCR DECR
%token ADDASSIGN SUBASSIGN
%token MULTASSIGN DIVASSIGN
%token EQ NEQ
%token LT LE GT GE
%token ADD SUB
%token MULT DIV REM
%token AND OR
%token OR NOT CPL
%token SEMI COLON
%token LP RP LB RB
%token <bool>BOOL
%token <int>INT
%token <string>LABEL
%token EOF

%start program
%type <IMPTree.imp_prog> program

%left ASSIGN ADDASSIGN SUBASSIGN MULTASSIGN DIVASSIGN
%left AND OR
%left EQ NEQ LT LE GT GE
%left ADD SUB
%left MULT DIV REM
%left NOT CPL

%%

program:
| TEXT text=instructions DATA data=data_declarations EOF
    {}
| TEXT text=instructions EOF
    {}
| error { let pos = $startpos in
          let message =
            Printf.sprintf "Syntax error at %d, %d" pos.pos_lnum (pos.pos_cnum - pos.pos_bol)
          in
          failwith message }
;

instructions:
| i=instruction SEMI s=instructions 
    { Cycle.prepend s i }
| t=LABEL COLON s=instructions 
    { Cycle.prepend s (IMPTree.TagDeclaration t) }
| c=control s=instructions
    { Cycle.extend c s }
|   {}
;

expr:
| i=INT 
    { ARTTree.Int i }
| b=BOOL 
    { ARTTree.Bool b }
| l=l_expr
    { ARTTree.LExpr l }
| STACKPOINTER
    { ARTTree.StackPointer }
| e1=expr op=binop e2=expr2
    { ARTTree.Binop(e1, op, e2) }
| op=unop e=expr
    { ARTTree.Unop(op, e) }
;

binop:
| ADD     { ARTTree.Add }
| SUB     { ARTTree.Sub }
| MULT    { ARTTree.Mult }
| DIV     { ARTTree.Div }
| MOD     { ARTTree.Rem }
| AND     { ARTTree.And }
| OR      { ARTTree.Or }
| LT      { ARTTree.Lt }
| LE      { ARTTree.Le }
| GT      { ARTTree.Gt }
| GE      { ARTTree.Ge }
| EQ      { ARTTree.Eq }
| NEQ     { ARTTree.Neq }
;

unop:
| SUB     { ARTTree.Minus }
| CPL     { ARTTree.Cpl }
| NOT     { ARTTree.Not }
;

l_expr:
| t=LABEL
    { ARTTree.Id t }
| MULT l=l_expr
    { ARTTree.LStar l }
;

instruction:
| NOP
    { IMPTree.Nop }
| EXIT SEMI
    { IMPTree.Exit }
| PRINT LP e=expr RP  
    { IMPTree.Print e }
| GOTO LP e=l_expr RP
    { IMPTree.Goto e}
| BREAK
    { IMPTree.Break }
| CONTINUE
    { IMPTree.Continue }
| a=assign
    { a }
;

assign:
| l=l_expr ASSIGN e=expr
    { IMPTree.Assign(l, e) }
| l=l_expr INCR
    { IMPTree.(Assign(l, Binop(LExpr l, Add, Int 1))) }
| l=l_expr DECR
    { IMPTree.(Assign(l, Binop(LExpr l, Sub, Int 1))) }
| l=l_expr ADDASSIGN e=expr
    { IMPTree.(Assign(LExpr l, Add, e)) }
| l=l_expr SUBASSIGN e=expr
    { IMPTree.(Assign(LExpr l, Sub, e)) }
| l=l_expr MULTASSIGN e=expr
    { IMPTree.(Assign(LExpr l, Mult, e)) }
| l=l_expr DIVASSIGN e=expr
    { IMPTree.(Assign(LExpr l, Div, e)) }
;

block:
| i=instruction SEMI
    { Cycle.from_elt i }
| LB i=instructions RB
    { i }
;

control:
| IF LP e=expr RP b=block
    { Cycle.from_elt IMPTree.If(e, b) }
| IF LP e=expr RP t=block ELSE e=block
    { Cycle.from_elt IMPTree.IfElse(e, t, e) }
| WHILE LP e=expr RP b=block
    { Cycle.from_elt IMPTree.While(e, b) }
| FOR LP init=assign SEMI cond=expr SEMI it=assign RP b=block
    { 
      let assign = Cycle.from_elt init in
      let block = Cycle.push b it in
      Cycle.append assign IMPTree.(While(expr, block)) in 
    }
;

data_declarations:
| d=data_declaration s=data_declarations
    {  }
|   {  }
;

data_declaration:
| l=LABEL COLON v=INT 
    {  }
;

