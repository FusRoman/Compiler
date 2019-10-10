%{
  open Lexing
  open Tagset
  open ARTTree
  open IMPTree
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
%token NOT CPL
%token ADDRESS
%token SEMI COLON COMMA
%token LP RP LB RB
%token <bool>BOOL
%token <int>INT
%token <string>LABEL
%token EOF

%start program
%type <IMPTree.imp_prog ARTTree.compiler_type> program
%type <IMPTree.imp_instrs ARTTree.compiler_type> instructions
%type <ARTTree.expression> expr
%type <ARTTree.l_expr> l_expr
%type <IMPTree.imp_instr> instruction
%type <IMPTree.imp_instr> assign
%type <IMPTree.imp_instrs ARTTree.compiler_type> block
%type <IMPTree.imp_instrs ARTTree.compiler_type> control
%type <ARTTree.datas ARTTree.compiler_type> data_declarations

%left AND OR
%left EQ NEQ LT LE GT GE
%left ADD SUB
%left MULT DIV REM
%left NOT CPL

%%

program:
| TEXT text=instructions DATA data=data_declarations EOF
    {
      try
        let tag_set = union text.tag_set data.tag_set in
        let syntax_tree = TextData(text.syntax_tree, data.syntax_tree) in
        {tag_set; syntax_tree}
      with
      | DuplicateElement t ->
        let pos = $startpos in
        raise (SyntaxError(
          Printf.sprintf "Tag '%s' is declared at least twice" t,
          pos.pos_lnum, pos.pos_cnum - pos.pos_bol))
    }
| TEXT text=instructions EOF
    { {tag_set = text.tag_set; syntax_tree = Text text.syntax_tree} }
;

instructions:
| i=instruction SEMI s=instructions 
    {
      try
        let syntax_tree = Cycle.prepend s.syntax_tree i in
        {tag_set = s.tag_set; syntax_tree}
      with
      | DuplicateElement t ->
        let pos = $startpos in
        raise (SyntaxError(
          Printf.sprintf "Tag '%s' is declared at least twice" t,
          pos.pos_lnum, pos.pos_cnum - pos.pos_bol))
    }

| t=LABEL COLON s=instructions 
    {
      let pos = $startpos in
      let line = pos.pos_lnum in
      let column = pos.pos_cnum - pos.pos_bol in
      try
        let tag_set = add t s.tag_set in
        let tag = {line; column; contents = t} in
        let syntax_tree = Cycle.prepend s.syntax_tree (TagDeclaration tag) in
        {tag_set; syntax_tree}
      with
      | DuplicateElement t ->
        raise (SyntaxError(
          Printf.sprintf "Tag '%s' is declared at least twice" t,
          line, column))
    }

| c=control s=instructions
    {
      try
        let tag_set = union c.tag_set s.tag_set in
        let syntax_tree = Cycle.extend c.syntax_tree s.syntax_tree in
        {tag_set; syntax_tree}
      with
      | DuplicateElement t ->
        let pos = $startpos in
        raise (SyntaxError(
          Printf.sprintf "Tag '%s' is declared at least twice" t,
          pos.pos_lnum, pos.pos_cnum - pos.pos_bol)) 
    }

|   { {syntax_tree = Cycle.empty_cycle; tag_set = empty} }
;

expr:
| i=INT 
    { Int i }
| b=BOOL 
    { Bool b }
| l=l_expr
    { LExpr l }
| STACKPOINTER
    { StackPointer }
| LP e=expr RP
    { e }
| e1=expr ADD e2=expr
    { Binop(e1, Add, e2) }
| e1=expr SUB e2=expr
    { Binop(e1, Sub, e2) }
| e1=expr MULT e2=expr
    { Binop(e1, Mult, e2) }
| e1=expr DIV e2=expr
    { Binop(e1, Div, e2) }
| e1=expr REM e2=expr
    { Binop(e1, Rem, e2) }
| e1=expr AND e2=expr
    { Binop(e1, And, e2) }
| e1=expr OR e2=expr
    { Binop(e1, Or, e2) }
| e1=expr LT e2=expr
    { Binop(e1, Lt, e2) }
| e1=expr LE e2=expr
    { Binop(e1, Le, e2) }
| e1=expr GT e2=expr
    { Binop(e1, Gt, e2) }
| e1=expr GE e2=expr
    { Binop(e1, Ge, e2) }
| e1=expr EQ e2=expr
    { Binop(e1, Eq, e2) }
| e1=expr NEQ e2=expr
    { Binop(e1, Neq, e2) }
| SUB e=expr
    { Unop(Minus, e) }
| CPL e=expr
    { Unop(Cpl, e) }
| NOT e=expr
    { Unop(Not, e) }
| ADDRESS t=LABEL
    {
      let pos = $startpos in
      let line = pos.pos_lnum in
      let column = pos.pos_cnum - pos.pos_bol in
      Address {line; column; contents = t}
    }
;

l_expr:
| t=LABEL
    { 
      let pos = $startpos in
      let line = pos.pos_lnum in
      let column = pos.pos_cnum - pos.pos_bol in
      Id {line; column; contents = t}
    }

| MULT l=l_expr
    { LStar l }
;

instruction:
| NOP
    { Nop }
| EXIT SEMI
    { Exit }
| PRINT LP e=expr RP  
    { Print e }
| GOTO LP e=l_expr RP
    { Goto e}
| BREAK
    { Break }
| CONTINUE
    { Continue }
| a=assign
    { a }
;

assign:
| l=l_expr ASSIGN e=expr
    { Assign(l, e) }
| l=l_expr INCR
    { (Assign(l, Binop(LExpr l, Add, Int 1))) }
| l=l_expr DECR
    { (Assign(l, Binop(LExpr l, Sub, Int 1))) }
| l=l_expr ADDASSIGN e=expr
    { (Assign(l, Binop(LExpr l, Add, e))) }
| l=l_expr SUBASSIGN e=expr
    { (Assign(l, Binop(LExpr l, Sub, e))) }
| l=l_expr MULTASSIGN e=expr
    { (Assign(l, Binop(LExpr l, Mult, e))) }
| l=l_expr DIVASSIGN e=expr
    { (Assign(l, Binop(LExpr l, Div, e))) }
;

block:
| i=instruction SEMI
    {
      (* Pour l'instant, une déclaration d'étiquette suivie d'une seule intrusction, sans {}, n'est pas autorisée *) 
      {syntax_tree = Cycle.from_elt i; tag_set = empty} 
    }

| LB i=instructions RB
    { i }
;

control:
| IF LP e=expr RP b=block
    {
      let syntax_tree = Cycle.from_elt (If(e, b.syntax_tree)) in
      {syntax_tree; tag_set = b.tag_set}
    }

| IF LP c=expr RP t=block ELSE e=block
    { 
      try
        let syntax_tree = Cycle.from_elt (IfElse(c, t.syntax_tree, e.syntax_tree)) in
        let tag_set = union t.tag_set e.tag_set in
        {tag_set; syntax_tree}
      with
      | DuplicateElement t ->
        let pos = $startpos in
        raise (SyntaxError(
          Printf.sprintf "Tag '%s' is declared at least twice" t,
          pos.pos_lnum, pos.pos_cnum - pos.pos_bol)) 
    }

| WHILE LP e=expr RP b=block
    {
      let syntax_tree = Cycle.from_elt (While(e, b.syntax_tree)) in
      {syntax_tree; tag_set = b.tag_set}
    }

| FOR LP init=assign SEMI cond=expr SEMI it=assign RP b=block
    { 
      let assign = Cycle.from_elt init in
      let block = Cycle.append b.syntax_tree it in
      let syntax_tree = Cycle.append assign (While(cond, block)) in 
      {syntax_tree; tag_set = b.tag_set}
    }
;

data_declarations:
| t=LABEL COLON v=INT s=data_declarations
    {
      let pos = $startpos in
      let line = pos.pos_lnum in
      let column = pos.pos_cnum - pos.pos_bol in
      try
        let tag_set = add t s.tag_set in
        let tag = {line; column; contents = (t, v)} in
        let syntax_tree = Cycle.prepend s.syntax_tree tag in
        {syntax_tree; tag_set}
      with
      | DuplicateElement t ->
        raise (SyntaxError(
          Printf.sprintf "Tag '%s' is declared at least twice" t,
          line, column)) 
    }

|   { {syntax_tree = Cycle.empty_cycle; tag_set = empty} }
;

