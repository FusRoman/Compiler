%{
  open Lexing
  open Tagset
  open ARTTree
  open IMPTree
  open CLLTree
  open FUNTree

  let get_line pos =
    pos.pos_lnum

  let get_column pos =
    pos.pos_cnum - pos.pos_bol

  let raise_syntax_error pos msg =
    raise (SyntaxError(msg, get_line pos, get_column pos))

  let raise_duplicate_element t =
    raise (SyntaxError(Printf.sprintf "Tag '%s' is declared at least twice" t.contents, 
      t.line, t.column
    ))

  let raise_reserved_variable pos var =
    raise_syntax_error pos (Printf.sprintf "'%s' is a reserved variable." var)

  let make_node pos contents =
    {line = get_line pos; column = get_column pos; contents}

  let make_genv fct data =
    let f set name line column=
      if Tagset.mem name set then
        raise_duplicate_element {contents = name; line; column};
      Tagset.add name set
    in
    let set = List.fold_left (fun acc fct -> 
      f acc fct.name.contents fct.name.line fct.name.column
    ) Tagset.empty fct in
    List.fold_left (fun acc d -> 
      f acc (fst d.contents) d.line d.column
    ) set data
%}

%token DATA
%token NOP PRINT EXIT
%token IF ELSE NO_ELSE
%token WHILE FOR 
%token CONTINUE BREAK RETURN
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
%type <FUNTree.fun_prog ARTTree.compiler_type> program
%type <FUNTree.parameter> parameter;
%type <FUNTree.function_definition> function_definition;
%type <FUNTree.fun_instrs> instructions
%type <ARTTree.expression> expr
%type <ARTTree.expression> l_expr
%type <FUNTree.fun_instr> instruction
%type <FUNTree.fun_instr> assign
%type <FUNTree.fun_instrs> block
%type <FUNTree.fun_instr> control
%type <(string * int) ARTTree.node> data_declaration

%nonassoc NO_ELSE
%nonassoc ELSE
%left AND OR
%left EQ NEQ LT LE GT GE
%left ADD SUB
%left MULT DIV REM
%left NOT CPL

%%

program:
| globals=list(function_definition) DATA data=list(data_declaration) EOF
    {
      let genv = make_genv globals data in
      let data = List.fold_left (fun acc d -> Cycle.append acc d) Cycle.empty_cycle data in
      {syntax_tree = (globals, data); tag_set = genv}
    }

| globals=list(function_definition) EOF
    {
      let genv = make_genv globals [] in
      {syntax_tree = (globals, Cycle.empty_cycle); tag_set = genv}
    }

| error
    { 
      raise_syntax_error $startpos "CLL program structure: list procedure declaration .data <declarations>" 
    }
;

parameter:
| name=LABEL 
    {
      {name = make_node $startpos name; reference = false}
    }

| ADDRESS name=LABEL
    {
      {name = make_node $startpos name; reference = true}
    }
;

function_definition:
| name=LABEL LP params=separated_list(COMMA, parameter) RP LB b=instructions RB
    {
      {name = make_node $startpos name; params; block = b}
    }
;

instructions:
| i=instruction SEMI s=instructions 
    {
      i::s
    }

| c=control s=instructions
| c=control SEMI s=instructions
    {
      c::s
    }

| control COLON instructions
| instruction COLON instructions
    {
      raise_syntax_error $startpos "Expected ';', found ':'."
    }

|   { [] }
;

expr:
| i=INT 
    { Int i }
| b=BOOL 
    { Bool b }
| l=l_expr
    { LStar l }
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
| ADDRESS e=LABEL
    { Id (make_node $startpos e) }
;

l_expr:
| t=LABEL
    { Id (make_node $startpos t) }
| MULT l=expr
    { l }
;

instruction:
| NOP
    { Nop }
| EXIT
    { Exit }
| PRINT LP e=expr RP  
    { Print e }
| BREAK
    { 
      Break (make_node $startpos ())
    }

| CONTINUE
    { 
      Continue (make_node $startpos ())
    }

| RETURN LP e=expr RP
    { Return e }

| a=assign
    { a }

| f=l_expr LP args=separated_list(COMMA, expr) RP
    { Call(f, args) }

| d=l_expr op=assign_binop f=l_expr LP args=separated_list(COMMA, expr) RP
    { SetCall(d, op, f, args) }
;

assign:
| l=l_expr op=assign_binop e=expr
    {
      BinopAssign(l, op, e)
    }

| l=l_expr op=assign_unop
    {
      UnopAssign(l, op)
    }
;

assign_binop:
| ASSIGN      { Standard }
| ADDASSIGN   { AddAssign }
| SUBASSIGN   { SubAssign }
| MULTASSIGN  { MultAssign }
| DIVASSIGN   { DivAssign }
;

assign_unop:
| INCR        { Incr }
| DECR        { Decr }
;

block:
| i=instruction SEMI
    { [i] }

| c=control
    { [c] }

| LB i=instructions RB
    { i }
;

control:
| IF LP e=expr RP b=block %prec NO_ELSE
    {
      FUNTree.If(e, b)
    } 

| IF LP c=expr RP t=block ELSE e=block
    { 
      IfElse(c, t, e)
    }

| WHILE LP e=expr RP b=block
    {
      While(e, b)
    }

| FOR LP init=separated_list(COMMA, assign) SEMI cond=expr SEMI it=separated_list(COMMA, assign) RP b=block
    {
      For(init, cond, it, b)
    }

| WHILE block
    {
      raise_syntax_error $startpos "No condition found for 'while'"
    }

| FOR LP expr SEMI separated_list(COMMA, assign) RP block
| FOR LP separated_list(COMMA, assign) SEMI separated_list(COMMA, assign) RP block
| FOR LP separated_list(COMMA, assign) SEMI expr RP block
| FOR LP separated_list(COMMA, assign) RP block
    {
      raise_syntax_error $startpos "Ill-formed 'for' loop"
    }

| FOR LP expr RP block
    {
      raise_syntax_error $startpos "Ill-formed 'for' loop; You may want to use a 'while' loop instead."
    }
;

data_declaration:
| t=LABEL COLON v=INT
    {
      if Tagset.mem t fun_variables then
        raise_reserved_variable $startpos t;
      make_node $startpos (t, v)
    }
| t=LABEL COLON b=BOOL
    {
      if Tagset.mem t fun_variables then
        raise_reserved_variable $startpos t; 
      make_node $startpos (t, Arith.int_of_bool b) 
    }
;