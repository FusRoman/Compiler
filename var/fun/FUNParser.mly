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
    let f set name =
      if Tagset.mem name.contents set then
        raise_duplicate_element name;
      Tagset.add name.contents set
    in
    let set = List.fold_left (fun acc fct -> 
      f acc fct.name
    ) Tagset.empty fct in
    List.fold_left (fun acc (t, _) -> 
      f acc t
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
%type <FUNTree.fun_function> function_definition;
%type <FUNTree.fun_instrs> instructions
%type <ARTTree.expression> expr
%type <ARTTree.expression> l_expr
%type <FUNTree.fun_instr> instruction
%type <FUNTree.fun_instrs> block
%type <FUNTree.fun_instr> control
%type <string ARTTree.node * int> data_declaration

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
      {syntax_tree = (globals, data); tag_set = genv}
    }

| globals=list(function_definition) EOF
    {
      let genv = make_genv globals [] in
      {syntax_tree = (globals, []); tag_set = genv}
    }

| error
    { 
      raise_syntax_error $startpos "FUN program structure: list of function declarations .data <declarations>" 
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
| e1=expr op=binop e2=expr
    { Binop(e1, op, e2) }
| op=unop e=expr
    { Unop(op, e) }
| ADDRESS e=l_expr 
    { e }
;

l_expr:
| t=LABEL
    { Id (make_node $startpos t) }
| MULT l=expr
    { l }
;

%inline binop:
| ADD         { Add }
| SUB         { Sub }
| MULT        { Mult }
| DIV         { Div }
| REM         { Rem }
| AND         { And }
| OR          { Or }
| LT          { Lt }
| GT          { Gt }
| LE          { Le }
| GE          { Ge }
| EQ          { Eq }
| NEQ         { Neq }
;

%inline unop:
| SUB         { Minus }
| CPL         { Cpl }
| NOT         { Not }
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

| RETURN e=expr
    { Return e }

| l=l_expr op=assign_binop e=expr
    {
      BinopAssign(l, op, e)
    }

| l=l_expr op=assign_unop
    {
      UnopAssign(l, op)
    }

| f=l_expr LP args=separated_list(COMMA, expr) RP
    { Call(f, args) }

| d=l_expr op=assign_binop f=l_expr LP args=separated_list(COMMA, expr) RP
    { SetCall(d, op, f, args) }

| RETURN f=l_expr LP RP
| RETURN LP f=l_expr LP RP RP
    { TerminalCall f }
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

| FOR LP init=separated_list(COMMA, any_instruction) SEMI cond=expr SEMI it=separated_list(COMMA, any_instruction) RP b=block
    {
      For(init, cond, it, b)
    }
;

any_instruction:
| i=instruction { i }
| c=control {c}
;

data_declaration:
| t=LABEL COLON v=INT
    {
      if Tagset.mem t fun_variables then
        raise_reserved_variable $startpos t;
      (make_node $startpos t, v)
    }
| t=LABEL COLON b=BOOL
    {
      if Tagset.mem t fun_variables then
        raise_reserved_variable $startpos t; 
      (make_node $startpos t, Arith.int_of_bool b) 
    }
;