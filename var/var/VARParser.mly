%{
  open Lexing
  open Tagset
  open ARTTree
  open IMPTree
  open CLLTree
  open FUNTree
  open VARTree

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

  let make_genv globals =
    List.fold_left (fun acc d ->
      let name =
        match d with
        | Fun f -> f.name
        | Var (v, _) -> v
      in
      try
        add name.contents acc
      with DuplicateElement _ ->
        raise_duplicate_element name
    ) empty globals

%}

%token VAR
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
%type <VARTree.var_prog ARTTree.compiler_type> program
%type <string ARTTree.node * VARTree.var_expression> variable_declaration
%type <FUNTree.parameter> parameter;
%type <VARTree.var_function> function_definition;
%type <VARTree.var_instrs> instructions
%type <VARTree.var_expression> expr
%type <VARTree.var_expression> l_expr
%type <VARTree.var_instr> instruction
%type <VARTree.var_instr> assign
%type <VARTree.var_instrs> block
%type <VARTree.var_instr> control

%nonassoc NO_ELSE
%nonassoc ELSE
%left AND OR
%left EQ NEQ LT LE GT GE
%left ADD SUB
%left MULT DIV REM
%left NOT CPL

%%

program:
| globals=list(global_declaration) EOF
    {
      {syntax_tree = globals; tag_set = make_genv globals}
    }

| error
    { 
      raise_syntax_error $startpos "FUN program structure: list of function declarations .data <declarations>" 
    }
;

variable_declaration:
| VAR name=LABEL ASSIGN e=expr
    {
      (make_node $startpos name, e)
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

global_declaration:
| f=function_definition
    { Fun f }

| v=variable_declaration SEMI
    { Var v }
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
    { Deref l }
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
| ADDRESS e=l_expr
    { e }
| f=l_expr LP args=separated_list(COMMA, expr) RP
    { Call(f, args) }
;

l_expr:
| t=LABEL
    { Id (make_node $startpos t) }
| MULT LP l=expr RP (* La grammaire a changé par rapport à FUN pour éviter les ambiguïtés ! *)
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

| RETURN e=expr
    { Return e }

| a=assign
    { a }

| f=l_expr LP args=separated_list(COMMA, expr) RP
    {
      Call(f, args) 
    }

| v=variable_declaration
    { Declaration v }
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

init_for:
| a=assign { a }
| v=variable_declaration { Declaration v }
;

control:
| IF LP e=expr RP b=block %prec NO_ELSE
    {
      VARTree.If(e, b)
    } 

| IF LP c=expr RP t=block ELSE e=block
    { 
      IfElse(c, t, e)
    }

| WHILE LP e=expr RP b=block
    {
      While(e, b)
    }

| FOR LP init=separated_list(COMMA, init_for) SEMI cond=expr SEMI it=separated_list(COMMA, assign) RP b=block
    {
      For(init, cond, it, b)
    }

| WHILE block
    {
      raise_syntax_error $startpos "No condition found for 'while'"
    }

| FOR LP expr SEMI separated_list(COMMA, assign) RP block
| FOR LP separated_list(COMMA, init_for) SEMI separated_list(COMMA, assign) RP block
| FOR LP separated_list(COMMA, init_for) SEMI expr RP block
| FOR LP separated_list(COMMA, init_for) RP block
    {
      raise_syntax_error $startpos "Ill-formed 'for' loop"
    }

| FOR LP expr RP block
    {
      raise_syntax_error $startpos "Ill-formed 'for' loop; You may want to use a 'while' loop instead."
    }
;