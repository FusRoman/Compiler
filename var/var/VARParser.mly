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

  (* 
    Au départ je voulais utiliser $loc, mais pour une raison inconnue menhir ne reconnaît pas cette variable 
    quand bien même elle apparaît dans sa documentation.
    A la place, il faut donc utiliser $startpos(symbol) et endpos(symbol).
  *)
  let get_left start _end  e =
    match e with
    | Deref e -> e
    | _ -> 
      let line = get_line start in
      let column = get_column start in
      raise (SyntaxError(
        Printf.sprintf "Line %d, character %d to line %d, character %d: this is not a left expression" line column (get_line _end) (get_column _end),
        line, column
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
%type <FUNTree.parameter> parameter
%type <VARTree.var_function> function_definition
%type <VARTree.var_instrs> instructions
%type <VARTree.var_expression> expr
%type <VARTree.var_expression> simple_expr
%type <VARTree.var_instr> instruction
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
      raise_syntax_error $startpos "Syntax error" 
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
| e1=expr op=binop e2=expr
    { Binop(e1, op, e2) }
| op=unop e=expr
    { Unop(op, e) }
| f=simple_expr LP args=separated_list(COMMA, expr) RP
    { Call(get_left $startpos(f) $endpos(f) f, args) }
| e=simple_expr
    { e }
;

simple_expr:
| i=INT 
    { Int i }
| b=BOOL 
    { Bool b }
| t=LABEL
    { Deref(Id(make_node $startpos t)) }
| LP e=expr RP
    { e }
| MULT l=simple_expr
    { Deref l }
| ADDRESS l=simple_expr
    { get_left $startpos(l) $endpos(l) l }
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

| l=expr op=assign_binop e=expr
    {
      BinopAssign(get_left $startpos(l) $endpos(l) l, op, e)
    }

| l=expr op=assign_unop
    {
      UnopAssign(get_left $startpos(l) $endpos(l) l, op)
    }

| f=simple_expr LP args=separated_list(COMMA, expr) RP
    {
      Call(get_left $startpos(f) $endpos(f) f, args) 
    }

| v=variable_declaration
    { Declaration v }
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

| FOR LP init=separated_list(COMMA, any_instruction) SEMI cond=expr SEMI it=separated_list(COMMA, any_instruction) RP b=block
    {
      For(init, cond, it, b)
    }
;

any_instruction:
| i=instruction { i }
| c=control {c}
;