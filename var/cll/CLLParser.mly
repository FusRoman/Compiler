%{
  open Lexing
  open Tagset
  open ARTTree
  open CLLTree
  open IMPTree

  let get_line pos =
    pos.pos_lnum

  let get_column pos =
    pos.pos_cnum - pos.pos_bol

  let raise_syntax_error pos msg =
    raise (SyntaxError(msg, get_line pos, get_column pos))

  let raise_duplicate_element pos t =
    raise_syntax_error pos (Printf.sprintf "Tag '%s' is declared at least twice" t)

  let raise_reserved_variable pos var=
    raise_syntax_error pos (Printf.sprintf "'%s' is a reserved variable." var)

  let make_node pos contents =
    {line = get_line pos; column = get_column pos; contents}
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
%type <CLLTree.cll_prog ARTTree.compiler_type> program
%type <CLLTree.cll_instrs> instructions
%type <ARTTree.expression> expr
%type <ARTTree.expression> l_expr
%type <CLLTree.cll_instr> instruction
%type <CLLTree.cll_instrs> block
%type <CLLTree.cll_instrs> control
%type <CLLTree.cll_instrs> for_header
%type <ARTTree.datas ARTTree.compiler_type> data_declarations
%type <(CLLTree.procedure_definition Cycle.cycle) ARTTree.compiler_type> procedure_definitions

%nonassoc NO_ELSE
%nonassoc ELSE
%left AND OR
%left EQ NEQ LT LE GT GE
%left ADD SUB
%left MULT DIV REM
%left NOT CPL

%%

program:
| globals=procedure_definitions DATA data=data_declarations EOF
    {
      try
        let tag_set = union globals.tag_set data.tag_set in
        let syntax_tree = (globals.syntax_tree, data.syntax_tree) in
        {tag_set; syntax_tree}
      with
      | DuplicateElement t ->
        raise_duplicate_element $startpos t
    }

| globals=procedure_definitions EOF
    {
      try
        let syntax_tree = (globals.syntax_tree, Cycle.empty_cycle) in
        {tag_set = globals.tag_set; syntax_tree}
      with
      | DuplicateElement t ->
        raise_duplicate_element $startpos t
    }

| error
    { 
      raise_syntax_error $startpos "CLL program structure: list procedure declaration .data <declarations>" 
    }
;

(* La règle des déclaration de procédure en cll *)
procedure_definitions:
| name=LABEL LP RP LB b=instructions RB s=procedure_definitions 
  {
    if Tagset.mem name cll_variables then
      raise_reserved_variable $startpos name;
    let name_node = make_node $startpos name in
    let decl = {name = name_node; block = b} in
    let syntax_tree = Cycle.prepend s.syntax_tree decl in
    try
      let tag_set = Tagset.add name s.tag_set in
      {syntax_tree; tag_set}
    with DuplicateElement t ->
      raise_duplicate_element $startpos t
  }

| {
    {syntax_tree = Cycle.empty_cycle; tag_set = Tagset.empty}
  }
;

instructions:
| i=instruction SEMI s=instructions 
    {
      Cycle.prepend s i
    }

| c=control s=instructions
| c=control SEMI s=instructions
    {
      Cycle.extend c s
    }

| control COLON instructions
| instruction COLON instructions
    {
      raise_syntax_error $startpos "Expected ';', found ':'."
    }

|   { Cycle.empty_cycle }
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

| RETURN
    {
      Return
    }

| RETURN f=l_expr LP RP
| RETURN LP f=l_expr LP RP RP
  {
    TerminalCall f
  }

| l_e=l_expr LP RP
    {
      Call l_e
    }

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
    {
      Cycle.from_elt i
    }

| c=control
    { c }

| LB i=instructions RB
    { i }
;

control:
| IF LP e=expr RP b=block %prec NO_ELSE
    {
      Cycle.from_elt (CLLTree.If(e, b))
    } 

| IF LP c=expr RP t=block ELSE e=block
    { 
      Cycle.from_elt (CLLTree.IfElse(c, t, e))
    }

| WHILE LP e=expr RP b=block
    {
      Cycle.from_elt (CLLTree.While(e, b))
    }

| FOR LP init=for_header SEMI cond=expr SEMI it=for_header RP b=block
    {
      Cycle.from_elt (For(init, cond, it, b))
    }

| WHILE block
    {
      raise_syntax_error $startpos "No condition found for 'while'"
    }
;

for_header:
|   { Cycle.empty_cycle }
| s=for_header_list { s }
;

for_header_list:
| i=instruction { Cycle.from_elt i }
| i=instruction COMMA s=for_header_list { Cycle.prepend s i }
| c=control { c }
| c=control COMMA s=for_header_list { Cycle.extend s c }
;

data_declarations:
| t=LABEL COLON v=INT s=data_declarations
    {
      if Tagset.mem t cll_variables then
        raise_reserved_variable $startpos t;
      try
        let tag_set = add t s.tag_set in
        let tag = make_node $startpos (t, v) in
        let syntax_tree = Cycle.prepend s.syntax_tree tag in
        {syntax_tree; tag_set}
      with
      | DuplicateElement t ->
        raise_duplicate_element $startpos t 
    }

| t=LABEL COLON b=BOOL s=data_declarations 
    {
      if Tagset.mem t cll_variables then
        raise_reserved_variable $startpos t;
      try
        let tag_set = add t s.tag_set in
        let tag = make_node $startpos (t, Arith.int_of_bool b) in
        let syntax_tree = Cycle.prepend s.syntax_tree tag in
        {syntax_tree; tag_set}
      with
      | DuplicateElement t ->
        raise_duplicate_element $startpos t 
    }

|   { {syntax_tree = Cycle.empty_cycle; tag_set = empty} }
;