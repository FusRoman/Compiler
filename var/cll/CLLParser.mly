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
%type <CLLTree.cll_instr> assign
%type <CLLTree.cll_instrs> block
%type <CLLTree.cll_instrs> control
%type <CLLTree.cll_instrs> assigns
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
        let syntax_tree = ProcedureDefinitionData(globals.syntax_tree, data.syntax_tree) in
        {tag_set; syntax_tree}
      with
      | DuplicateElement t ->
        raise_duplicate_element $startpos t
    }

| globals=procedure_definitions EOF
    {
      try
        let syntax_tree = ProcedureDefinition globals.syntax_tree in
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

| RETURN
    {
      Return
    }

| l_e=l_expr LP RP
    {
      Call l_e
    }

| a=assign
    { a }
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

| FOR LP init=assigns SEMI cond=expr SEMI it=assigns RP b=block
    {
      Cycle.from_elt (For(init, cond, it, b))
    }

| WHILE block
    {
      raise_syntax_error $startpos "No condition found for 'while'"
    }

| FOR LP expr SEMI assigns RP block
| FOR LP assigns SEMI assigns RP block
| FOR LP assigns SEMI expr RP block
| FOR LP assigns RP block
    {
      raise_syntax_error $startpos "Ill-formed 'for' loop"
    }

| FOR LP expr RP block
    {
      raise_syntax_error $startpos "Ill-formed 'for' loop; You may want to use a 'while' loop instead."
    }
;

assigns:
| a=assign  { Cycle.from_elt a }
| a=assign COMMA s=assigns { Cycle.prepend s a }
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