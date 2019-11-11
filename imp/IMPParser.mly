%{
  open Lexing
  open Tagset
  open ARTTree
  open IMPTree

  let get_line pos =
    pos.pos_lnum

  let get_column pos =
    pos.pos_cnum - pos.pos_bol

  let raise_syntax_error pos msg =
    raise (SyntaxError(msg, get_line pos, get_column pos))

  let raise_duplicate_element pos t =
    raise_syntax_error pos (Printf.sprintf "Tag '%s' is declared at least twice" t)

  let raise_stack_pointer pos =
    raise_syntax_error pos "'stack_pointer' is a reserved tag."

  let make_node pos contents =
    {line = get_line pos; column = get_column pos; contents}
%}

%token TEXT DATA
%token NOP PRINT EXIT GOTO
%token IF ELSE NO_ELSE
%token WHILE FOR 
%token CONTINUE BREAK
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
%type <ARTTree.expression> l_expr
%type <IMPTree.imp_instr> instruction
%type <IMPTree.imp_instr> assign
%type <IMPTree.imp_instrs ARTTree.compiler_type> block
%type <IMPTree.imp_instrs ARTTree.compiler_type> control
%type <IMPTree.imp_instrs> assigns
%type <ARTTree.datas ARTTree.compiler_type> data_declarations

%nonassoc NO_ELSE
%nonassoc ELSE
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
        raise_duplicate_element $startpos t
    }
| TEXT text=instructions EOF
    { {tag_set = text.tag_set; syntax_tree = Text text.syntax_tree} }
| error
    { 
      raise_syntax_error $startpos "IMP program structure: .text <instructions> .data <declarations>" 
    }
;

instructions:
| i=instruction SEMI s=instructions 
    {
      try
        let syntax_tree = Cycle.prepend s.syntax_tree i in
        {tag_set = s.tag_set; syntax_tree}
      with
      | DuplicateElement t ->
        raise_duplicate_element $startpos t
    }

| t=LABEL COLON s=instructions 
    {
      try
        let tag_set = add t s.tag_set in
        let tag = make_node $startpos t in
        let syntax_tree = Cycle.prepend s.syntax_tree (TagDeclaration tag) in
        {tag_set; syntax_tree}
      with
      | DuplicateElement t ->
        raise_duplicate_element $startpos t
    }

| c=control s=instructions
| c=control SEMI s=instructions (* On accepte aussi ce cas par commodité *)
    {
      try
        let tag_set = union c.tag_set s.tag_set in
        let syntax_tree = Cycle.extend c.syntax_tree s.syntax_tree in
        {tag_set; syntax_tree}
      with
      | DuplicateElement t ->
        raise_duplicate_element $startpos t 
    }

| control COLON instructions
| instruction COLON instructions
    {
      raise_syntax_error $startpos "Expected ';', found ':'."
    }

| t=LABEL SEMI instructions
    {
      raise_syntax_error $startpos "Expected ':', found ';'."
    }

| LABEL instructions
    {
      raise_syntax_error $startpos "You may have forgotten a ':'."
    }

|   { {syntax_tree = Cycle.empty_cycle; tag_set = empty} }
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
| ADDRESS t=LABEL 
    { Id (make_node $startpos t) }
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
| GOTO LP e=l_expr RP
    { Goto e}
| BREAK
    { 
      Break (make_node $startpos ())
    }

| CONTINUE
    { 
      Continue (make_node $startpos ())
    }

| a=assign
    { a }

| t=LABEL LP e=l_expr RP
    {
      raise_syntax_error $startpos (Printf.sprintf "Unknown instruction '%s'." t)
    }
;

assign:
| l=l_expr op=assign_binop e=expr
    {
      simplify_assign_binop l op e
    }

| l=l_expr op=assign_unop
    {
      simplify_assign_unop l op
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
      (* Pour l'instant, une déclaration d'étiquette suivie d'une seule intrusction, sans {}, n'est pas autorisée *) 
      {syntax_tree = Cycle.from_elt i; tag_set = empty} 
    }

| c=control 
    { c }

| LB i=instructions RB
    { i }
;

control:
| IF LP e=expr RP b=block %prec NO_ELSE
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
        raise_duplicate_element $startpos t
    }

| WHILE LP e=expr RP b=block
    {
      let syntax_tree = Cycle.from_elt (While(e, b.syntax_tree)) in
      {syntax_tree; tag_set = b.tag_set}
    }

| FOR LP init=assigns SEMI cond=expr SEMI it=assigns RP b=block
    {
      {syntax_tree = for_to_while init cond it b.syntax_tree; 
      tag_set = b.tag_set}
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