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
%type <IMPTree.imp_instrs ARTTree.compiler_type> block
%type <IMPTree.imp_instrs ARTTree.compiler_type> control
%type <IMPTree.imp_instrs ARTTree.compiler_type> for_header
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

| l=l_expr op=assign_binop e=expr
    {
      simplify_assign_binop l op e
    }

| l=l_expr op=assign_unop
    {
      simplify_assign_unop l op
    }

| t=LABEL LP e=l_expr RP
    {
      raise_syntax_error $startpos (Printf.sprintf "Unknown instruction '%s'." t)
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

| FOR LP init=for_header SEMI cond=expr SEMI it=for_header RP b=block
    {
      let syntax_tree = for_to_while init.syntax_tree cond it.syntax_tree b.syntax_tree in
      let tag_set = Tagset.(union init.tag_set (union it.tag_set b.tag_set)) in
      {syntax_tree; tag_set}
    }
;

for_header:
|   { 
      {syntax_tree = Cycle.empty_cycle; tag_set = Tagset.empty} 
    }
| s=for_header_list { s }
;

(* 
  Toutes les instructions sont acceptées dans les en-têtes de boucle for, pour faciliter le travail des languages de plus haut niveau. 
  La syntaxe pour les blocs de contrôle n'est pas intuitive, mais peu importe puisque de toute façon ce n'est pas conçu pour être 
  utilisé par le programmeur.
*)
for_header_list:
| i=instruction 
    { 
      {syntax_tree = Cycle.from_elt i; 
      tag_set = Tagset.empty} 
    }
| i=instruction COMMA s=for_header_list
    {
      {syntax_tree = Cycle.prepend s.syntax_tree i; tag_set = s.tag_set}
    }
| c=control { c }
| c=control COMMA s=for_header_list
    {
      let syntax_tree = Cycle.extend c.syntax_tree s.syntax_tree in 
      let tag_set = Tagset.union c.tag_set s.tag_set in
      {syntax_tree; tag_set}
    }
| t=LABEL
    { 
      {syntax_tree = Cycle.from_elt (TagDeclaration (make_node $startpos t));
      tag_set = Tagset.singleton t} 
    }
| t=LABEL COLON s=for_header_list
    {
      let syntax_tree = Cycle.prepend s.syntax_tree (TagDeclaration (make_node $startpos t)) in
      let tag_set = Tagset.add t s.tag_set in
      {syntax_tree; tag_set}
    }
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