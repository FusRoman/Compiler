%{
  open Lexing
  open Tagset
  open ARTTree
  open IMPTree
  open FUNTree
  open TYPTree

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

  let make_env = List.fold_left (
    fun (genv, type_env, tree) elt ->
      match elt with
      |Type (s, t) ->
        (genv, StringMap.add s.contents t type_env,tree)
      |Var (t, s, e) -> (StringMap.add s.contents t genv, type_env, (Var (t,s,e)) :: tree)
      |Fun f -> 
        let param_list = List.map (
          fun param ->
            param.params_type
        ) f.params in
        (StringMap.add f.name.contents (TFun (param_list, f.return_type)) genv, type_env, (Fun f)::tree)
  ) (StringMap.empty, StringMap.empty, [])

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
%}

%token VAR TINT TSTRING TFUN TYPE ARROW NEW
%token NOP PRINT EXIT
%token IF ELSE NO_ELSE
%token WHILE FOR 
%token CONTINUE BREAK RETURN
%token ASSIGN INCR DECR
%token ADDASSIGN SUBASSIGN
%token MULTASSIGN DIVASSIGN
%token EQ NEQ SEQ
%token LT LE GT GE
%token ADD SUB
%token MULT DIV REM
%token AND OR
%token NOT CPL
%token ADDRESS
%token SEMI COLON COMMA DOT PIPE
%token LP RP LB RB LS RS
%token <bool>BOOL
%token <int>INT
%token <string>LABEL
%token EOF

%start program
%type <TYPTree.typ_prog TYPTree.program> program
%type <TYPTree.variable> variable_declaration
%type <TYPTree.parameter> parameter
%type <TYPTree.typ_function> function_definition
%type <TYPTree.typ_instrs> instructions
%type <TYPTree.typ_expression ARTTree.node> expr
%type <TYPTree.typ_expression ARTTree.node> simple_expr
%type <TYPTree.typ_instr> instruction
%type <TYPTree.typ_instrs> block
%type <TYPTree.typ_instr> control
%type <TYPTree._type> type_expr

%nonassoc NO_ELSE
%nonassoc ELSE
%left AND OR
%left EQ NEQ LT LE GT GE SEQ
%left ADD SUB
%left MULT DIV REM
%left NOT CPL
%left DOT

%%

program:
| globals=list(global_declaration) EOF
    {
      let (genv, _type, tree) = make_env globals in
      {genv; _type; tree}
    }

| error
    { 
      raise_syntax_error $startpos "Syntax error" 
    }
;

global_declaration:
| f=function_definition
    {
      Fun f
    }

| var=global_variable_declaration SEMI
    {
      Var var
    }
| t=type_declaration SEMI
    { 
      Type t
    }
;

global_variable_declaration:
| VAR name=LABEL COLON typ=type_expr ASSIGN e=expr
  {
    (typ, make_node $startpos name, e)
  }
;

type_declaration:
| TYPE name_type=LABEL ASSIGN _type=type_expr
  {
    (make_node $startpos name_type, _type)
  }
;

variable_declaration:
| VAR name=LABEL COLON typ=type_expr ASSIGN e=expr
    {
      (typ, make_node $startpos name, e)
    }
;

parameter:
| name=LABEL COLON params_type=type_expr 
    {
      {name = make_node $startpos name; reference = false; params_type}
    }

| ADDRESS name=LABEL COLON params_type=type_expr
    {
      {name = make_node $startpos name; reference = true; params_type}
    }
;

function_definition:
| return_type=type_expr name=LABEL LP params=separated_list(COMMA, parameter) RP LB b=instructions RB
    {
      {name = make_node $startpos name; params; block = b; return_type}
    }
;


type_expr:
| TINT
  {
    TInt
  }
| TSTRING
  {
    (TPointer (TArray (TInt)))
  }
| TFUN LP ps=separated_list(COMMA, type_expr) ARROW ty=type_expr RP
  {
    (TPointer (TFun (ps, ty)))
  }
| l = LABEL
  {
    TAlias (make_node $startpos l)
  }
| t = type_expr LS RS
  {
    (TPointer (TArray (t)))
  }
| LP t=type_expr RP
  {
    t
  }
| LP fst=type_expr COMMA t=separated_nonempty_list(COMMA, type_expr) RP
  {
    (TPointer (TTuple (fst::t)))
  }
| t=type_expr MULT
  {
    TPointer t
  }
| LB fields=separated_nonempty_list(SEMI, field_declaration) RB
  {
    let (env, _) = List.fold_left (fun (s_map, decalage) (label, _type) ->
      if StringMap.mem label.contents s_map then
         raise (SyntaxError(
           Printf.sprintf "Field '%s' has been declared twice in the same record type" label.contents,
           label.line, label.column
         ))
      else
        ((StringMap.add label.contents (_type, decalage) s_map), decalage + 1)
    ) (StringMap.empty, 0) fields in
    TPointer (TRecord env)
  }
;

field_declaration:
| f=LABEL COLON t=type_expr
  {
    ((make_node $startpos f), t)
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
| e1=expr op=binop e2=expr
    { make_node $startpos (Binop(e1, op, e2)) }
| op=unop e=expr
    { make_node $startpos (Unop(op, e)) }
| f=simple_expr LP args=separated_list(COMMA, expr) RP
    { make_node $startpos ((Call((get_left $startpos $endpos f.contents), args)): TYPTree.typ_expression) }
| e=simple_expr
    { make_node $startpos e.contents }
| l=simple_expr LS e=expr RS
    { make_node $startpos (Deref (make_node $startpos (ArrayAccess(l, e))))  }
| l=expr DOT f=LABEL
    { make_node $startpos (Deref (make_node $startpos (RecordAccess(l, make_node $startpos(f) f)))) }
| l=expr DOT LP i=INT RP
    { make_node $startpos (Deref (make_node $startpos (TupleAccess (l, i)))) }
;

simple_expr:
| i=INT
    { make_node $startpos (Int i) }
| b=BOOL
    { make_node $startpos (Bool b) }
| t=LABEL
    { make_node $startpos (Deref( make_node $startpos (Id(make_node $startpos t)))) }
| LP e=expr RP
    { make_node $startpos e.contents }
| MULT l=simple_expr
    { make_node $startpos (Deref l) }
| ADDRESS l=simple_expr
    { make_node $startpos (get_left $startpos $endpos l.contents).contents }
| LB LT t_e=type_expr GT fields=separated_nonempty_list(SEMI, field_instanciation) RB
    { make_node $startpos (NewRecord (t_e,fields)) }
| LS size=expr PIPE init_elt=expr RS
    { make_node $startpos (NewArray (size,init_elt)) }
| LS l=separated_nonempty_list(SEMI, expr) RS
    { make_node $startpos (InitArray (l)) }
| LP fst=expr COMMA l=separated_nonempty_list(COMMA, expr) RP
    { make_node $startpos (NewTuple (fst::l)) }
;

%inline binop:
| ADD         { ARTBinop Add }
| SUB         { ARTBinop Sub }
| MULT        { ARTBinop Mult }
| DIV         { ARTBinop Div }
| REM         { ARTBinop Rem }
| AND         { ARTBinop And }
| OR          { ARTBinop Or }
| LT          { ARTBinop Lt }
| GT          { ARTBinop Gt }
| LE          { ARTBinop Le }
| GE          { ARTBinop Ge }
| EQ          { ARTBinop Eq }
| NEQ         { ARTBinop Neq }
| SEQ         { Seq }
;

%inline unop:
| SUB         { Minus }
| CPL         { Cpl }
| NOT         { Not }
;

field_instanciation:
| f=LABEL ASSIGN e=expr
    {
      (make_node $startpos f,e)
    }
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
      BinopAssign(get_left $startpos $endpos l.contents, op, e)
    }

| l=expr op=assign_unop
    {
      UnopAssign(get_left $startpos $endpos l.contents, op)
    }

| f=simple_expr LP args=separated_list(COMMA, expr) RP
    {
      Call(get_left $startpos $endpos f.contents, args)
    }

| v=variable_declaration
    { 
      Declaration v
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
      If(e, b)
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
| c=control { c }
;