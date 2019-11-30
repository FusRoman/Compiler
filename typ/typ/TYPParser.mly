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
        (genv, type_env.add s t,tree)
      |Var (t, s, e) -> (genv.add s t, type_env, (Var (t,s,e)) :: tree)
      |Fun f -> 
        let param_list = List.map (
          fun param ->
            param.params_type
        ) f.params in
        (genv.add f.name.contents (Fun (param_list, f.return_type)), type_env, (Fun f)::tree)
  ) (StringMap.empty, StringMap.empty, [])

%}

%token VAR TINT TBOOL TCHAR TSTRING TFUN DOT TYPE ARROW
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
%type <TYPTree.typ_expression> expr
%type <TYPTree.typ_expression> l_expr
%type <TYPTree.typ_instr> instruction
%type <TYPTree.typ_instrs> block
%type <TYPTree.typ_instr> control
%type <TYPTree._type> type_expr

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
      let (genv, _type, tree) = make_env globals in
      {genv, _type, tree}
    }

| error
    { 
      raise_syntax_error $startpos "Syntax error" 
    }
;

global_declaration:
| def_fun=function_definition
    {
      Fun f
    }

| var=variable_declaration SEMI
    {
      Var v
    }
| t=list(type_declaration) SEMI
    { 
      Type t
    }
;

type_declaration:
| TYPE name_type=LABEL ASSIGN _type=type_expr
  {
    (make_node $startpos name_type, _type)
  }
;

variable_declaration:
| type_variable=type_expr name=LABEL ASSIGN e=expr
    {
      (type_variable,make_node $startpos name, e)
    }
;

parameter:
| params_type=type_expr name=LABEL
    {
      {name = make_node $startpos name; reference = false; params_type}
    }

| params_type=type_expr ADDRESS name=LABEL
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
|TINT
  {
    TInt
  }
|TSTRING
  {
    TArray (TChar)
  }
|TFUN LP ps=separated_list(COMMA, type_expr) ARROW ty=type_expr RP
  {
    TFun (ps, ty)
  }
|l = LABEL
  {
    TAlias (make_node $startpos l)
  }
|t = type_expr LS RS
  {
    TArray (t)
  }
| t=type_expr MULT
  {
    TPointer t
  }
| LB fields=separated_nonempty_list(SEMI, field_declaration) RB
| LB fields=separated_nonempty_list(SEMI, field_declaration) SEMI RB
  {
    let (env, _) = List.fold_left (fun (s_map, decalage) (label, _type) ->
      if StringMap.mem label.contents s_map then
         raise (SyntaxError(
           Printf.sprintf "Field '%s' has been declared twice in the same record type" s.contents,
           s.line, s.column
         ))
      else
        ((StringMap.add label.contents (_type, decalage) s_map), decalage + 1)
    ) (StringMap.empty, 0) fields in
    TRecord env
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

l_expr:
| t=LABEL
    { Id (make_node $startpos t) }
| MULT LP l=expr RP (* La grammaire a changé par rapport à FUN pour éviter les ambiguïtés ! *)
    { l }
| l_expr=l_expr DOT l=LABEL
    {
      RecordAccess(l_expr, l)
    }
| l_expr=l_expr LS e=expr RS
    {
      ArrayAccess (l_expr, e)
    }
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
| t_e=type_expr LB fields=separated_nonempty_list(SEMI, field_instanciation) RB
| t_e=type_expr LB fields=separated_nonempty_list(SEMI, field_instanciation) SEMI RB
    {
      NewRecord (t_e,fields)
    }
| LP size=expr RP LS init_elt=expr RS
    {
      NewArray (size,init_elt)
    }
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

| l=l_expr op=assign_binop e=expr
    {
      BinopAssign(l, op, e)
    }

| l=l_expr op=assign_unop
    {
      UnopAssign(l, op)
    }

| f=l_expr LP args=separated_list(COMMA, expr) RP
    {
      Call(f, args)
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

| FOR LP init=separated_list(COMMA, instruction) SEMI cond=expr SEMI it=separated_list(COMMA, instruction) RP b=block
    {
      For(init, cond, it, b)
    }
;