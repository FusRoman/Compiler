%{
  open Lexing
  open ARTTree
  open Tagset
  open Cycle
%}


%token TEXT DATA
%token PRINT EXIT NOP STACK_POINTER
%token SEMI TWO_POINT
%token <int>INT
%token EOF
%token LP RP
%token <string>ID
%token <bool>BOOL
%token NOT
%token JUMP WHEN
%token AFFECT
%token LEFT_EXPR_STAR
%token ADD SUB MUL DIV MOD AND OR INF SUP INF_EQUAL SUP_EQUAL EQUAL NOT_EQUAL 

%left AND OR
%left SUP INF INF_EQUAL SUP_EQUAL EQUAL NOT_EQUAL
%left ADD SUB
%left MUL DIV MOD
%right NOT

%on_error_reduce instruction
%on_error_reduce data_declaration
%on_error_reduce tag
%on_error_reduce expression

%start source
%type <ARTTree.art_prog ARTTree.compiler_type> source
%type <ARTTree.art_instrs ARTTree.compiler_type> instructions
%type <ARTTree.art_instr> instruction
%type <ARTTree.l_expr> l_express
%type <ARTTree.expression> expression
%type <ARTTree.datas ARTTree.compiler_type> data_declarations
%type <ARTTree.data ARTTree.compiler_type> data_declaration
%type <ARTTree.art_instr ARTTree.compiler_type> tag
  
%%

source:
| TEXT text=instructions EOF
    {
      {tag_set = text.tag_set;
      syntax_tree = Prog text.syntax_tree}
    }
| TEXT text=instructions DATA data=data_declarations EOF
    {
      try
        let tag_set = Tagset.union text.tag_set data.tag_set in
        {tag_set = tag_set;
        syntax_tree = ProgData (text.syntax_tree, data.syntax_tree)}
      with
      |Tagset.DuplicateElement s ->
        let pos = $startpos in
        raise (ARTTree.SyntaxError ("This tag "^s^" has been already declared before.", pos.pos_lnum, (pos.pos_cnum - pos.pos_bol)))
    }
| error {
   let pos = $startpos in
   raise (SyntaxError ("An ART program must begin with a .text tag follow by instructions and eventually a .data tag with data declarations", pos.pos_lnum, (pos.pos_cnum - pos.pos_bol)))
}
;

instructions:
| (* empty *) { {tag_set = Tagset.empty; syntax_tree = Cycle.empty_cycle} }
| i=instruction SEMI is=instructions 
{ 
  {tag_set = is.tag_set;
  syntax_tree =  (Cycle.append is.syntax_tree i)} 
}
| t=tag TWO_POINT is=instructions 
  {
    try
    {tag_set = Tagset.union t.tag_set is.tag_set;
    syntax_tree = (Cycle.append is.syntax_tree t.syntax_tree)}
    with
    |Tagset.DuplicateElement s ->
      let pos = $startpos in
      raise (ARTTree.SyntaxError ("This tag "^s^" has been already declared before.", pos.pos_lnum, (pos.pos_cnum - pos.pos_bol)))
  }
| instruction TWO_POINT instructions {
  let pos = $startpos in
   raise (SyntaxError ("You cannot used a ':' symbol between two instructions.", pos.pos_lnum, (pos.pos_cnum - pos.pos_bol)))
}
| tag SEMI instructions {
   let pos = $startpos in
   raise (SyntaxError ("tag declaration ill formed, may you have forgot a ':'.", pos.pos_lnum, (pos.pos_cnum - pos.pos_bol)))
}
| error {
   let pos = $startpos in
   raise (SyntaxError ("May be, you have forgot a ';' between two instructions or a ':' for a tag declaration.", pos.pos_lnum, (pos.pos_cnum - pos.pos_bol)))
}
;
  
tag:
|i=ID {
    let pos = $startpos in
    let tag = {
      line = pos.pos_lnum;
      column = (pos.pos_cnum - pos.pos_bol);
      contents = i
    } in
    {tag_set = Tagset.singleton i; 
    syntax_tree = TagDeclaration tag
    }
  }
|STACK_POINTER {
    let pos = $startpos in
    raise (SyntaxError ("stack_pointer is a reserved tag", pos.pos_lnum, (pos.pos_cnum - pos.pos_bol)))
 }
;

instruction:
| PRINT LP e=expression RP { Print e }
| NOP { Nop }
| EXIT { Exit }
| JUMP l_e=l_express { Jump l_e }
| JUMP l_e=l_express WHEN e=expression { JumpWhen (l_e,e) }
| l_e=l_express AFFECT e=expression { Assign (l_e,e) }
| error {
  let pos = $startpos in
  raise (SyntaxError ("Only expression is allowed inside an instruction.", pos.pos_lnum, (pos.pos_cnum - pos.pos_bol)))
}
;

l_express:
| i=ID {
    let pos = $startpos in
    Id {
      line = pos.pos_lnum;
      column = (pos.pos_cnum - pos.pos_bol);
      contents = i
    }
  }
| LEFT_EXPR_STAR e=l_express { e }
;

expression:
| i=INT  { Int i }
| STACK_POINTER { StackPointer }
| l_e=l_express { LExpr l_e }
| b=BOOL { Bool b }
| SUB e=expression { Unop (Minus,e) }
| NOT e=expression { Unop (Not,e) }
| LP e=expression RP { e }
| e1=expression ADD e2=expression { 
  Binop (e1,Add,e2) 
  }
| e1=expression SUB e2=expression { 
  Binop (e1,Sub,e2)
   }
| e1=expression DIV e2=expression { 
  Binop (e1,Div,e2)
}
| e1=expression MUL e2=expression { 
  Binop (e1,Mult,e2)
   }
| e1=expression INF e2=expression { 
  Binop (e1,Lt,e2)
   }
| e1=expression INF_EQUAL e2=expression { 
  Binop (e1,Le,e2)
   }
| e1=expression SUP e2=expression { 
  Binop (e1,Gt,e2)
   }
| e1=expression SUP_EQUAL e2=expression {
  Binop (e1,Ge,e2)
   }
| e1=expression MOD e2=expression { 
  Binop (e1,Rem,e2)
   }
| e1=expression EQUAL e2=expression { 
  Binop (e1,Eq,e2)
   }
| e1=expression NOT_EQUAL e2=expression { 
  Binop (e1,Neq,e2)
   }
| e1=expression AND e2=expression { 
  Binop (e1,And,e2)
   }
| e1=expression OR e2=expression { 
  Binop (e1,Or,e2)
   }
| error {
  let pos = $startpos in
  raise (SyntaxError ("ill formed expression.", pos.pos_lnum, (pos.pos_cnum - pos.pos_bol)))
}
;

data_declarations:
| d=data_declaration ds=data_declarations {
  try
  {tag_set = (Tagset.union d.tag_set ds.tag_set);
  syntax_tree = (Cycle.append ds.syntax_tree d.syntax_tree)}
  with
    |Tagset.DuplicateElement s ->
      let pos = $startpos in
      raise (ARTTree.SyntaxError ("This tag "^s^" has been already declared before.", pos.pos_lnum, (pos.pos_cnum - pos.pos_bol)))
  }
| { {tag_set = Tagset.empty; syntax_tree = Cycle.empty_cycle} }
;

data_declaration:
| t=ID TWO_POINT i=INT {
  let pos = $startpos in
  let data = {
    line = pos.pos_lnum;
    column = (pos.pos_cnum - pos.pos_bol);
    contents = (t,i)
  } in
    {
      tag_set = Tagset.singleton t; syntax_tree = data
    }
  }
|STACK_POINTER {
   let pos = $startpos in
   raise (SyntaxError ("stack_pointer is a reserved tag", pos.pos_lnum, (pos.pos_cnum - pos.pos_bol)))
  }
| error {
  let pos = $startpos in
   raise (SyntaxError ("ill formed tag declaration", pos.pos_lnum, (pos.pos_cnum - pos.pos_bol)))
}

;

%%
