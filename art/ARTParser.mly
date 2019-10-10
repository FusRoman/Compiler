%{
  open Lexing
  open ART_SyntaxTree
  open Tagset
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
%type <ART_SyntaxTree.prog ART_SyntaxTree.compiler_type> source
%type <ART_SyntaxTree.instrs ART_SyntaxTree.compiler_type> instructions
%type <ART_SyntaxTree.instr> instruction
%type <ART_SyntaxTree.l_expr> l_express
%type <ART_SyntaxTree.expression> expression
%type <ART_SyntaxTree.datas ART_SyntaxTree.compiler_type> data_declarations
%type <ART_SyntaxTree.data ART_SyntaxTree.compiler_type> data_declaration
%type <ART_SyntaxTree.tag ART_SyntaxTree.compiler_type> tag
  
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
        syntax_tree = Prog_Data (text.syntax_tree, data.syntax_tree)}
      with
      |Tagset.DuplicateElement s ->
        let pos = $startpos in
        raise (ART_SyntaxTree.SyntaxError ("This tag "^s^" has been already declared before.", pos.pos_lnum, (pos.pos_cnum - pos.pos_bol)))
    }
| error {
   let pos = $startpos in
   raise (SyntaxError ("An ART program must begin with a .text tag follow by instructions and eventually a .data tag with data declarations", pos.pos_lnum, (pos.pos_cnum - pos.pos_bol)))
}
;

instructions:
| (* empty *) { {tag_set = Tagset.empty; syntax_tree = Empty_instr} }
| i=instruction SEMI is=instructions 
{ 
  {tag_set = is.tag_set;
  syntax_tree = Instrs (i,is.syntax_tree)} 
}
| t=tag TWO_POINT is=instructions 
  {
    try
    {tag_set = Tagset.union t.tag_set is.tag_set;
    syntax_tree = Instrs_with_tag (t.syntax_tree,is.syntax_tree)}
    with
    |Tagset.DuplicateElement s ->
      let pos = $startpos in
      raise (ART_SyntaxTree.SyntaxError ("This tag "^s^" has been already declared before.", pos.pos_lnum, (pos.pos_cnum - pos.pos_bol)))
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
    {tag_set = Tagset.singleton i; 
    syntax_tree = Tag (i,pos.pos_lnum, (pos.pos_cnum - pos.pos_bol))
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
| l_e=l_express AFFECT e=expression { Affect (l_e,e) }
| error {
  let pos = $startpos in
  raise (SyntaxError ("Only expression is allowed inside an instruction.", pos.pos_lnum, (pos.pos_cnum - pos.pos_bol)))
}
;

l_express:
| i=ID {
  let pos = $startpos in
  Id (i, pos.pos_lnum, (pos.pos_cnum - pos.pos_bol))
  }
| LEFT_EXPR_STAR e=l_express { e }
;

expression:
| i=INT  { Int i }
| STACK_POINTER { STACK_POINTER }
| l_e=l_express { L_expr l_e }
| b=BOOL { Bool b }
| SUB e=expression { Unop (MINUS,e) }
| NOT e=expression { Unop (NOT,e) }
| LP e=expression RP { Expr_parenthese e }
| e1=expression ADD e2=expression { 
  Binop (e1,ADD,e2) 
  }
| e1=expression SUB e2=expression { 
  Binop (e1,SUB,e2)
   }
| e1=expression DIV e2=expression { 
  Binop (e1,DIV,e2)
}
| e1=expression MUL e2=expression { 
  Binop (e1,MULT,e2)
   }
| e1=expression INF e2=expression { 
  Binop (e1,INF,e2)
   }
| e1=expression INF_EQUAL e2=expression { 
  Binop (e1,INF_EQUAL,e2)
   }
| e1=expression SUP e2=expression { 
  Binop (e1,SUP,e2)
   }
| e1=expression SUP_EQUAL e2=expression {
  Binop (e1,SUP_EQUAL,e2)
   }
| e1=expression MOD e2=expression { 
  Binop (e1,REM,e2)
   }
| e1=expression EQUAL e2=expression { 
  Binop (e1,EQUAL,e2)
   }
| e1=expression NOT_EQUAL e2=expression { 
  Binop (e1,NOT_EQUAL,e2)
   }
| e1=expression AND e2=expression { 
  Binop (e1,AND,e2)
   }
| e1=expression OR e2=expression { 
  Binop (e1,OR,e2)
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
  syntax_tree = Datas (d.syntax_tree,ds.syntax_tree)}
  with
    |Tagset.DuplicateElement s ->
      let pos = $startpos in
      raise (ART_SyntaxTree.SyntaxError ("This tag "^s^" has been already declared before.", pos.pos_lnum, (pos.pos_cnum - pos.pos_bol)))
  }
| { {tag_set = Tagset.empty; syntax_tree = Empty_data} }
;

data_declaration:
| t=ID TWO_POINT i=INT {
  let pos = $startpos in
    {
      tag_set = Tagset.singleton t; syntax_tree = Data (t,i,pos.pos_lnum, (pos.pos_cnum - pos.pos_bol))
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
