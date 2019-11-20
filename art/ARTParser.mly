%{
  open Lexing
  open ARTTree
  open Tagset
  open Cycle
%}


%token TEXT DATA
%token PRINT EXIT NOP
%token SEMI TWO_POINT
%token <int>INT
%token EOF
%token LP RP
%token <string>ID
%token <bool>BOOL
%token NOT CPL ADRESS
%token JUMP WHEN
%token AFFECT
%token ADD SUB MUL DIV MOD AND OR INF SUP INF_EQUAL SUP_EQUAL EQUAL NOT_EQUAL 

%left AND OR
%left SUP INF INF_EQUAL SUP_EQUAL EQUAL NOT_EQUAL
%left ADD SUB
%left MUL DIV MOD
%right NOT CPL

/*
 La declaration %on_error_reduce est utilisé pour la gestion des messages d'erreur du parser. Les règles sous l'influence
 d'un %on_error_reduce vont être obligées de continuer à lire des tokens et à effectuer des réductions plus loin que 
 l'erreur relevée ce qui permet au parser de connaitre de façon un peu plus précise l'origine de l'erreur relevé.
*/
%on_error_reduce instruction
%on_error_reduce data_declaration
%on_error_reduce tag
%on_error_reduce expression

%start source
%type <ARTTree.art_prog ARTTree.compiler_type> source
%type <ARTTree.art_instrs ARTTree.compiler_type> instructions
%type <ARTTree.art_instr> instruction
%type <ARTTree.expression> l_express
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
      (* L'exception DuplicateElements est levé lorsque on essaie d'ajouter un élément qui existe déjà 
      dans l'ensemble. Comme union effectue des ajout, cette exception peut être levé et permet de 
      savoir si un tag du même nom a déjà été déclaré avant et de lever une SyntaxError. *)
      |Tagset.DuplicateElement s ->
        let pos = $startpos in
        raise (ARTTree.SyntaxError ("Tag '"^s^"' is declared at least twice.", pos.pos_lnum, (pos.pos_cnum - pos.pos_bol)))
    }
| error {
   let pos = $startpos in
   raise (SyntaxError ("ART program structure: .text <instructions> .data <declarations>" , pos.pos_lnum, (pos.pos_cnum - pos.pos_bol)))
}
;

instructions:
| (* empty *) { {tag_set = Tagset.empty; syntax_tree = Cycle.empty_cycle} }
| i=instruction SEMI is=instructions 
{ 
  {tag_set = is.tag_set;
  syntax_tree =  (Cycle.prepend is.syntax_tree i)} 
}
| t=tag TWO_POINT is=instructions 
  {
    try
    {tag_set = Tagset.union t.tag_set is.tag_set;
    syntax_tree = (Cycle.prepend is.syntax_tree t.syntax_tree)}
    with
    |Tagset.DuplicateElement s ->
      let pos = $startpos in
      raise (ARTTree.SyntaxError ("Tag '"^s^"' is declared at least twice.", pos.pos_lnum, (pos.pos_cnum - pos.pos_bol)))
  }
  (* Les règles suivantes interviennent dans la génération de messages d'erreur plus précis et ne détermine pas
  les véritables règles de syntaxe du langage art. *)
| instruction TWO_POINT instructions {
  let pos = $startpos in
   raise (SyntaxError ("Expected ':', found ';'.", pos.pos_lnum, (pos.pos_cnum - pos.pos_bol)))
}
| tag SEMI instructions {
   let pos = $startpos in
   raise (SyntaxError ("You may have forgotten a ':'.", pos.pos_lnum, (pos.pos_cnum - pos.pos_bol)))
}
| error {
   let pos = $startpos in
   raise (SyntaxError ("You may have forgotten a ';' between two instructions or a ':' for a tag declaration.", pos.pos_lnum, (pos.pos_cnum - pos.pos_bol)))
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
  raise (SyntaxError ("Ill-formed instruction.", pos.pos_lnum, (pos.pos_cnum - pos.pos_bol)))
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
| MUL e=expression { e }
;

expression:
| i=INT                 { Int i }
| l_e=l_express         { LStar l_e }
| b=BOOL                { Bool b }
| op=unop e=expression  { Unop (op, e) }
| ADRESS e=l_express    { e }
| LP e=expression RP    { e }
| e1=expression op=binop e2=expression { Binop(e1, op, e2) }
| error {
  let pos = $startpos in
  raise (SyntaxError ("Ill-formed expression.", pos.pos_lnum, (pos.pos_cnum - pos.pos_bol)))
}
;

%inline binop:
| ADD         { Add }
| SUB         { Sub }
| MUL         { Mult }
| DIV         { Div }
| MOD         { Rem }
| AND         { And }
| OR          { Or }
| INF         { Lt }
| SUP         { Gt }
| INF_EQUAL   { Le }
| SUP_EQUAL   { Ge }
| EQUAL       { Eq }
| NOT_EQUAL   { Neq }
;

%inline unop:
| SUB         { Minus }
| CPL         { Cpl }
| NOT         { Not }
;

data_declarations:
| d=data_declaration ds=data_declarations {
  try
  {tag_set = (Tagset.union d.tag_set ds.tag_set);
  syntax_tree = (Cycle.prepend ds.syntax_tree d.syntax_tree)}
  with
    |Tagset.DuplicateElement s ->
      let pos = $startpos in
      raise (ARTTree.SyntaxError ("Tag '"^s^"' is declared at least twice.", pos.pos_lnum, (pos.pos_cnum - pos.pos_bol)))
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
    { tag_set = Tagset.singleton t; syntax_tree = data }
  }

| t=ID TWO_POINT b=BOOL {
    let pos = $startpos in
    let data = {
      line = pos.pos_lnum;
      column = (pos.pos_cnum - pos.pos_bol);
      contents = (t, Arith.int_of_bool b)
    } in
    { tag_set = Tagset.singleton t; syntax_tree = data }
}

| error {
  let pos = $startpos in
   raise (SyntaxError ("Ill-formed tag declaration", pos.pos_lnum, (pos.pos_cnum - pos.pos_bol)))
}
;

%%
