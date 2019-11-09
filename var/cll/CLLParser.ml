
module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | WHILE
    | TEXT
    | SUBASSIGN
    | SUB
    | STACKPOINTER
    | SEMI
    | RP
    | RETURN
    | REM
    | RB
    | PRINT
    | OR
    | NO_ELSE
    | NOT
    | NOP
    | NEQ
    | MULTASSIGN
    | MULT
    | LT
    | LP
    | LE
    | LB
    | LABEL of (
# 70 "var/cll/CLLParser.mly"
       (string)
# 33 "var/cll/CLLParser.ml"
  )
    | INT of (
# 69 "var/cll/CLLParser.mly"
       (int)
# 38 "var/cll/CLLParser.ml"
  )
    | INCR
    | IF
    | GT
    | GE
    | FOR
    | EXIT
    | EQ
    | EOF
    | ELSE
    | DIVASSIGN
    | DIV
    | DECR
    | DATA
    | CPL
    | CONTINUE
    | COMMA
    | COLON
    | BREAK
    | BOOL of (
# 68 "var/cll/CLLParser.mly"
       (bool)
# 61 "var/cll/CLLParser.ml"
  )
    | ASSIGN
    | AND
    | ADDRESS
    | ADDASSIGN
    | ADD
  
end

include MenhirBasics

let _eRR =
  MenhirBasics.Error

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state = 
  | MenhirState147
  | MenhirState144
  | MenhirState140
  | MenhirState131
  | MenhirState128
  | MenhirState126
  | MenhirState125
  | MenhirState123
  | MenhirState119
  | MenhirState116
  | MenhirState114
  | MenhirState111
  | MenhirState109
  | MenhirState107
  | MenhirState105
  | MenhirState102
  | MenhirState100
  | MenhirState87
  | MenhirState84
  | MenhirState81
  | MenhirState71
  | MenhirState67
  | MenhirState66
  | MenhirState65
  | MenhirState63
  | MenhirState61
  | MenhirState60
  | MenhirState59
  | MenhirState58
  | MenhirState56
  | MenhirState49
  | MenhirState47
  | MenhirState45
  | MenhirState43
  | MenhirState41
  | MenhirState39
  | MenhirState37
  | MenhirState35
  | MenhirState33
  | MenhirState30
  | MenhirState28
  | MenhirState26
  | MenhirState24
  | MenhirState17
  | MenhirState15
  | MenhirState12
  | MenhirState11
  | MenhirState9
  | MenhirState8
  | MenhirState5
  | MenhirState4
  | MenhirState0

# 1 "var/cll/CLLParser.mly"
  
  open Lexing
  open Tagset
  open ARTTree
  open CLLTree

  type assign_binop =
    | Standard
    | AddAssign
    | SubAssign
    | MultAssign 
    | DivAssign

  type assign_unop =
    | Incr 
    | Decr 

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

  let make_compiler_type (tag_set, proc_decl_cycle) (name_node, block) =
    (*
    
    Cette fonction est utilisé dans les fold_left des régles 'start' de la grammaire. Elle permet
    de faire la transformation de la liste renvoyé par la règle ' program -> list(procedure_declaration)' 
    en un couple (tag_set, CLL.instrs) utilisé pour construire cll_prog compiler_type
    
    En c, une variable globale n'a pas le droit d'avoir le même nom qu'une procédure mais 
    une variable local peut avoir le même nom qu'une procédure, y compris si cette variable local 
    à la même nom que sa procédure. Y penser quand on fera Var.*)
    let proc_decl = {name = name_node; block = block.syntax_tree} in
    ((union (add name_node.contents tag_set) block.tag_set), Cycle.prepend proc_decl_cycle proc_decl)

# 186 "var/cll/CLLParser.ml"

let rec _menhir_goto_assign_unop : _menhir_env -> 'ttv_tail -> 'tv_assign_unop -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv539 * _menhir_state * (
# 77 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 194 "var/cll/CLLParser.ml"
    ) * Lexing.position) = Obj.magic _menhir_stack in
    let (_v : 'tv_assign_unop) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv537 * _menhir_state * (
# 77 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 201 "var/cll/CLLParser.ml"
    ) * Lexing.position) = Obj.magic _menhir_stack in
    let ((op : 'tv_assign_unop) : 'tv_assign_unop) = _v in
    ((let (_menhir_stack, _menhir_s, (l : (
# 77 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 207 "var/cll/CLLParser.ml"
    )), _startpos_l_) = _menhir_stack in
    let _startpos = _startpos_l_ in
    let _v : (
# 79 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 213 "var/cll/CLLParser.ml"
    ) = 
# 283 "var/cll/CLLParser.mly"
    (
      match op with
      | Incr -> Assign(l, Binop(l, Add, Int 1))
      | Decr -> Assign(l, Binop(l, Sub, Int 1))
    )
# 221 "var/cll/CLLParser.ml"
     in
    _menhir_goto_assign _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv538)) : 'freshtv540)

and _menhir_goto_assign_binop : _menhir_env -> 'ttv_tail -> 'tv_assign_binop -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv535 * _menhir_state * (
# 77 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 232 "var/cll/CLLParser.ml"
    ) * Lexing.position) * 'tv_assign_binop) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BOOL _v ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
    | CPL ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | INT _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
    | LABEL _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | MULT ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | STACKPOINTER ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | SUB ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState81) : 'freshtv536)

and _menhir_goto_assigns : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 82 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 265 "var/cll/CLLParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState84 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv509 * _menhir_state * Lexing.position)) * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 275 "var/cll/CLLParser.ml"
        )) * Lexing.position) * _menhir_state * (
# 82 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 279 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RP ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv505 * _menhir_state * Lexing.position)) * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 289 "var/cll/CLLParser.ml"
            )) * Lexing.position) * _menhir_state * (
# 82 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 293 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BREAK ->
                _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | CONTINUE ->
                _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EXIT ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOR ->
                _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IF ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LABEL _v ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | MULT ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NOP ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | PRINT ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RETURN ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | WHILE ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState87) : 'freshtv506)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv507 * _menhir_state * Lexing.position)) * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 333 "var/cll/CLLParser.ml"
            )) * Lexing.position) * _menhir_state * (
# 82 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 337 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv508)) : 'freshtv510)
    | MenhirState100 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv513 * _menhir_state * (
# 79 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 346 "var/cll/CLLParser.ml"
        ) * Lexing.position)) * _menhir_state * (
# 82 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 350 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv511 * _menhir_state * (
# 79 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 356 "var/cll/CLLParser.ml"
        ) * Lexing.position)) * _menhir_state * (
# 82 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 360 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (a : (
# 79 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 365 "var/cll/CLLParser.ml"
        )), _startpos_a_), _, (s : (
# 82 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 369 "var/cll/CLLParser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : (
# 82 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 375 "var/cll/CLLParser.ml"
        ) = 
# 373 "var/cll/CLLParser.mly"
                           ( Cycle.prepend s a )
# 379 "var/cll/CLLParser.ml"
         in
        _menhir_goto_assigns _menhir_env _menhir_stack _menhir_s _v) : 'freshtv512)) : 'freshtv514)
    | MenhirState71 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv521 * _menhir_state * Lexing.position)) * _menhir_state * (
# 82 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 387 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RP ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv515 * _menhir_state * Lexing.position)) * _menhir_state * (
# 82 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 397 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BREAK ->
                _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | CONTINUE ->
                _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EXIT ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOR ->
                _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IF ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LABEL _v ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState116
            | MULT ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NOP ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | PRINT ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RETURN ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | WHILE ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState116) : 'freshtv516)
        | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv517 * _menhir_state * Lexing.position)) * _menhir_state * (
# 82 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 435 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_stack = (_menhir_stack, _endpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ADDRESS ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BOOL _v ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _v
            | CPL ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState105
            | INT _v ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _v
            | LABEL _v ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LP ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState105
            | MULT ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NOT ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState105
            | STACKPOINTER ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState105
            | SUB ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState105
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState105) : 'freshtv518)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv519 * _menhir_state * Lexing.position)) * _menhir_state * (
# 82 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 473 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv520)) : 'freshtv522)
    | MenhirState107 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv527 * _menhir_state * Lexing.position)) * _menhir_state * (
# 82 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 482 "var/cll/CLLParser.ml"
        )) * Lexing.position) * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 486 "var/cll/CLLParser.ml"
        )) * Lexing.position) * _menhir_state * (
# 82 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 490 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RP ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv523 * _menhir_state * Lexing.position)) * _menhir_state * (
# 82 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 500 "var/cll/CLLParser.ml"
            )) * Lexing.position) * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 504 "var/cll/CLLParser.ml"
            )) * Lexing.position) * _menhir_state * (
# 82 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 508 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BREAK ->
                _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | CONTINUE ->
                _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EXIT ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOR ->
                _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IF ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LABEL _v ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState109
            | MULT ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NOP ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | PRINT ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RETURN ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | WHILE ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState109) : 'freshtv524)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv525 * _menhir_state * Lexing.position)) * _menhir_state * (
# 82 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 548 "var/cll/CLLParser.ml"
            )) * Lexing.position) * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 552 "var/cll/CLLParser.ml"
            )) * Lexing.position) * _menhir_state * (
# 82 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 556 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv526)) : 'freshtv528)
    | MenhirState105 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv533 * _menhir_state * Lexing.position)) * _menhir_state * (
# 82 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 565 "var/cll/CLLParser.ml"
        )) * Lexing.position) * _menhir_state * (
# 82 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 569 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RP ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv529 * _menhir_state * Lexing.position)) * _menhir_state * (
# 82 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 579 "var/cll/CLLParser.ml"
            )) * Lexing.position) * _menhir_state * (
# 82 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 583 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BREAK ->
                _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | CONTINUE ->
                _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EXIT ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOR ->
                _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IF ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LABEL _v ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | MULT ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NOP ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | PRINT ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RETURN ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | WHILE ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState114) : 'freshtv530)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv531 * _menhir_state * Lexing.position)) * _menhir_state * (
# 82 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 623 "var/cll/CLLParser.ml"
            )) * Lexing.position) * _menhir_state * (
# 82 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 627 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv532)) : 'freshtv534)
    | _ ->
        _menhir_fail ()

and _menhir_run73 : _menhir_env -> 'ttv_tail -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv503) = Obj.magic _menhir_stack in
    ((let _1 = () in
    let _v : 'tv_assign_binop = 
# 298 "var/cll/CLLParser.mly"
              ( SubAssign )
# 643 "var/cll/CLLParser.ml"
     in
    _menhir_goto_assign_binop _menhir_env _menhir_stack _v) : 'freshtv504)

and _menhir_run74 : _menhir_env -> 'ttv_tail -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv501) = Obj.magic _menhir_stack in
    ((let _1 = () in
    let _v : 'tv_assign_binop = 
# 299 "var/cll/CLLParser.mly"
              ( MultAssign )
# 656 "var/cll/CLLParser.ml"
     in
    _menhir_goto_assign_binop _menhir_env _menhir_stack _v) : 'freshtv502)

and _menhir_run75 : _menhir_env -> 'ttv_tail -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv499) = Obj.magic _menhir_stack in
    ((let _1 = () in
    let _v : 'tv_assign_unop = 
# 304 "var/cll/CLLParser.mly"
              ( Incr )
# 669 "var/cll/CLLParser.ml"
     in
    _menhir_goto_assign_unop _menhir_env _menhir_stack _v) : 'freshtv500)

and _menhir_run76 : _menhir_env -> 'ttv_tail -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv497) = Obj.magic _menhir_stack in
    ((let _1 = () in
    let _v : 'tv_assign_binop = 
# 300 "var/cll/CLLParser.mly"
              ( DivAssign )
# 682 "var/cll/CLLParser.ml"
     in
    _menhir_goto_assign_binop _menhir_env _menhir_stack _v) : 'freshtv498)

and _menhir_run77 : _menhir_env -> 'ttv_tail -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv495) = Obj.magic _menhir_stack in
    ((let _1 = () in
    let _v : 'tv_assign_unop = 
# 305 "var/cll/CLLParser.mly"
              ( Decr )
# 695 "var/cll/CLLParser.ml"
     in
    _menhir_goto_assign_unop _menhir_env _menhir_stack _v) : 'freshtv496)

and _menhir_run78 : _menhir_env -> 'ttv_tail -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv493) = Obj.magic _menhir_stack in
    ((let _1 = () in
    let _v : 'tv_assign_binop = 
# 296 "var/cll/CLLParser.mly"
              ( Standard )
# 708 "var/cll/CLLParser.ml"
     in
    _menhir_goto_assign_binop _menhir_env _menhir_stack _v) : 'freshtv494)

and _menhir_run79 : _menhir_env -> 'ttv_tail -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv491) = Obj.magic _menhir_stack in
    ((let _1 = () in
    let _v : 'tv_assign_binop = 
# 297 "var/cll/CLLParser.mly"
              ( AddAssign )
# 721 "var/cll/CLLParser.ml"
     in
    _menhir_goto_assign_binop _menhir_env _menhir_stack _v) : 'freshtv492)

and _menhir_reduce29 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 77 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 728 "var/cll/CLLParser.ml"
) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (l : (
# 77 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 734 "var/cll/CLLParser.ml"
    )), _startpos_l_) = _menhir_stack in
    let _v : (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 739 "var/cll/CLLParser.ml"
    ) = 
# 182 "var/cll/CLLParser.mly"
    ( (* pas de déréférencement ! ARTParser le fait déjà *) l )
# 743 "var/cll/CLLParser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_assign : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 79 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 750 "var/cll/CLLParser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState4 | MenhirState5 | MenhirState58 | MenhirState59 | MenhirState60 | MenhirState61 | MenhirState131 | MenhirState123 | MenhirState125 | MenhirState128 | MenhirState126 | MenhirState65 | MenhirState119 | MenhirState116 | MenhirState114 | MenhirState111 | MenhirState109 | MenhirState102 | MenhirState87 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv481 * _menhir_state * (
# 79 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 760 "var/cll/CLLParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv479 * _menhir_state * (
# 79 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 766 "var/cll/CLLParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (a : (
# 79 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 771 "var/cll/CLLParser.ml"
        )), _startpos_a_) = _menhir_stack in
        let _startpos = _startpos_a_ in
        let _v : (
# 78 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 777 "var/cll/CLLParser.ml"
        ) = 
# 258 "var/cll/CLLParser.mly"
    ( a )
# 781 "var/cll/CLLParser.ml"
         in
        _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv480)) : 'freshtv482)
    | MenhirState71 | MenhirState105 | MenhirState107 | MenhirState100 | MenhirState84 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv489 * _menhir_state * (
# 79 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 789 "var/cll/CLLParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COMMA ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv483 * _menhir_state * (
# 79 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 799 "var/cll/CLLParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LABEL _v ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MULT ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState100) : 'freshtv484)
        | RP | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv485 * _menhir_state * (
# 79 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 817 "var/cll/CLLParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (a : (
# 79 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 822 "var/cll/CLLParser.ml"
            )), _startpos_a_) = _menhir_stack in
            let _v : (
# 82 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 827 "var/cll/CLLParser.ml"
            ) = 
# 372 "var/cll/CLLParser.mly"
            ( Cycle.from_elt a )
# 831 "var/cll/CLLParser.ml"
             in
            _menhir_goto_assigns _menhir_env _menhir_stack _menhir_s _v) : 'freshtv486)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv487 * _menhir_state * (
# 79 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 841 "var/cll/CLLParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv488)) : 'freshtv490)
    | _ ->
        _menhir_fail ()

and _menhir_run24 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 851 "var/cll/CLLParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BOOL _v ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v
    | CPL ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState24
    | INT _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v
    | LABEL _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState24
    | MULT ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState24
    | STACKPOINTER ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState24
    | SUB ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState24
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState24

and _menhir_run26 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 885 "var/cll/CLLParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BOOL _v ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
    | CPL ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | INT _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
    | LABEL _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | MULT ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | STACKPOINTER ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | SUB ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState26

and _menhir_run33 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 919 "var/cll/CLLParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BOOL _v ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
    | CPL ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | INT _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
    | LABEL _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | MULT ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | STACKPOINTER ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | SUB ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState33

and _menhir_run35 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 953 "var/cll/CLLParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BOOL _v ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
    | CPL ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | INT _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
    | LABEL _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | MULT ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | STACKPOINTER ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | SUB ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState35

and _menhir_run28 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 987 "var/cll/CLLParser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _startpos ->
    let _menhir_stack = (_menhir_stack, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BOOL _v ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
    | CPL ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | INT _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
    | LABEL _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | MULT ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | STACKPOINTER ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | SUB ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState28

and _menhir_run39 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1022 "var/cll/CLLParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BOOL _v ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | CPL ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | INT _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | LABEL _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | MULT ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | STACKPOINTER ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | SUB ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState39

and _menhir_run41 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1056 "var/cll/CLLParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BOOL _v ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
    | CPL ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | INT _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
    | LABEL _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | MULT ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | STACKPOINTER ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | SUB ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState41

and _menhir_run43 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1090 "var/cll/CLLParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BOOL _v ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
    | CPL ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | INT _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
    | LABEL _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | MULT ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | STACKPOINTER ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | SUB ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState43

and _menhir_run45 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1124 "var/cll/CLLParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BOOL _v ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | CPL ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | INT _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | LABEL _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | MULT ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | STACKPOINTER ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | SUB ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState45

and _menhir_run47 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1158 "var/cll/CLLParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BOOL _v ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
    | CPL ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | INT _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
    | LABEL _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | MULT ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | STACKPOINTER ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | SUB ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState47

and _menhir_run30 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1192 "var/cll/CLLParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BOOL _v ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | CPL ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | INT _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | LABEL _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | MULT ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | STACKPOINTER ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | SUB ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState30

and _menhir_run49 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1226 "var/cll/CLLParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BOOL _v ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
    | CPL ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | INT _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
    | LABEL _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | MULT ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | STACKPOINTER ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | SUB ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState49

and _menhir_run37 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1260 "var/cll/CLLParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BOOL _v ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | CPL ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | INT _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | LABEL _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | MULT ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | STACKPOINTER ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | SUB ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState37

and _menhir_goto_control : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 81 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs ARTTree.compiler_type)
# 1294 "var/cll/CLLParser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState4 | MenhirState5 | MenhirState58 | MenhirState119 | MenhirState65 | MenhirState116 | MenhirState114 | MenhirState111 | MenhirState109 | MenhirState102 | MenhirState87 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv471 * Lexing.position * _menhir_state * (
# 81 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs ARTTree.compiler_type)
# 1304 "var/cll/CLLParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv469 * Lexing.position * _menhir_state * (
# 81 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs ARTTree.compiler_type)
# 1310 "var/cll/CLLParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _endpos_c_, _menhir_s, (c : (
# 81 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs ARTTree.compiler_type)
# 1315 "var/cll/CLLParser.ml"
        )), _startpos_c_) = _menhir_stack in
        let _endpos = _endpos_c_ in
        let _v : (
# 80 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs ARTTree.compiler_type)
# 1321 "var/cll/CLLParser.ml"
        ) = 
# 316 "var/cll/CLLParser.mly"
    ( c )
# 1325 "var/cll/CLLParser.ml"
         in
        _menhir_goto_block _menhir_env _menhir_stack _endpos _menhir_s _v) : 'freshtv470)) : 'freshtv472)
    | MenhirState59 | MenhirState60 | MenhirState61 | MenhirState131 | MenhirState125 | MenhirState128 | MenhirState126 | MenhirState123 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv477 * Lexing.position * _menhir_state * (
# 81 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs ARTTree.compiler_type)
# 1333 "var/cll/CLLParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BREAK ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState125 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | COLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv473 * Lexing.position * _menhir_state * (
# 81 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs ARTTree.compiler_type)
# 1345 "var/cll/CLLParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState125 in
            ((let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BREAK ->
                _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | CONTINUE ->
                _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EXIT ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOR ->
                _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IF ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LABEL _v ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MULT ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NOP ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | PRINT ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RETURN ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | WHILE ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RB ->
                _menhir_reduce65 _menhir_env (Obj.magic _menhir_stack) MenhirState128
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState128) : 'freshtv474)
        | CONTINUE ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState125 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | EXIT ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState125 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | FOR ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState125 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | IF ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState125 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LABEL _v ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState125 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MULT ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState125 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NOP ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState125 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | PRINT ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState125 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | RETURN ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState125 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv475 * Lexing.position * _menhir_state * (
# 81 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs ARTTree.compiler_type)
# 1403 "var/cll/CLLParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_menhir_s : _menhir_state) = MenhirState125 in
            ((let _menhir_stack = (_menhir_stack, _endpos, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BREAK ->
                _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | CONTINUE ->
                _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EXIT ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOR ->
                _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IF ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LABEL _v ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MULT ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NOP ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | PRINT ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RETURN ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | WHILE ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RB ->
                _menhir_reduce65 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState126) : 'freshtv476)
        | WHILE ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState125 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | RB ->
            _menhir_reduce65 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState125) : 'freshtv478)
    | _ ->
        _menhir_fail ()

and _menhir_goto_data_declarations : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 83 "var/cll/CLLParser.mly"
      (ARTTree.datas ARTTree.compiler_type)
# 1453 "var/cll/CLLParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState147 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv459 * _menhir_state * (
# 70 "var/cll/CLLParser.mly"
       (string)
# 1463 "var/cll/CLLParser.ml"
        ) * Lexing.position)) * (
# 69 "var/cll/CLLParser.mly"
       (int)
# 1467 "var/cll/CLLParser.ml"
        )) * _menhir_state * (
# 83 "var/cll/CLLParser.mly"
      (ARTTree.datas ARTTree.compiler_type)
# 1471 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv457 * _menhir_state * (
# 70 "var/cll/CLLParser.mly"
       (string)
# 1477 "var/cll/CLLParser.ml"
        ) * Lexing.position)) * (
# 69 "var/cll/CLLParser.mly"
       (int)
# 1481 "var/cll/CLLParser.ml"
        )) * _menhir_state * (
# 83 "var/cll/CLLParser.mly"
      (ARTTree.datas ARTTree.compiler_type)
# 1485 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s, (t : (
# 70 "var/cll/CLLParser.mly"
       (string)
# 1490 "var/cll/CLLParser.ml"
        )), _startpos_t_), (v : (
# 69 "var/cll/CLLParser.mly"
       (int)
# 1494 "var/cll/CLLParser.ml"
        ))), _, (s : (
# 83 "var/cll/CLLParser.mly"
      (ARTTree.datas ARTTree.compiler_type)
# 1498 "var/cll/CLLParser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : (
# 83 "var/cll/CLLParser.mly"
      (ARTTree.datas ARTTree.compiler_type)
# 1504 "var/cll/CLLParser.ml"
        ) = let _startpos = _startpos_t_ in
        
# 378 "var/cll/CLLParser.mly"
    (
      try
        let tag_set = add t s.tag_set in
        let tag = make_node _startpos (t, v) in
        let syntax_tree = Cycle.prepend s.syntax_tree tag in
        {syntax_tree; tag_set}
      with
      | DuplicateElement t ->
        raise_duplicate_element _startpos t 
    )
# 1518 "var/cll/CLLParser.ml"
         in
        _menhir_goto_data_declarations _menhir_env _menhir_stack _menhir_s _v) : 'freshtv458)) : 'freshtv460)
    | MenhirState144 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv467 * _menhir_state * 'tv_list_procedure_declaration_ * Lexing.position)) * _menhir_state * (
# 83 "var/cll/CLLParser.mly"
      (ARTTree.datas ARTTree.compiler_type)
# 1526 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EOF ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv463 * _menhir_state * 'tv_list_procedure_declaration_ * Lexing.position)) * _menhir_state * (
# 83 "var/cll/CLLParser.mly"
      (ARTTree.datas ARTTree.compiler_type)
# 1536 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv461 * _menhir_state * 'tv_list_procedure_declaration_ * Lexing.position)) * _menhir_state * (
# 83 "var/cll/CLLParser.mly"
      (ARTTree.datas ARTTree.compiler_type)
# 1542 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (globals : 'tv_list_procedure_declaration_), _startpos_globals_), _, (data : (
# 83 "var/cll/CLLParser.mly"
      (ARTTree.datas ARTTree.compiler_type)
# 1547 "var/cll/CLLParser.ml"
            ))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : (
# 74 "var/cll/CLLParser.mly"
      (CLLTree.cll_prog ARTTree.compiler_type)
# 1554 "var/cll/CLLParser.ml"
            ) = let _startpos = _startpos_globals_ in
            
# 102 "var/cll/CLLParser.mly"
    (
      try
        let (tag_set,syntax_tree) = List.fold_left make_compiler_type (empty,Cycle.empty_cycle) globals in
        let tag_set = union tag_set data.tag_set in
        let syntax_tree = Procedure_Definition_Data (syntax_tree, data.syntax_tree) in
        {tag_set; syntax_tree}
      with
      | DuplicateElement t ->
        raise_duplicate_element _startpos t
    )
# 1568 "var/cll/CLLParser.ml"
             in
            _menhir_goto_program _menhir_env _menhir_stack _menhir_s _v) : 'freshtv462)) : 'freshtv464)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv465 * _menhir_state * 'tv_list_procedure_declaration_ * Lexing.position)) * _menhir_state * (
# 83 "var/cll/CLLParser.mly"
      (ARTTree.datas ARTTree.compiler_type)
# 1578 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv466)) : 'freshtv468)
    | _ ->
        _menhir_fail ()

and _menhir_goto_instructions : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 75 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs ARTTree.compiler_type)
# 1588 "var/cll/CLLParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState61 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv423 * _menhir_state * (
# 70 "var/cll/CLLParser.mly"
       (string)
# 1598 "var/cll/CLLParser.ml"
        ) * Lexing.position) * Lexing.position * _menhir_state) * _menhir_state * (
# 75 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs ARTTree.compiler_type)
# 1602 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv421 * _menhir_state * (
# 70 "var/cll/CLLParser.mly"
       (string)
# 1608 "var/cll/CLLParser.ml"
        ) * Lexing.position) * Lexing.position * _menhir_state) * _menhir_state * (
# 75 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs ARTTree.compiler_type)
# 1612 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s, (t : (
# 70 "var/cll/CLLParser.mly"
       (string)
# 1617 "var/cll/CLLParser.ml"
        )), _startpos_t_), _endpos__2_, _), _, (_3 : (
# 75 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs ARTTree.compiler_type)
# 1621 "var/cll/CLLParser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : (
# 75 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs ARTTree.compiler_type)
# 1627 "var/cll/CLLParser.ml"
        ) = let _startpos = _startpos_t_ in
        
# 164 "var/cll/CLLParser.mly"
    (
      raise_syntax_error _startpos "Expected ':', found ';'."
    )
# 1634 "var/cll/CLLParser.ml"
         in
        _menhir_goto_instructions _menhir_env _menhir_stack _menhir_s _v) : 'freshtv422)) : 'freshtv424)
    | MenhirState123 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv427 * _menhir_state * (
# 78 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 1642 "var/cll/CLLParser.ml"
        ) * Lexing.position) * Lexing.position) * _menhir_state * (
# 75 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs ARTTree.compiler_type)
# 1646 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv425 * _menhir_state * (
# 78 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 1652 "var/cll/CLLParser.ml"
        ) * Lexing.position) * Lexing.position) * _menhir_state * (
# 75 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs ARTTree.compiler_type)
# 1656 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s, (i : (
# 78 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 1661 "var/cll/CLLParser.ml"
        )), _startpos_i_), _endpos__2_), _, (s : (
# 75 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs ARTTree.compiler_type)
# 1665 "var/cll/CLLParser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : (
# 75 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs ARTTree.compiler_type)
# 1671 "var/cll/CLLParser.ml"
        ) = let _startpos = _startpos_i_ in
        
# 136 "var/cll/CLLParser.mly"
    (
      try
        let syntax_tree = Cycle.prepend s.syntax_tree i in
        {tag_set = s.tag_set; syntax_tree}
      with
      | DuplicateElement t ->
        raise_duplicate_element _startpos t
    )
# 1683 "var/cll/CLLParser.ml"
         in
        _menhir_goto_instructions _menhir_env _menhir_stack _menhir_s _v) : 'freshtv426)) : 'freshtv428)
    | MenhirState126 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv431 * Lexing.position * _menhir_state * (
# 81 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs ARTTree.compiler_type)
# 1691 "var/cll/CLLParser.ml"
        ) * Lexing.position) * Lexing.position * _menhir_state) * _menhir_state * (
# 75 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs ARTTree.compiler_type)
# 1695 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv429 * Lexing.position * _menhir_state * (
# 81 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs ARTTree.compiler_type)
# 1701 "var/cll/CLLParser.ml"
        ) * Lexing.position) * Lexing.position * _menhir_state) * _menhir_state * (
# 75 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs ARTTree.compiler_type)
# 1705 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _endpos_c_, _menhir_s, (c : (
# 81 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs ARTTree.compiler_type)
# 1710 "var/cll/CLLParser.ml"
        )), _startpos_c_), _endpos__2_, _), _, (s : (
# 75 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs ARTTree.compiler_type)
# 1714 "var/cll/CLLParser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : (
# 75 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs ARTTree.compiler_type)
# 1720 "var/cll/CLLParser.ml"
        ) = let _startpos = _startpos_c_ in
        
# 147 "var/cll/CLLParser.mly"
    (
      try
        let tag_set = union c.tag_set s.tag_set in
        let syntax_tree = Cycle.extend c.syntax_tree s.syntax_tree in
        {tag_set; syntax_tree}
      with
      | DuplicateElement t ->
        raise_duplicate_element _startpos t 
    )
# 1733 "var/cll/CLLParser.ml"
         in
        _menhir_goto_instructions _menhir_env _menhir_stack _menhir_s _v) : 'freshtv430)) : 'freshtv432)
    | MenhirState128 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv435 * Lexing.position * _menhir_state * (
# 81 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs ARTTree.compiler_type)
# 1741 "var/cll/CLLParser.ml"
        ) * Lexing.position) * _menhir_state) * _menhir_state * (
# 75 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs ARTTree.compiler_type)
# 1745 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv433 * Lexing.position * _menhir_state * (
# 81 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs ARTTree.compiler_type)
# 1751 "var/cll/CLLParser.ml"
        ) * Lexing.position) * _menhir_state) * _menhir_state * (
# 75 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs ARTTree.compiler_type)
# 1755 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _endpos__1_, _menhir_s, (_1 : (
# 81 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs ARTTree.compiler_type)
# 1760 "var/cll/CLLParser.ml"
        )), _startpos__1_), _), _, (_3 : (
# 75 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs ARTTree.compiler_type)
# 1764 "var/cll/CLLParser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : (
# 75 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs ARTTree.compiler_type)
# 1770 "var/cll/CLLParser.ml"
        ) = let _startpos = _startpos__1_ in
        
# 159 "var/cll/CLLParser.mly"
    (
      raise_syntax_error _startpos "Expected ';', found ':'."
    )
# 1777 "var/cll/CLLParser.ml"
         in
        _menhir_goto_instructions _menhir_env _menhir_stack _menhir_s _v) : 'freshtv434)) : 'freshtv436)
    | MenhirState125 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv439 * Lexing.position * _menhir_state * (
# 81 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs ARTTree.compiler_type)
# 1785 "var/cll/CLLParser.ml"
        ) * Lexing.position) * _menhir_state * (
# 75 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs ARTTree.compiler_type)
# 1789 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv437 * Lexing.position * _menhir_state * (
# 81 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs ARTTree.compiler_type)
# 1795 "var/cll/CLLParser.ml"
        ) * Lexing.position) * _menhir_state * (
# 75 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs ARTTree.compiler_type)
# 1799 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _endpos_c_, _menhir_s, (c : (
# 81 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs ARTTree.compiler_type)
# 1804 "var/cll/CLLParser.ml"
        )), _startpos_c_), _, (s : (
# 75 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs ARTTree.compiler_type)
# 1808 "var/cll/CLLParser.ml"
        ))) = _menhir_stack in
        let _v : (
# 75 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs ARTTree.compiler_type)
# 1813 "var/cll/CLLParser.ml"
        ) = let _startpos = _startpos_c_ in
        
# 147 "var/cll/CLLParser.mly"
    (
      try
        let tag_set = union c.tag_set s.tag_set in
        let syntax_tree = Cycle.extend c.syntax_tree s.syntax_tree in
        {tag_set; syntax_tree}
      with
      | DuplicateElement t ->
        raise_duplicate_element _startpos t 
    )
# 1826 "var/cll/CLLParser.ml"
         in
        _menhir_goto_instructions _menhir_env _menhir_stack _menhir_s _v) : 'freshtv438)) : 'freshtv440)
    | MenhirState131 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv443 * _menhir_state * (
# 78 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 1834 "var/cll/CLLParser.ml"
        ) * Lexing.position)) * _menhir_state * (
# 75 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs ARTTree.compiler_type)
# 1838 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv441 * _menhir_state * (
# 78 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 1844 "var/cll/CLLParser.ml"
        ) * Lexing.position)) * _menhir_state * (
# 75 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs ARTTree.compiler_type)
# 1848 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (_1 : (
# 78 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 1853 "var/cll/CLLParser.ml"
        )), _startpos__1_), _, (_3 : (
# 75 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs ARTTree.compiler_type)
# 1857 "var/cll/CLLParser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : (
# 75 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs ARTTree.compiler_type)
# 1863 "var/cll/CLLParser.ml"
        ) = let _startpos = _startpos__1_ in
        
# 159 "var/cll/CLLParser.mly"
    (
      raise_syntax_error _startpos "Expected ';', found ':'."
    )
# 1870 "var/cll/CLLParser.ml"
         in
        _menhir_goto_instructions _menhir_env _menhir_stack _menhir_s _v) : 'freshtv442)) : 'freshtv444)
    | MenhirState60 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv447 * _menhir_state * (
# 70 "var/cll/CLLParser.mly"
       (string)
# 1878 "var/cll/CLLParser.ml"
        ) * Lexing.position) * _menhir_state * (
# 75 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs ARTTree.compiler_type)
# 1882 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv445 * _menhir_state * (
# 70 "var/cll/CLLParser.mly"
       (string)
# 1888 "var/cll/CLLParser.ml"
        ) * Lexing.position) * _menhir_state * (
# 75 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs ARTTree.compiler_type)
# 1892 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (_1 : (
# 70 "var/cll/CLLParser.mly"
       (string)
# 1897 "var/cll/CLLParser.ml"
        )), _startpos__1_), _, (_2 : (
# 75 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs ARTTree.compiler_type)
# 1901 "var/cll/CLLParser.ml"
        ))) = _menhir_stack in
        let _v : (
# 75 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs ARTTree.compiler_type)
# 1906 "var/cll/CLLParser.ml"
        ) = let _startpos = _startpos__1_ in
        
# 169 "var/cll/CLLParser.mly"
    (
      raise_syntax_error _startpos "You may have forgotten a ':'."
    )
# 1913 "var/cll/CLLParser.ml"
         in
        _menhir_goto_instructions _menhir_env _menhir_stack _menhir_s _v) : 'freshtv446)) : 'freshtv448)
    | MenhirState59 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv455 * _menhir_state) * _menhir_state * (
# 75 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs ARTTree.compiler_type)
# 1921 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RB ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv451 * _menhir_state) * _menhir_state * (
# 75 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs ARTTree.compiler_type)
# 1931 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv449 * _menhir_state) * _menhir_state * (
# 75 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs ARTTree.compiler_type)
# 1939 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            let (_endpos__3_ : Lexing.position) = _endpos in
            ((let ((_menhir_stack, _menhir_s), _, (i : (
# 75 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs ARTTree.compiler_type)
# 1945 "var/cll/CLLParser.ml"
            ))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _endpos = _endpos__3_ in
            let _v : (
# 80 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs ARTTree.compiler_type)
# 1953 "var/cll/CLLParser.ml"
            ) = 
# 319 "var/cll/CLLParser.mly"
    ( i )
# 1957 "var/cll/CLLParser.ml"
             in
            _menhir_goto_block _menhir_env _menhir_stack _endpos _menhir_s _v) : 'freshtv450)) : 'freshtv452)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv453 * _menhir_state) * _menhir_state * (
# 75 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs ARTTree.compiler_type)
# 1967 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv454)) : 'freshtv456)
    | _ ->
        _menhir_fail ()

and _menhir_goto_l_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 77 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1977 "var/cll/CLLParser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState12 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv389 * _menhir_state * Lexing.position) * _menhir_state * (
# 77 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1987 "var/cll/CLLParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv387 * _menhir_state * Lexing.position) * _menhir_state * (
# 77 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1993 "var/cll/CLLParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _startpos__1_), _, (l : (
# 77 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1998 "var/cll/CLLParser.ml"
        )), _startpos_l_) = _menhir_stack in
        let _1 = () in
        let _startpos = _startpos__1_ in
        let _v : (
# 77 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2005 "var/cll/CLLParser.ml"
        ) = 
# 227 "var/cll/CLLParser.mly"
    ( LStar l )
# 2009 "var/cll/CLLParser.ml"
         in
        _menhir_goto_l_expr _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv388)) : 'freshtv390)
    | MenhirState81 | MenhirState63 | MenhirState56 | MenhirState8 | MenhirState9 | MenhirState11 | MenhirState49 | MenhirState47 | MenhirState45 | MenhirState43 | MenhirState41 | MenhirState39 | MenhirState37 | MenhirState35 | MenhirState33 | MenhirState30 | MenhirState28 | MenhirState26 | MenhirState24 | MenhirState15 | MenhirState17 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv391 * _menhir_state * (
# 77 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2017 "var/cll/CLLParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        (_menhir_reduce29 _menhir_env (Obj.magic _menhir_stack) : 'freshtv392)
    | MenhirState67 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv399 * _menhir_state * (
# 70 "var/cll/CLLParser.mly"
       (string)
# 2025 "var/cll/CLLParser.ml"
        ) * Lexing.position) * _menhir_state) * _menhir_state * (
# 77 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2029 "var/cll/CLLParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RP ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv395 * _menhir_state * (
# 70 "var/cll/CLLParser.mly"
       (string)
# 2039 "var/cll/CLLParser.ml"
            ) * Lexing.position) * _menhir_state) * _menhir_state * (
# 77 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2043 "var/cll/CLLParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv393 * _menhir_state * (
# 70 "var/cll/CLLParser.mly"
       (string)
# 2050 "var/cll/CLLParser.ml"
            ) * Lexing.position) * _menhir_state) * _menhir_state * (
# 77 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2054 "var/cll/CLLParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (t : (
# 70 "var/cll/CLLParser.mly"
       (string)
# 2059 "var/cll/CLLParser.ml"
            )), _startpos_t_), _), _, (e : (
# 77 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2063 "var/cll/CLLParser.ml"
            )), _startpos_e_) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _startpos = _startpos_t_ in
            let _v : (
# 78 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 2071 "var/cll/CLLParser.ml"
            ) = let _startpos = _startpos_t_ in
            
# 261 "var/cll/CLLParser.mly"
    (
      raise_syntax_error _startpos (Printf.sprintf "Unknown instruction '%s'." t)
    )
# 2078 "var/cll/CLLParser.ml"
             in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv394)) : 'freshtv396)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv397 * _menhir_state * (
# 70 "var/cll/CLLParser.mly"
       (string)
# 2088 "var/cll/CLLParser.ml"
            ) * Lexing.position) * _menhir_state) * _menhir_state * (
# 77 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2092 "var/cll/CLLParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv398)) : 'freshtv400)
    | MenhirState105 | MenhirState71 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv403 * _menhir_state * (
# 77 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2101 "var/cll/CLLParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADDASSIGN ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | DECR ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | DIVASSIGN ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | MULTASSIGN ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | SUBASSIGN ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | ADD | AND | DIV | EQ | GE | GT | LE | LT | MULT | NEQ | OR | REM | RP | SEMI | SUB ->
            _menhir_reduce29 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv401 * _menhir_state * (
# 77 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2129 "var/cll/CLLParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv402)) : 'freshtv404)
    | MenhirState107 | MenhirState100 | MenhirState84 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv407 * _menhir_state * (
# 77 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2138 "var/cll/CLLParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADDASSIGN ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | DECR ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | DIVASSIGN ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | MULTASSIGN ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | SUBASSIGN ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv405 * _menhir_state * (
# 77 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2164 "var/cll/CLLParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv406)) : 'freshtv408)
    | MenhirState4 | MenhirState5 | MenhirState58 | MenhirState59 | MenhirState60 | MenhirState131 | MenhirState125 | MenhirState128 | MenhirState126 | MenhirState123 | MenhirState61 | MenhirState119 | MenhirState65 | MenhirState116 | MenhirState114 | MenhirState111 | MenhirState109 | MenhirState102 | MenhirState87 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv419 * _menhir_state * (
# 77 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2173 "var/cll/CLLParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADDASSIGN ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | DECR ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | DIVASSIGN ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | LP ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv415 * _menhir_state * (
# 77 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2193 "var/cll/CLLParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | RP ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv411 * _menhir_state * (
# 77 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2203 "var/cll/CLLParser.ml"
                ) * Lexing.position)) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv409 * _menhir_state * (
# 77 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2210 "var/cll/CLLParser.ml"
                ) * Lexing.position)) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, (l_e : (
# 77 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2215 "var/cll/CLLParser.ml"
                )), _startpos_l_e_) = _menhir_stack in
                let _3 = () in
                let _2 = () in
                let _startpos = _startpos_l_e_ in
                let _v : (
# 78 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 2223 "var/cll/CLLParser.ml"
                ) = 
# 253 "var/cll/CLLParser.mly"
    (
      Call l_e
    )
# 2229 "var/cll/CLLParser.ml"
                 in
                _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv410)) : 'freshtv412)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv413 * _menhir_state * (
# 77 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2239 "var/cll/CLLParser.ml"
                ) * Lexing.position)) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv414)) : 'freshtv416)
        | MULTASSIGN ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | SUBASSIGN ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv417 * _menhir_state * (
# 77 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2254 "var/cll/CLLParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv418)) : 'freshtv420)
    | _ ->
        _menhir_fail ()

and _menhir_goto_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2264 "var/cll/CLLParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState17 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv253 * _menhir_state) * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2274 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv251 * _menhir_state) * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2280 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _, (e : (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2285 "var/cll/CLLParser.ml"
        ))) = _menhir_stack in
        let _1 = () in
        let _v : (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2291 "var/cll/CLLParser.ml"
        ) = 
# 216 "var/cll/CLLParser.mly"
    ( Unop(Cpl, e) )
# 2295 "var/cll/CLLParser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv252)) : 'freshtv254)
    | MenhirState15 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv261 * _menhir_state) * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2303 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADD ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NEQ ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | RP ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv257 * _menhir_state) * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2337 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv255 * _menhir_state) * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2344 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (e : (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2349 "var/cll/CLLParser.ml"
            ))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2356 "var/cll/CLLParser.ml"
            ) = 
# 186 "var/cll/CLLParser.mly"
    ( e )
# 2360 "var/cll/CLLParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv256)) : 'freshtv258)
        | SUB ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv259 * _menhir_state) * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2372 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv260)) : 'freshtv262)
    | MenhirState24 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv267 * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2381 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2385 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIV ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | REM ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | ADD | AND | COLON | COMMA | EQ | GE | GT | LE | LT | NEQ | OR | RP | SEMI | SUB ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv263 * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2401 "var/cll/CLLParser.ml"
            ))) * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2405 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (e1 : (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2410 "var/cll/CLLParser.ml"
            ))), _, (e2 : (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2414 "var/cll/CLLParser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2420 "var/cll/CLLParser.ml"
            ) = 
# 190 "var/cll/CLLParser.mly"
    ( Binop(e1, Sub, e2) )
# 2424 "var/cll/CLLParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv264)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv265 * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2434 "var/cll/CLLParser.ml"
            ))) * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2438 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv266)) : 'freshtv268)
    | MenhirState26 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv271 * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2447 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2451 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv269 * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2457 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2461 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (e1 : (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2466 "var/cll/CLLParser.ml"
        ))), _, (e2 : (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2470 "var/cll/CLLParser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2476 "var/cll/CLLParser.ml"
        ) = 
# 196 "var/cll/CLLParser.mly"
    ( Binop(e1, Rem, e2) )
# 2480 "var/cll/CLLParser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv270)) : 'freshtv272)
    | MenhirState28 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv275 * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2488 "var/cll/CLLParser.ml"
        )) * Lexing.position) * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2492 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv273 * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2498 "var/cll/CLLParser.ml"
        )) * Lexing.position) * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2502 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s, (e1 : (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2507 "var/cll/CLLParser.ml"
        ))), _startpos__2_), _, (e2 : (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2511 "var/cll/CLLParser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2517 "var/cll/CLLParser.ml"
        ) = 
# 192 "var/cll/CLLParser.mly"
    ( Binop(e1, Mult, e2) )
# 2521 "var/cll/CLLParser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv274)) : 'freshtv276)
    | MenhirState30 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv279 * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2529 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2533 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv277 * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2539 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2543 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (e1 : (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2548 "var/cll/CLLParser.ml"
        ))), _, (e2 : (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2552 "var/cll/CLLParser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2558 "var/cll/CLLParser.ml"
        ) = 
# 194 "var/cll/CLLParser.mly"
    ( Binop(e1, Div, e2) )
# 2562 "var/cll/CLLParser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv278)) : 'freshtv280)
    | MenhirState33 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv285 * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2570 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2574 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADD ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NEQ ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | AND | COLON | COMMA | OR | RP | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv281 * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2606 "var/cll/CLLParser.ml"
            ))) * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2610 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (e1 : (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2615 "var/cll/CLLParser.ml"
            ))), _, (e2 : (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2619 "var/cll/CLLParser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2625 "var/cll/CLLParser.ml"
            ) = 
# 200 "var/cll/CLLParser.mly"
    ( Binop(e1, Or, e2) )
# 2629 "var/cll/CLLParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv282)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv283 * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2639 "var/cll/CLLParser.ml"
            ))) * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2643 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv284)) : 'freshtv286)
    | MenhirState35 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv291 * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2652 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2656 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADD ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | REM ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | AND | COLON | COMMA | EQ | GE | GT | LE | LT | NEQ | OR | RP | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv287 * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2676 "var/cll/CLLParser.ml"
            ))) * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2680 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (e1 : (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2685 "var/cll/CLLParser.ml"
            ))), _, (e2 : (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2689 "var/cll/CLLParser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2695 "var/cll/CLLParser.ml"
            ) = 
# 212 "var/cll/CLLParser.mly"
    ( Binop(e1, Neq, e2) )
# 2699 "var/cll/CLLParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv288)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv289 * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2709 "var/cll/CLLParser.ml"
            ))) * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2713 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv290)) : 'freshtv292)
    | MenhirState37 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv297 * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2722 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2726 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIV ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | REM ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | ADD | AND | COLON | COMMA | EQ | GE | GT | LE | LT | NEQ | OR | RP | SEMI | SUB ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv293 * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2742 "var/cll/CLLParser.ml"
            ))) * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2746 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (e1 : (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2751 "var/cll/CLLParser.ml"
            ))), _, (e2 : (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2755 "var/cll/CLLParser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2761 "var/cll/CLLParser.ml"
            ) = 
# 188 "var/cll/CLLParser.mly"
    ( Binop(e1, Add, e2) )
# 2765 "var/cll/CLLParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv294)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv295 * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2775 "var/cll/CLLParser.ml"
            ))) * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2779 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv296)) : 'freshtv298)
    | MenhirState39 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv303 * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2788 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2792 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADD ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | REM ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | AND | COLON | COMMA | EQ | GE | GT | LE | LT | NEQ | OR | RP | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv299 * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2812 "var/cll/CLLParser.ml"
            ))) * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2816 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (e1 : (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2821 "var/cll/CLLParser.ml"
            ))), _, (e2 : (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2825 "var/cll/CLLParser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2831 "var/cll/CLLParser.ml"
            ) = 
# 202 "var/cll/CLLParser.mly"
    ( Binop(e1, Lt, e2) )
# 2835 "var/cll/CLLParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv300)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv301 * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2845 "var/cll/CLLParser.ml"
            ))) * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2849 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv302)) : 'freshtv304)
    | MenhirState41 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv309 * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2858 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2862 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADD ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | REM ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | AND | COLON | COMMA | EQ | GE | GT | LE | LT | NEQ | OR | RP | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv305 * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2882 "var/cll/CLLParser.ml"
            ))) * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2886 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (e1 : (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2891 "var/cll/CLLParser.ml"
            ))), _, (e2 : (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2895 "var/cll/CLLParser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2901 "var/cll/CLLParser.ml"
            ) = 
# 204 "var/cll/CLLParser.mly"
    ( Binop(e1, Le, e2) )
# 2905 "var/cll/CLLParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv306)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv307 * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2915 "var/cll/CLLParser.ml"
            ))) * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2919 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv308)) : 'freshtv310)
    | MenhirState43 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv315 * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2928 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2932 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADD ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | REM ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | AND | COLON | COMMA | EQ | GE | GT | LE | LT | NEQ | OR | RP | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv311 * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2952 "var/cll/CLLParser.ml"
            ))) * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2956 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (e1 : (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2961 "var/cll/CLLParser.ml"
            ))), _, (e2 : (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2965 "var/cll/CLLParser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2971 "var/cll/CLLParser.ml"
            ) = 
# 206 "var/cll/CLLParser.mly"
    ( Binop(e1, Gt, e2) )
# 2975 "var/cll/CLLParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv312)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv313 * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2985 "var/cll/CLLParser.ml"
            ))) * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2989 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv314)) : 'freshtv316)
    | MenhirState45 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv321 * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2998 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3002 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADD ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | REM ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | AND | COLON | COMMA | EQ | GE | GT | LE | LT | NEQ | OR | RP | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv317 * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3022 "var/cll/CLLParser.ml"
            ))) * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3026 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (e1 : (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3031 "var/cll/CLLParser.ml"
            ))), _, (e2 : (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3035 "var/cll/CLLParser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3041 "var/cll/CLLParser.ml"
            ) = 
# 208 "var/cll/CLLParser.mly"
    ( Binop(e1, Ge, e2) )
# 3045 "var/cll/CLLParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv318)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv319 * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3055 "var/cll/CLLParser.ml"
            ))) * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3059 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv320)) : 'freshtv322)
    | MenhirState47 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv327 * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3068 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3072 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADD ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | REM ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | AND | COLON | COMMA | EQ | GE | GT | LE | LT | NEQ | OR | RP | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv323 * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3092 "var/cll/CLLParser.ml"
            ))) * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3096 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (e1 : (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3101 "var/cll/CLLParser.ml"
            ))), _, (e2 : (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3105 "var/cll/CLLParser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3111 "var/cll/CLLParser.ml"
            ) = 
# 210 "var/cll/CLLParser.mly"
    ( Binop(e1, Eq, e2) )
# 3115 "var/cll/CLLParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv324)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv325 * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3125 "var/cll/CLLParser.ml"
            ))) * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3129 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv326)) : 'freshtv328)
    | MenhirState49 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv333 * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3138 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3142 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADD ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NEQ ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | AND | COLON | COMMA | OR | RP | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv329 * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3174 "var/cll/CLLParser.ml"
            ))) * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3178 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (e1 : (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3183 "var/cll/CLLParser.ml"
            ))), _, (e2 : (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3187 "var/cll/CLLParser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3193 "var/cll/CLLParser.ml"
            ) = 
# 198 "var/cll/CLLParser.mly"
    ( Binop(e1, And, e2) )
# 3197 "var/cll/CLLParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv330)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv331 * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3207 "var/cll/CLLParser.ml"
            ))) * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3211 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv332)) : 'freshtv334)
    | MenhirState11 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv337 * _menhir_state) * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3220 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv335 * _menhir_state) * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3226 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _, (e : (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3231 "var/cll/CLLParser.ml"
        ))) = _menhir_stack in
        let _1 = () in
        let _v : (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3237 "var/cll/CLLParser.ml"
        ) = 
# 218 "var/cll/CLLParser.mly"
    ( Unop(Not, e) )
# 3241 "var/cll/CLLParser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv336)) : 'freshtv338)
    | MenhirState9 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv343 * _menhir_state) * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3249 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIV ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | REM ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | ADD | AND | COLON | COMMA | EQ | GE | GT | LE | LT | NEQ | OR | RP | SEMI | SUB ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv339 * _menhir_state) * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3265 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (e : (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3270 "var/cll/CLLParser.ml"
            ))) = _menhir_stack in
            let _1 = () in
            let _v : (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3276 "var/cll/CLLParser.ml"
            ) = 
# 214 "var/cll/CLLParser.mly"
    ( Unop(Minus, e) )
# 3280 "var/cll/CLLParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv340)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv341 * _menhir_state) * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3290 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv342)) : 'freshtv344)
    | MenhirState8 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv351 * _menhir_state * Lexing.position)) * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3299 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADD ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NEQ ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | RP ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv347 * _menhir_state * Lexing.position)) * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3333 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv345 * _menhir_state * Lexing.position)) * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3340 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, _startpos__1_), _, (e : (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3345 "var/cll/CLLParser.ml"
            ))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _v : (
# 78 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 3354 "var/cll/CLLParser.ml"
            ) = 
# 236 "var/cll/CLLParser.mly"
    ( Print e )
# 3358 "var/cll/CLLParser.ml"
             in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv346)) : 'freshtv348)
        | SUB ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv349 * _menhir_state * Lexing.position)) * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3370 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv350)) : 'freshtv352)
    | MenhirState56 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv357 * _menhir_state * Lexing.position) * _menhir_state) * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3379 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADD ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NEQ ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | RP ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv353 * _menhir_state * Lexing.position) * _menhir_state) * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3413 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BREAK ->
                _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | CONTINUE ->
                _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EXIT ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOR ->
                _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IF ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LABEL _v ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState58
            | MULT ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NOP ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | PRINT ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RETURN ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | WHILE ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState58) : 'freshtv354)
        | SUB ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv355 * _menhir_state * Lexing.position) * _menhir_state) * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3455 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv356)) : 'freshtv358)
    | MenhirState63 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv363 * _menhir_state * Lexing.position)) * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3464 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADD ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NEQ ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | RP ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv359 * _menhir_state * Lexing.position)) * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3498 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BREAK ->
                _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | CONTINUE ->
                _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EXIT ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOR ->
                _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IF ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LABEL _v ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | MULT ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NOP ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | PRINT ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RETURN ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | WHILE ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState65) : 'freshtv360)
        | SUB ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv361 * _menhir_state * Lexing.position)) * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3540 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv362)) : 'freshtv364)
    | MenhirState81 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv369 * _menhir_state * (
# 77 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3549 "var/cll/CLLParser.ml"
        ) * Lexing.position) * 'tv_assign_binop) * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3553 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADD ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NEQ ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | COLON | COMMA | RP | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv365 * _menhir_state * (
# 77 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3589 "var/cll/CLLParser.ml"
            ) * Lexing.position) * 'tv_assign_binop) * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3593 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (l : (
# 77 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3598 "var/cll/CLLParser.ml"
            )), _startpos_l_), (op : 'tv_assign_binop)), _, (e : (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3602 "var/cll/CLLParser.ml"
            ))) = _menhir_stack in
            let _startpos = _startpos_l_ in
            let _v : (
# 79 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 3608 "var/cll/CLLParser.ml"
            ) = 
# 268 "var/cll/CLLParser.mly"
    (
      match op with
      | Standard -> Assign(l, e)
      | AddAssign -> Assign(l, Binop(l, Add, e))
      | SubAssign -> Assign(l, Binop(l, Sub, e)) 
      | MultAssign -> Assign(l, Binop(l, Mult, e))
      | DivAssign -> (Assign(l, Binop(l, Div, e)))
    )
# 3619 "var/cll/CLLParser.ml"
             in
            _menhir_goto_assign _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv366)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv367 * _menhir_state * (
# 77 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3629 "var/cll/CLLParser.ml"
            ) * Lexing.position) * 'tv_assign_binop) * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3633 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv368)) : 'freshtv370)
    | MenhirState71 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv377 * _menhir_state * Lexing.position)) * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3642 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADD ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NEQ ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | RP ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv371 * _menhir_state * Lexing.position)) * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3676 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BREAK ->
                _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | CONTINUE ->
                _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EXIT ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOR ->
                _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IF ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LABEL _v ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState102
            | MULT ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NOP ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | PRINT ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RETURN ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | WHILE ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState102) : 'freshtv372)
        | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv373 * _menhir_state * Lexing.position)) * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3714 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_stack = (_menhir_stack, _endpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LABEL _v ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MULT ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState84) : 'freshtv374)
        | SUB ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv375 * _menhir_state * Lexing.position)) * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3738 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv376)) : 'freshtv378)
    | MenhirState105 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv385 * _menhir_state * Lexing.position)) * _menhir_state * (
# 82 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3747 "var/cll/CLLParser.ml"
        )) * Lexing.position) * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3751 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADD ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NEQ ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | RP ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv379 * _menhir_state * Lexing.position)) * _menhir_state * (
# 82 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3785 "var/cll/CLLParser.ml"
            )) * Lexing.position) * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3789 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BREAK ->
                _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | CONTINUE ->
                _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EXIT ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOR ->
                _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IF ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LABEL _v ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState111
            | MULT ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NOP ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | PRINT ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RETURN ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | WHILE ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState111) : 'freshtv380)
        | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv381 * _menhir_state * Lexing.position)) * _menhir_state * (
# 82 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3827 "var/cll/CLLParser.ml"
            )) * Lexing.position) * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3831 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_stack = (_menhir_stack, _endpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LABEL _v ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MULT ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState107) : 'freshtv382)
        | SUB ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv383 * _menhir_state * Lexing.position)) * _menhir_state * (
# 82 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3855 "var/cll/CLLParser.ml"
            )) * Lexing.position) * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3859 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv384)) : 'freshtv386)
    | _ ->
        _menhir_fail ()

and _menhir_goto_block : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 80 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs ARTTree.compiler_type)
# 3869 "var/cll/CLLParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState87 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv201 * _menhir_state * Lexing.position)) * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3879 "var/cll/CLLParser.ml"
        )) * Lexing.position) * _menhir_state * (
# 82 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3883 "var/cll/CLLParser.ml"
        ))) * Lexing.position * _menhir_state * (
# 80 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs ARTTree.compiler_type)
# 3887 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv199 * _menhir_state * Lexing.position)) * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3893 "var/cll/CLLParser.ml"
        )) * Lexing.position) * _menhir_state * (
# 82 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3897 "var/cll/CLLParser.ml"
        ))) * Lexing.position * _menhir_state * (
# 80 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs ARTTree.compiler_type)
# 3901 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (((((_menhir_stack, _menhir_s, _startpos__1_), _, (_3 : (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3906 "var/cll/CLLParser.ml"
        ))), _endpos__4_), _, (_5 : (
# 82 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3910 "var/cll/CLLParser.ml"
        ))), _endpos__7_, _, (_7 : (
# 80 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs ARTTree.compiler_type)
# 3914 "var/cll/CLLParser.ml"
        ))) = _menhir_stack in
        let _6 = () in
        let _4 = () in
        let _2 = () in
        let _1 = () in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__7_ in
        let _v : (
# 81 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs ARTTree.compiler_type)
# 3925 "var/cll/CLLParser.ml"
        ) = let _startpos = _startpos__1_ in
        
# 361 "var/cll/CLLParser.mly"
    (
      raise_syntax_error _startpos "Ill-formed 'for' loop"
    )
# 3932 "var/cll/CLLParser.ml"
         in
        _menhir_goto_control _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv200)) : 'freshtv202)
    | MenhirState102 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv205 * _menhir_state * Lexing.position)) * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3940 "var/cll/CLLParser.ml"
        ))) * Lexing.position * _menhir_state * (
# 80 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs ARTTree.compiler_type)
# 3944 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv203 * _menhir_state * Lexing.position)) * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3950 "var/cll/CLLParser.ml"
        ))) * Lexing.position * _menhir_state * (
# 80 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs ARTTree.compiler_type)
# 3954 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s, _startpos__1_), _, (_3 : (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3959 "var/cll/CLLParser.ml"
        ))), _endpos__5_, _, (_5 : (
# 80 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs ARTTree.compiler_type)
# 3963 "var/cll/CLLParser.ml"
        ))) = _menhir_stack in
        let _4 = () in
        let _2 = () in
        let _1 = () in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__5_ in
        let _v : (
# 81 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs ARTTree.compiler_type)
# 3973 "var/cll/CLLParser.ml"
        ) = let _startpos = _startpos__1_ in
        
# 366 "var/cll/CLLParser.mly"
    (
      raise_syntax_error _startpos "Ill-formed 'for' loop; You may want to use a 'while' loop instead."
    )
# 3980 "var/cll/CLLParser.ml"
         in
        _menhir_goto_control _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv204)) : 'freshtv206)
    | MenhirState109 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((((('freshtv209 * _menhir_state * Lexing.position)) * _menhir_state * (
# 82 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3988 "var/cll/CLLParser.ml"
        )) * Lexing.position) * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3992 "var/cll/CLLParser.ml"
        )) * Lexing.position) * _menhir_state * (
# 82 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3996 "var/cll/CLLParser.ml"
        ))) * Lexing.position * _menhir_state * (
# 80 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs ARTTree.compiler_type)
# 4000 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((((('freshtv207 * _menhir_state * Lexing.position)) * _menhir_state * (
# 82 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 4006 "var/cll/CLLParser.ml"
        )) * Lexing.position) * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 4010 "var/cll/CLLParser.ml"
        )) * Lexing.position) * _menhir_state * (
# 82 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 4014 "var/cll/CLLParser.ml"
        ))) * Lexing.position * _menhir_state * (
# 80 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs ARTTree.compiler_type)
# 4018 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (((((((_menhir_stack, _menhir_s, _startpos__1_), _, (init : (
# 82 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 4023 "var/cll/CLLParser.ml"
        ))), _endpos__4_), _, (cond : (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 4027 "var/cll/CLLParser.ml"
        ))), _endpos__6_), _, (it : (
# 82 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 4031 "var/cll/CLLParser.ml"
        ))), _endpos_b_, _, (b : (
# 80 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs ARTTree.compiler_type)
# 4035 "var/cll/CLLParser.ml"
        ))) = _menhir_stack in
        let _8 = () in
        let _6 = () in
        let _4 = () in
        let _2 = () in
        let _1 = () in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_b_ in
        let _v : (
# 81 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs ARTTree.compiler_type)
# 4047 "var/cll/CLLParser.ml"
        ) = 
# 347 "var/cll/CLLParser.mly"
    (
      {syntax_tree = for_to_while init cond it b.syntax_tree; 
      tag_set = b.tag_set}
    )
# 4054 "var/cll/CLLParser.ml"
         in
        _menhir_goto_control _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv208)) : 'freshtv210)
    | MenhirState111 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv213 * _menhir_state * Lexing.position)) * _menhir_state * (
# 82 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 4062 "var/cll/CLLParser.ml"
        )) * Lexing.position) * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 4066 "var/cll/CLLParser.ml"
        ))) * Lexing.position * _menhir_state * (
# 80 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs ARTTree.compiler_type)
# 4070 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv211 * _menhir_state * Lexing.position)) * _menhir_state * (
# 82 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 4076 "var/cll/CLLParser.ml"
        )) * Lexing.position) * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 4080 "var/cll/CLLParser.ml"
        ))) * Lexing.position * _menhir_state * (
# 80 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs ARTTree.compiler_type)
# 4084 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (((((_menhir_stack, _menhir_s, _startpos__1_), _, (_3 : (
# 82 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 4089 "var/cll/CLLParser.ml"
        ))), _endpos__4_), _, (_5 : (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 4093 "var/cll/CLLParser.ml"
        ))), _endpos__7_, _, (_7 : (
# 80 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs ARTTree.compiler_type)
# 4097 "var/cll/CLLParser.ml"
        ))) = _menhir_stack in
        let _6 = () in
        let _4 = () in
        let _2 = () in
        let _1 = () in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__7_ in
        let _v : (
# 81 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs ARTTree.compiler_type)
# 4108 "var/cll/CLLParser.ml"
        ) = let _startpos = _startpos__1_ in
        
# 361 "var/cll/CLLParser.mly"
    (
      raise_syntax_error _startpos "Ill-formed 'for' loop"
    )
# 4115 "var/cll/CLLParser.ml"
         in
        _menhir_goto_control _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv212)) : 'freshtv214)
    | MenhirState114 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv217 * _menhir_state * Lexing.position)) * _menhir_state * (
# 82 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 4123 "var/cll/CLLParser.ml"
        )) * Lexing.position) * _menhir_state * (
# 82 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 4127 "var/cll/CLLParser.ml"
        ))) * Lexing.position * _menhir_state * (
# 80 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs ARTTree.compiler_type)
# 4131 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv215 * _menhir_state * Lexing.position)) * _menhir_state * (
# 82 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 4137 "var/cll/CLLParser.ml"
        )) * Lexing.position) * _menhir_state * (
# 82 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 4141 "var/cll/CLLParser.ml"
        ))) * Lexing.position * _menhir_state * (
# 80 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs ARTTree.compiler_type)
# 4145 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (((((_menhir_stack, _menhir_s, _startpos__1_), _, (_3 : (
# 82 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 4150 "var/cll/CLLParser.ml"
        ))), _endpos__4_), _, (_5 : (
# 82 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 4154 "var/cll/CLLParser.ml"
        ))), _endpos__7_, _, (_7 : (
# 80 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs ARTTree.compiler_type)
# 4158 "var/cll/CLLParser.ml"
        ))) = _menhir_stack in
        let _6 = () in
        let _4 = () in
        let _2 = () in
        let _1 = () in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__7_ in
        let _v : (
# 81 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs ARTTree.compiler_type)
# 4169 "var/cll/CLLParser.ml"
        ) = let _startpos = _startpos__1_ in
        
# 361 "var/cll/CLLParser.mly"
    (
      raise_syntax_error _startpos "Ill-formed 'for' loop"
    )
# 4176 "var/cll/CLLParser.ml"
         in
        _menhir_goto_control _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv216)) : 'freshtv218)
    | MenhirState116 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv221 * _menhir_state * Lexing.position)) * _menhir_state * (
# 82 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 4184 "var/cll/CLLParser.ml"
        ))) * Lexing.position * _menhir_state * (
# 80 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs ARTTree.compiler_type)
# 4188 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv219 * _menhir_state * Lexing.position)) * _menhir_state * (
# 82 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 4194 "var/cll/CLLParser.ml"
        ))) * Lexing.position * _menhir_state * (
# 80 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs ARTTree.compiler_type)
# 4198 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s, _startpos__1_), _, (_3 : (
# 82 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 4203 "var/cll/CLLParser.ml"
        ))), _endpos__5_, _, (_5 : (
# 80 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs ARTTree.compiler_type)
# 4207 "var/cll/CLLParser.ml"
        ))) = _menhir_stack in
        let _4 = () in
        let _2 = () in
        let _1 = () in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__5_ in
        let _v : (
# 81 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs ARTTree.compiler_type)
# 4217 "var/cll/CLLParser.ml"
        ) = let _startpos = _startpos__1_ in
        
# 361 "var/cll/CLLParser.mly"
    (
      raise_syntax_error _startpos "Ill-formed 'for' loop"
    )
# 4224 "var/cll/CLLParser.ml"
         in
        _menhir_goto_control _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv220)) : 'freshtv222)
    | MenhirState65 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv229 * _menhir_state * Lexing.position)) * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 4232 "var/cll/CLLParser.ml"
        ))) * Lexing.position * _menhir_state * (
# 80 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs ARTTree.compiler_type)
# 4236 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ELSE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv223 * _menhir_state * Lexing.position)) * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 4246 "var/cll/CLLParser.ml"
            ))) * Lexing.position * _menhir_state * (
# 80 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs ARTTree.compiler_type)
# 4250 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BREAK ->
                _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | CONTINUE ->
                _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EXIT ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOR ->
                _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IF ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LABEL _v ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState119
            | MULT ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NOP ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | PRINT ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RETURN ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | WHILE ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState119) : 'freshtv224)
        | BREAK | COLON | CONTINUE | DATA | EOF | EXIT | FOR | IF | LABEL _ | MULT | NOP | PRINT | RB | RETURN | SEMI | WHILE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv225 * _menhir_state * Lexing.position)) * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 4288 "var/cll/CLLParser.ml"
            ))) * Lexing.position * _menhir_state * (
# 80 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs ARTTree.compiler_type)
# 4292 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, _startpos__1_), _, (e : (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 4297 "var/cll/CLLParser.ml"
            ))), _endpos_b_, _, (b : (
# 80 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs ARTTree.compiler_type)
# 4301 "var/cll/CLLParser.ml"
            ))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos_b_ in
            let _v : (
# 81 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs ARTTree.compiler_type)
# 4311 "var/cll/CLLParser.ml"
            ) = 
# 324 "var/cll/CLLParser.mly"
    (
      let syntax_tree = Cycle.from_elt (If(e, b.syntax_tree)) in
      {syntax_tree; tag_set = b.tag_set}
    )
# 4318 "var/cll/CLLParser.ml"
             in
            _menhir_goto_control _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv226)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv227 * _menhir_state * Lexing.position)) * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 4328 "var/cll/CLLParser.ml"
            ))) * Lexing.position * _menhir_state * (
# 80 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs ARTTree.compiler_type)
# 4332 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv228)) : 'freshtv230)
    | MenhirState119 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv233 * _menhir_state * Lexing.position)) * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 4341 "var/cll/CLLParser.ml"
        ))) * Lexing.position * _menhir_state * (
# 80 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs ARTTree.compiler_type)
# 4345 "var/cll/CLLParser.ml"
        ))) * Lexing.position * _menhir_state * (
# 80 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs ARTTree.compiler_type)
# 4349 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv231 * _menhir_state * Lexing.position)) * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 4355 "var/cll/CLLParser.ml"
        ))) * Lexing.position * _menhir_state * (
# 80 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs ARTTree.compiler_type)
# 4359 "var/cll/CLLParser.ml"
        ))) * Lexing.position * _menhir_state * (
# 80 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs ARTTree.compiler_type)
# 4363 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((((_menhir_stack, _menhir_s, _startpos__1_), _, (c : (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 4368 "var/cll/CLLParser.ml"
        ))), _endpos_t_, _, (t : (
# 80 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs ARTTree.compiler_type)
# 4372 "var/cll/CLLParser.ml"
        ))), _endpos_e_, _, (e : (
# 80 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs ARTTree.compiler_type)
# 4376 "var/cll/CLLParser.ml"
        ))) = _menhir_stack in
        let _6 = () in
        let _4 = () in
        let _2 = () in
        let _1 = () in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_e_ in
        let _v : (
# 81 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs ARTTree.compiler_type)
# 4387 "var/cll/CLLParser.ml"
        ) = let _startpos = _startpos__1_ in
        
# 330 "var/cll/CLLParser.mly"
    ( 
      try
        let syntax_tree = Cycle.from_elt (IfElse(c, t.syntax_tree, e.syntax_tree)) in
        let tag_set = union t.tag_set e.tag_set in
        {tag_set; syntax_tree}
      with
      | DuplicateElement t ->
        raise_duplicate_element _startpos t
    )
# 4400 "var/cll/CLLParser.ml"
         in
        _menhir_goto_control _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv232)) : 'freshtv234)
    | MenhirState58 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv237 * _menhir_state * Lexing.position) * _menhir_state) * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 4408 "var/cll/CLLParser.ml"
        ))) * Lexing.position * _menhir_state * (
# 80 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs ARTTree.compiler_type)
# 4412 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv235 * _menhir_state * Lexing.position) * _menhir_state) * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 4418 "var/cll/CLLParser.ml"
        ))) * Lexing.position * _menhir_state * (
# 80 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs ARTTree.compiler_type)
# 4422 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((((_menhir_stack, _menhir_s, _startpos__1_), _), _, (e : (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 4427 "var/cll/CLLParser.ml"
        ))), _endpos_b_, _, (b : (
# 80 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs ARTTree.compiler_type)
# 4431 "var/cll/CLLParser.ml"
        ))) = _menhir_stack in
        let _4 = () in
        let _2 = () in
        let _1 = () in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_b_ in
        let _v : (
# 81 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs ARTTree.compiler_type)
# 4441 "var/cll/CLLParser.ml"
        ) = 
# 341 "var/cll/CLLParser.mly"
    (
      let syntax_tree = Cycle.from_elt (While(e, b.syntax_tree)) in
      {syntax_tree; tag_set = b.tag_set}
    )
# 4448 "var/cll/CLLParser.ml"
         in
        _menhir_goto_control _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv236)) : 'freshtv238)
    | MenhirState5 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv241 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * (
# 80 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs ARTTree.compiler_type)
# 4456 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv239 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * (
# 80 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs ARTTree.compiler_type)
# 4462 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _startpos__1_), _endpos__2_, _, (_2 : (
# 80 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs ARTTree.compiler_type)
# 4467 "var/cll/CLLParser.ml"
        ))) = _menhir_stack in
        let _1 = () in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__2_ in
        let _v : (
# 81 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs ARTTree.compiler_type)
# 4475 "var/cll/CLLParser.ml"
        ) = let _startpos = _startpos__1_ in
        
# 353 "var/cll/CLLParser.mly"
    (
      raise_syntax_error _startpos "No condition found for 'while'"
    )
# 4482 "var/cll/CLLParser.ml"
         in
        _menhir_goto_control _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv240)) : 'freshtv242)
    | MenhirState4 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv249 * _menhir_state * (
# 70 "var/cll/CLLParser.mly"
       (string)
# 4490 "var/cll/CLLParser.ml"
        ) * Lexing.position))) * Lexing.position * _menhir_state * (
# 80 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs ARTTree.compiler_type)
# 4494 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv247 * _menhir_state * (
# 70 "var/cll/CLLParser.mly"
       (string)
# 4500 "var/cll/CLLParser.ml"
        ) * Lexing.position))) * Lexing.position * _menhir_state * (
# 80 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs ARTTree.compiler_type)
# 4504 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (name : (
# 70 "var/cll/CLLParser.mly"
       (string)
# 4509 "var/cll/CLLParser.ml"
        )), _startpos_name_), _endpos_b_, _, (b : (
# 80 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs ARTTree.compiler_type)
# 4513 "var/cll/CLLParser.ml"
        ))) = _menhir_stack in
        let _3 = () in
        let _2 = () in
        let _startpos = _startpos_name_ in
        let _endpos = _endpos_b_ in
        let _v : 'tv_procedure_declaration = let _startpos = _startpos_name_ in
        
# 130 "var/cll/CLLParser.mly"
  (
    {line = get_line _startpos; column = get_column _startpos; contents = name}, b
  )
# 4525 "var/cll/CLLParser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv245) = _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_procedure_declaration) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv243 * Lexing.position * _menhir_state * 'tv_procedure_declaration * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LABEL _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | DATA | EOF ->
            _menhir_reduce68 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState140) : 'freshtv244)) : 'freshtv246)) : 'freshtv248)) : 'freshtv250)
    | _ ->
        _menhir_fail ()

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_reduce26 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (
# 83 "var/cll/CLLParser.mly"
      (ARTTree.datas ARTTree.compiler_type)
# 4560 "var/cll/CLLParser.ml"
    ) = 
# 389 "var/cll/CLLParser.mly"
    ( {syntax_tree = Cycle.empty_cycle; tag_set = empty} )
# 4564 "var/cll/CLLParser.ml"
     in
    _menhir_goto_data_declarations _menhir_env _menhir_stack _menhir_s _v

and _menhir_run145 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 70 "var/cll/CLLParser.mly"
       (string)
# 4571 "var/cll/CLLParser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COLON ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv195 * _menhir_state * (
# 70 "var/cll/CLLParser.mly"
       (string)
# 4583 "var/cll/CLLParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | INT _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv191 * _menhir_state * (
# 70 "var/cll/CLLParser.mly"
       (string)
# 4593 "var/cll/CLLParser.ml"
            ) * Lexing.position)) = Obj.magic _menhir_stack in
            let (_v : (
# 69 "var/cll/CLLParser.mly"
       (int)
# 4598 "var/cll/CLLParser.ml"
            )) = _v in
            ((let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LABEL _v ->
                _menhir_run145 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EOF ->
                _menhir_reduce26 _menhir_env (Obj.magic _menhir_stack) MenhirState147
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState147) : 'freshtv192)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv193 * _menhir_state * (
# 70 "var/cll/CLLParser.mly"
       (string)
# 4619 "var/cll/CLLParser.ml"
            ) * Lexing.position)) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv194)) : 'freshtv196)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv197 * _menhir_state * (
# 70 "var/cll/CLLParser.mly"
       (string)
# 4630 "var/cll/CLLParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv198)

and _menhir_reduce65 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (
# 75 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs ARTTree.compiler_type)
# 4640 "var/cll/CLLParser.ml"
    ) = 
# 173 "var/cll/CLLParser.mly"
    ( {syntax_tree = Cycle.empty_cycle; tag_set = empty} )
# 4644 "var/cll/CLLParser.ml"
     in
    _menhir_goto_instructions _menhir_env _menhir_stack _menhir_s _v

and _menhir_run60 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 70 "var/cll/CLLParser.mly"
       (string)
# 4651 "var/cll/CLLParser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BREAK ->
        _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CONTINUE ->
        _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | EXIT ->
        _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FOR ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IF ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LABEL _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | MULT ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOP ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | PRINT ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RETURN ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | SEMI ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv189 * _menhir_state * (
# 70 "var/cll/CLLParser.mly"
       (string)
# 4685 "var/cll/CLLParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        let (_menhir_s : _menhir_state) = MenhirState60 in
        ((let _menhir_stack = (_menhir_stack, _endpos, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BREAK ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | CONTINUE ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | EXIT ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | FOR ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | IF ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LABEL _v ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MULT ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NOP ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | PRINT ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | RETURN ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | WHILE ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | RB ->
            _menhir_reduce65 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState61) : 'freshtv190)
    | WHILE ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | ADDASSIGN | ASSIGN | DECR | DIVASSIGN | INCR | MULTASSIGN | SUBASSIGN ->
        _menhir_reduce66 _menhir_env (Obj.magic _menhir_stack)
    | RB ->
        _menhir_reduce65 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState60

and _menhir_reduce66 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 70 "var/cll/CLLParser.mly"
       (string)
# 4735 "var/cll/CLLParser.ml"
) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (t : (
# 70 "var/cll/CLLParser.mly"
       (string)
# 4741 "var/cll/CLLParser.ml"
    )), _startpos_t_) = _menhir_stack in
    let _startpos = _startpos_t_ in
    let _v : (
# 77 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 4747 "var/cll/CLLParser.ml"
    ) = let _startpos = _startpos_t_ in
    
# 225 "var/cll/CLLParser.mly"
    ( Id (make_node _startpos t) )
# 4752 "var/cll/CLLParser.ml"
     in
    _menhir_goto_l_expr _menhir_env _menhir_stack _menhir_s _v _startpos

and _menhir_run67 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 70 "var/cll/CLLParser.mly"
       (string)
# 4759 "var/cll/CLLParser.ml"
) * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LABEL _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MULT ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState67

and _menhir_run9 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BOOL _v ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
    | CPL ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | INT _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
    | LABEL _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | MULT ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | STACKPOINTER ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | SUB ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState9

and _menhir_run10 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv187) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 4816 "var/cll/CLLParser.ml"
    ) = 
# 184 "var/cll/CLLParser.mly"
    ( StackPointer )
# 4820 "var/cll/CLLParser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv188)

and _menhir_run11 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BOOL _v ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v
    | CPL ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | INT _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v
    | LABEL _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | MULT ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | STACKPOINTER ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | SUB ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState11

and _menhir_run15 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BOOL _v ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
    | CPL ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | INT _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
    | LABEL _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | MULT ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | STACKPOINTER ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | SUB ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState15

and _menhir_run13 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 70 "var/cll/CLLParser.mly"
       (string)
# 4889 "var/cll/CLLParser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce66 _menhir_env (Obj.magic _menhir_stack)

and _menhir_run16 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 69 "var/cll/CLLParser.mly"
       (int)
# 4899 "var/cll/CLLParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv185) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((i : (
# 69 "var/cll/CLLParser.mly"
       (int)
# 4909 "var/cll/CLLParser.ml"
    )) : (
# 69 "var/cll/CLLParser.mly"
       (int)
# 4913 "var/cll/CLLParser.ml"
    )) = _v in
    ((let _v : (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 4918 "var/cll/CLLParser.ml"
    ) = 
# 178 "var/cll/CLLParser.mly"
    ( Int i )
# 4922 "var/cll/CLLParser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv186)

and _menhir_run17 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BOOL _v ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v
    | CPL ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | INT _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v
    | LABEL _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | MULT ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | STACKPOINTER ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | SUB ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState17

and _menhir_run18 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 68 "var/cll/CLLParser.mly"
       (bool)
# 4960 "var/cll/CLLParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv183) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((b : (
# 68 "var/cll/CLLParser.mly"
       (bool)
# 4970 "var/cll/CLLParser.ml"
    )) : (
# 68 "var/cll/CLLParser.mly"
       (bool)
# 4974 "var/cll/CLLParser.ml"
    )) = _v in
    ((let _v : (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 4979 "var/cll/CLLParser.ml"
    ) = 
# 180 "var/cll/CLLParser.mly"
    ( Bool b )
# 4983 "var/cll/CLLParser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv184)

and _menhir_run19 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LABEL _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv179 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        let (_v : (
# 70 "var/cll/CLLParser.mly"
       (string)
# 4999 "var/cll/CLLParser.ml"
        )) = _v in
        let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv177 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        let ((t : (
# 70 "var/cll/CLLParser.mly"
       (string)
# 5008 "var/cll/CLLParser.ml"
        )) : (
# 70 "var/cll/CLLParser.mly"
       (string)
# 5012 "var/cll/CLLParser.ml"
        )) = _v in
        let (_startpos_t_ : Lexing.position) = _startpos in
        ((let (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
        let _1 = () in
        let _v : (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 5020 "var/cll/CLLParser.ml"
        ) = let _startpos = _startpos__1_ in
        
# 220 "var/cll/CLLParser.mly"
    ( Address (make_node _startpos t) )
# 5025 "var/cll/CLLParser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv178)) : 'freshtv180)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv181 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv182)

and _menhir_goto_instruction : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 78 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 5039 "var/cll/CLLParser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState4 | MenhirState5 | MenhirState58 | MenhirState119 | MenhirState65 | MenhirState116 | MenhirState114 | MenhirState111 | MenhirState109 | MenhirState102 | MenhirState87 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv167 * _menhir_state * (
# 78 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 5049 "var/cll/CLLParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv163 * _menhir_state * (
# 78 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 5059 "var/cll/CLLParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv161 * _menhir_state * (
# 78 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 5067 "var/cll/CLLParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos__2_ : Lexing.position) = _endpos in
            ((let (_menhir_stack, _menhir_s, (i : (
# 78 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 5073 "var/cll/CLLParser.ml"
            )), _startpos_i_) = _menhir_stack in
            let _2 = () in
            let _endpos = _endpos__2_ in
            let _v : (
# 80 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs ARTTree.compiler_type)
# 5080 "var/cll/CLLParser.ml"
            ) = 
# 310 "var/cll/CLLParser.mly"
    (
      (* Pour l'instant, une déclaration d'étiquette suivie d'une seule instruction, sans {}, n'est pas autorisée *) 
      {syntax_tree = Cycle.from_elt i; tag_set = empty} 
    )
# 5087 "var/cll/CLLParser.ml"
             in
            _menhir_goto_block _menhir_env _menhir_stack _endpos _menhir_s _v) : 'freshtv162)) : 'freshtv164)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv165 * _menhir_state * (
# 78 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 5097 "var/cll/CLLParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv166)) : 'freshtv168)
    | MenhirState59 | MenhirState60 | MenhirState131 | MenhirState125 | MenhirState128 | MenhirState126 | MenhirState123 | MenhirState61 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv175 * _menhir_state * (
# 78 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 5106 "var/cll/CLLParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv169 * _menhir_state * (
# 78 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 5116 "var/cll/CLLParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BREAK ->
                _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState131 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | CONTINUE ->
                _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState131 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EXIT ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState131 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOR ->
                _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState131 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IF ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState131 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LABEL _v ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState131 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MULT ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState131 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NOP ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState131 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | PRINT ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState131 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RETURN ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState131 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | WHILE ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState131 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RB ->
                _menhir_reduce65 _menhir_env (Obj.magic _menhir_stack) MenhirState131
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState131) : 'freshtv170)
        | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv171 * _menhir_state * (
# 78 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 5154 "var/cll/CLLParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_stack = (_menhir_stack, _endpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BREAK ->
                _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | CONTINUE ->
                _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EXIT ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOR ->
                _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IF ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LABEL _v ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MULT ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NOP ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | PRINT ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RETURN ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | WHILE ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RB ->
                _menhir_reduce65 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState123) : 'freshtv172)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv173 * _menhir_state * (
# 78 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 5196 "var/cll/CLLParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv174)) : 'freshtv176)
    | _ ->
        _menhir_fail ()

and _menhir_goto_program : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 74 "var/cll/CLLParser.mly"
      (CLLTree.cll_prog ARTTree.compiler_type)
# 5206 "var/cll/CLLParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv159) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : (
# 74 "var/cll/CLLParser.mly"
      (CLLTree.cll_prog ARTTree.compiler_type)
# 5215 "var/cll/CLLParser.ml"
    )) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv157) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : (
# 74 "var/cll/CLLParser.mly"
      (CLLTree.cll_prog ARTTree.compiler_type)
# 5223 "var/cll/CLLParser.ml"
    )) : (
# 74 "var/cll/CLLParser.mly"
      (CLLTree.cll_prog ARTTree.compiler_type)
# 5227 "var/cll/CLLParser.ml"
    )) = _v in
    (Obj.magic _1 : 'freshtv158)) : 'freshtv160)

and _menhir_goto_list_procedure_declaration_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_list_procedure_declaration_ -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState140 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv145 * Lexing.position * _menhir_state * 'tv_procedure_declaration * Lexing.position) * _menhir_state * 'tv_list_procedure_declaration_ * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv143 * Lexing.position * _menhir_state * 'tv_procedure_declaration * Lexing.position) * _menhir_state * 'tv_list_procedure_declaration_ * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _endpos_x_, _menhir_s, (x : 'tv_procedure_declaration), _startpos_x_), _, (xs : 'tv_list_procedure_declaration_), _startpos_xs_) = _menhir_stack in
        let _startpos = _startpos_x_ in
        let _v : 'tv_list_procedure_declaration_ = 
# 201 "/home/fusroman/.opam/4.06.0/lib/menhir/standard.mly"
    ( x :: xs )
# 5245 "var/cll/CLLParser.ml"
         in
        _menhir_goto_list_procedure_declaration_ _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv144)) : 'freshtv146)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv155 * _menhir_state * 'tv_list_procedure_declaration_ * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DATA ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv147 * _menhir_state * 'tv_list_procedure_declaration_ * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LABEL _v ->
                _menhir_run145 _menhir_env (Obj.magic _menhir_stack) MenhirState144 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EOF ->
                _menhir_reduce26 _menhir_env (Obj.magic _menhir_stack) MenhirState144
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState144) : 'freshtv148)
        | EOF ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv151 * _menhir_state * 'tv_list_procedure_declaration_ * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv149 * _menhir_state * 'tv_list_procedure_declaration_ * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (globals : 'tv_list_procedure_declaration_), _startpos_globals_) = _menhir_stack in
            let _2 = () in
            let _v : (
# 74 "var/cll/CLLParser.mly"
      (CLLTree.cll_prog ARTTree.compiler_type)
# 5278 "var/cll/CLLParser.ml"
            ) = let _startpos = _startpos_globals_ in
            
# 113 "var/cll/CLLParser.mly"
    (
      try
        let (tag_set,syntax_tree) = List.fold_left make_compiler_type (empty,Cycle.empty_cycle) globals in
        let syntax_tree = Procedure_Definition syntax_tree in
        {tag_set; syntax_tree}
      with
      | DuplicateElement t ->
        raise_duplicate_element _startpos t
    )
# 5291 "var/cll/CLLParser.ml"
             in
            _menhir_goto_program _menhir_env _menhir_stack _menhir_s _v) : 'freshtv150)) : 'freshtv152)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv153 * _menhir_state * 'tv_list_procedure_declaration_ * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv154)) : 'freshtv156)
    | _ ->
        _menhir_fail ()

and _menhir_run5 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BREAK ->
        _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CONTINUE ->
        _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | EXIT ->
        _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FOR ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IF ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LABEL _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | LP ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv141 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState5 in
        ((let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADDRESS ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | BOOL _v ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
        | CPL ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | INT _v ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
        | LABEL _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LP ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | MULT ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NOT ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | STACKPOINTER ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | SUB ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState56) : 'freshtv142)
    | MULT ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOP ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | PRINT ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RETURN ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | WHILE ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState5

and _menhir_run6 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv139) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_startpos__1_ : Lexing.position) = _startpos in
    ((let _1 = () in
    let _startpos = _startpos__1_ in
    let _v : (
# 78 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 5383 "var/cll/CLLParser.ml"
    ) = 
# 248 "var/cll/CLLParser.mly"
    (
      Return
    )
# 5389 "var/cll/CLLParser.ml"
     in
    _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv140)

and _menhir_run7 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LP ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv135 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADDRESS ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | BOOL _v ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
        | CPL ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState8
        | INT _v ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
        | LABEL _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LP ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState8
        | MULT ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NOT ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState8
        | STACKPOINTER ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState8
        | SUB ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState8
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState8) : 'freshtv136)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv137 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv138)

and _menhir_run55 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv133) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_startpos__1_ : Lexing.position) = _startpos in
    ((let _1 = () in
    let _startpos = _startpos__1_ in
    let _v : (
# 78 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 5449 "var/cll/CLLParser.ml"
    ) = 
# 232 "var/cll/CLLParser.mly"
    ( Nop )
# 5453 "var/cll/CLLParser.ml"
     in
    _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv134)

and _menhir_run12 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LABEL _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MULT ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState12

and _menhir_run59 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BREAK ->
        _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CONTINUE ->
        _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | EXIT ->
        _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FOR ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IF ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LABEL _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MULT ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOP ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | PRINT ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RETURN ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | WHILE ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RB ->
        _menhir_reduce65 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState59

and _menhir_run66 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 70 "var/cll/CLLParser.mly"
       (string)
# 5510 "var/cll/CLLParser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LP ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | ADDASSIGN | ASSIGN | DECR | DIVASSIGN | INCR | MULTASSIGN | SUBASSIGN ->
        _menhir_reduce66 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState66

and _menhir_run62 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LP ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv129 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADDRESS ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | BOOL _v ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
        | CPL ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | INT _v ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
        | LABEL _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LP ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | MULT ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NOT ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | STACKPOINTER ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | SUB ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState63) : 'freshtv130)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv131 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv132)

and _menhir_run70 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LP ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv125 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADDRESS ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | BOOL _v ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
        | CPL ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | INT _v ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
        | LABEL _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LP ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | MULT ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NOT ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | STACKPOINTER ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | SUB ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState71) : 'freshtv126)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv127 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv128)

and _menhir_run88 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv123) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_startpos__1_ : Lexing.position) = _startpos in
    ((let _1 = () in
    let _startpos = _startpos__1_ in
    let _v : (
# 78 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 5626 "var/cll/CLLParser.ml"
    ) = 
# 234 "var/cll/CLLParser.mly"
    ( Exit )
# 5630 "var/cll/CLLParser.ml"
     in
    _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv124)

and _menhir_run89 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv121) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_startpos__1_ : Lexing.position) = _startpos in
    ((let _1 = () in
    let _startpos = _startpos__1_ in
    let _v : (
# 78 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 5646 "var/cll/CLLParser.ml"
    ) = let _startpos = _startpos__1_ in
    
# 243 "var/cll/CLLParser.mly"
    ( 
      Continue (make_node _startpos ())
    )
# 5653 "var/cll/CLLParser.ml"
     in
    _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv122)

and _menhir_run90 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv119) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_startpos__1_ : Lexing.position) = _startpos in
    ((let _1 = () in
    let _startpos = _startpos__1_ in
    let _v : (
# 78 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 5669 "var/cll/CLLParser.ml"
    ) = let _startpos = _startpos__1_ in
    
# 238 "var/cll/CLLParser.mly"
    ( 
      Break (make_node _startpos ())
    )
# 5676 "var/cll/CLLParser.ml"
     in
    _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv120)

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState147 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv11 * _menhir_state * (
# 70 "var/cll/CLLParser.mly"
       (string)
# 5688 "var/cll/CLLParser.ml"
        ) * Lexing.position)) * (
# 69 "var/cll/CLLParser.mly"
       (int)
# 5692 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv12)
    | MenhirState144 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv13 * _menhir_state * 'tv_list_procedure_declaration_ * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv14)
    | MenhirState140 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv15 * Lexing.position * _menhir_state * 'tv_procedure_declaration * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv16)
    | MenhirState131 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv17 * _menhir_state * (
# 78 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 5711 "var/cll/CLLParser.ml"
        ) * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv18)
    | MenhirState128 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv19 * Lexing.position * _menhir_state * (
# 81 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs ARTTree.compiler_type)
# 5720 "var/cll/CLLParser.ml"
        ) * Lexing.position) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv20)
    | MenhirState126 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv21 * Lexing.position * _menhir_state * (
# 81 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs ARTTree.compiler_type)
# 5729 "var/cll/CLLParser.ml"
        ) * Lexing.position) * Lexing.position * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv22)
    | MenhirState125 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv23 * Lexing.position * _menhir_state * (
# 81 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs ARTTree.compiler_type)
# 5738 "var/cll/CLLParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv24)
    | MenhirState123 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv25 * _menhir_state * (
# 78 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 5747 "var/cll/CLLParser.ml"
        ) * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv26)
    | MenhirState119 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv27 * _menhir_state * Lexing.position)) * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 5756 "var/cll/CLLParser.ml"
        ))) * Lexing.position * _menhir_state * (
# 80 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs ARTTree.compiler_type)
# 5760 "var/cll/CLLParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv28)
    | MenhirState116 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv29 * _menhir_state * Lexing.position)) * _menhir_state * (
# 82 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 5769 "var/cll/CLLParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv30)
    | MenhirState114 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv31 * _menhir_state * Lexing.position)) * _menhir_state * (
# 82 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 5778 "var/cll/CLLParser.ml"
        )) * Lexing.position) * _menhir_state * (
# 82 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 5782 "var/cll/CLLParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv32)
    | MenhirState111 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv33 * _menhir_state * Lexing.position)) * _menhir_state * (
# 82 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 5791 "var/cll/CLLParser.ml"
        )) * Lexing.position) * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 5795 "var/cll/CLLParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv34)
    | MenhirState109 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((('freshtv35 * _menhir_state * Lexing.position)) * _menhir_state * (
# 82 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 5804 "var/cll/CLLParser.ml"
        )) * Lexing.position) * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 5808 "var/cll/CLLParser.ml"
        )) * Lexing.position) * _menhir_state * (
# 82 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 5812 "var/cll/CLLParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv36)
    | MenhirState107 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv37 * _menhir_state * Lexing.position)) * _menhir_state * (
# 82 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 5821 "var/cll/CLLParser.ml"
        )) * Lexing.position) * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 5825 "var/cll/CLLParser.ml"
        )) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv38)
    | MenhirState105 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv39 * _menhir_state * Lexing.position)) * _menhir_state * (
# 82 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 5834 "var/cll/CLLParser.ml"
        )) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv40)
    | MenhirState102 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv41 * _menhir_state * Lexing.position)) * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 5843 "var/cll/CLLParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv42)
    | MenhirState100 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv43 * _menhir_state * (
# 79 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 5852 "var/cll/CLLParser.ml"
        ) * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv44)
    | MenhirState87 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv45 * _menhir_state * Lexing.position)) * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 5861 "var/cll/CLLParser.ml"
        )) * Lexing.position) * _menhir_state * (
# 82 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 5865 "var/cll/CLLParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv46)
    | MenhirState84 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv47 * _menhir_state * Lexing.position)) * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 5874 "var/cll/CLLParser.ml"
        )) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv48)
    | MenhirState81 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv49 * _menhir_state * (
# 77 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 5883 "var/cll/CLLParser.ml"
        ) * Lexing.position) * 'tv_assign_binop) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv50)
    | MenhirState71 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv51 * _menhir_state * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv52)
    | MenhirState67 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv53 * _menhir_state * (
# 70 "var/cll/CLLParser.mly"
       (string)
# 5897 "var/cll/CLLParser.ml"
        ) * Lexing.position) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv54)
    | MenhirState66 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv55 * _menhir_state * (
# 70 "var/cll/CLLParser.mly"
       (string)
# 5906 "var/cll/CLLParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv56)
    | MenhirState65 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv57 * _menhir_state * Lexing.position)) * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 5915 "var/cll/CLLParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv58)
    | MenhirState63 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv59 * _menhir_state * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv60)
    | MenhirState61 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv61 * _menhir_state * (
# 70 "var/cll/CLLParser.mly"
       (string)
# 5929 "var/cll/CLLParser.ml"
        ) * Lexing.position) * Lexing.position * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv62)
    | MenhirState60 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv63 * _menhir_state * (
# 70 "var/cll/CLLParser.mly"
       (string)
# 5938 "var/cll/CLLParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv64)
    | MenhirState59 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv65 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv66)
    | MenhirState58 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv67 * _menhir_state * Lexing.position) * _menhir_state) * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 5952 "var/cll/CLLParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv68)
    | MenhirState56 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv69 * _menhir_state * Lexing.position) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv70)
    | MenhirState49 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv71 * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 5966 "var/cll/CLLParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv72)
    | MenhirState47 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv73 * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 5975 "var/cll/CLLParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv74)
    | MenhirState45 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv75 * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 5984 "var/cll/CLLParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv76)
    | MenhirState43 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv77 * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 5993 "var/cll/CLLParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv78)
    | MenhirState41 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv79 * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 6002 "var/cll/CLLParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv80)
    | MenhirState39 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv81 * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 6011 "var/cll/CLLParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv82)
    | MenhirState37 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv83 * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 6020 "var/cll/CLLParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv84)
    | MenhirState35 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv85 * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 6029 "var/cll/CLLParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv86)
    | MenhirState33 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv87 * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 6038 "var/cll/CLLParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv88)
    | MenhirState30 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv89 * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 6047 "var/cll/CLLParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv90)
    | MenhirState28 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv91 * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 6056 "var/cll/CLLParser.ml"
        )) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv92)
    | MenhirState26 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv93 * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 6065 "var/cll/CLLParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv94)
    | MenhirState24 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv95 * _menhir_state * (
# 76 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 6074 "var/cll/CLLParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv96)
    | MenhirState17 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv97 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv98)
    | MenhirState15 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv99 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv100)
    | MenhirState12 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv101 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv102)
    | MenhirState11 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv103 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv104)
    | MenhirState9 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv105 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv106)
    | MenhirState8 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv107 * _menhir_state * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv108)
    | MenhirState5 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv109 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv110)
    | MenhirState4 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv111 * _menhir_state * (
# 70 "var/cll/CLLParser.mly"
       (string)
# 6118 "var/cll/CLLParser.ml"
        ) * Lexing.position))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv112)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv117) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv115) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState0 in
        let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv113) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_startpos__1_ : Lexing.position) = _startpos in
        ((let _1 = () in
        let _v : (
# 74 "var/cll/CLLParser.mly"
      (CLLTree.cll_prog ARTTree.compiler_type)
# 6137 "var/cll/CLLParser.ml"
        ) = let _startpos = _startpos__1_ in
        
# 123 "var/cll/CLLParser.mly"
    ( 
      raise_syntax_error _startpos "CLL program structure: list procedure declaration .data <declarations>" 
    )
# 6144 "var/cll/CLLParser.ml"
         in
        _menhir_goto_program _menhir_env _menhir_stack _menhir_s _v) : 'freshtv114)) : 'freshtv116)) : 'freshtv118)

and _menhir_reduce68 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let (_, _startpos) = Obj.magic _menhir_stack in
    let _v : 'tv_list_procedure_declaration_ = 
# 199 "/home/fusroman/.opam/4.06.0/lib/menhir/standard.mly"
    ( [] )
# 6154 "var/cll/CLLParser.ml"
     in
    _menhir_goto_list_procedure_declaration_ _menhir_env _menhir_stack _menhir_s _v _startpos

and _menhir_run2 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 70 "var/cll/CLLParser.mly"
       (string)
# 6161 "var/cll/CLLParser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LP ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv7 * _menhir_state * (
# 70 "var/cll/CLLParser.mly"
       (string)
# 6173 "var/cll/CLLParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RP ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv3 * _menhir_state * (
# 70 "var/cll/CLLParser.mly"
       (string)
# 6183 "var/cll/CLLParser.ml"
            ) * Lexing.position)) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BREAK ->
                _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | CONTINUE ->
                _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EXIT ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOR ->
                _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IF ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LABEL _v ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState4
            | MULT ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NOP ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | PRINT ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RETURN ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | WHILE ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState4) : 'freshtv4)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv5 * _menhir_state * (
# 70 "var/cll/CLLParser.mly"
       (string)
# 6223 "var/cll/CLLParser.ml"
            ) * Lexing.position)) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv6)) : 'freshtv8)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv9 * _menhir_state * (
# 70 "var/cll/CLLParser.mly"
       (string)
# 6234 "var/cll/CLLParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv10)

and _menhir_discard : _menhir_env -> _menhir_env =
  fun _menhir_env ->
    let lexer = _menhir_env._menhir_lexer in
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    }

and program : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 74 "var/cll/CLLParser.mly"
      (CLLTree.cll_prog ARTTree.compiler_type)
# 6254 "var/cll/CLLParser.ml"
) =
  fun lexer lexbuf ->
    let _menhir_env =
      let (lexer : Lexing.lexbuf -> token) = lexer in
      let (lexbuf : Lexing.lexbuf) = lexbuf in
      ((let _tok = Obj.magic () in
      {
        _menhir_lexer = lexer;
        _menhir_lexbuf = lexbuf;
        _menhir_token = _tok;
        _menhir_error = false;
      }) : _menhir_env)
    in
    Obj.magic (let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1) = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    ((let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LABEL _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DATA | EOF ->
        _menhir_reduce68 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0) : 'freshtv2))

# 233 "/home/fusroman/.opam/4.06.0/lib/menhir/standard.mly"
  

# 6285 "var/cll/CLLParser.ml"
