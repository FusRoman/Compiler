
module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | WHILE
    | VAR
    | TYPE
    | TSTRING
    | TINT
    | TFUN
    | SUBASSIGN
    | SUB
    | SEMI
    | RS
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
    | LS
    | LP
    | LE
    | LB
    | LABEL of (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 38 "typ/typ/TYPParser.ml"
  )
    | INT of (
# 63 "typ/typ/TYPParser.mly"
       (int)
# 43 "typ/typ/TYPParser.ml"
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
    | DOTSIZE
    | DOT
    | DIVASSIGN
    | DIV
    | DECR
    | CPL
    | CONTINUE
    | COMMA
    | COLON
    | BREAK
    | BOOL of (
# 62 "typ/typ/TYPParser.mly"
       (bool)
# 67 "typ/typ/TYPParser.ml"
  )
    | ASSIGN
    | ARROW
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
  | MenhirState203
  | MenhirState196
  | MenhirState191
  | MenhirState188
  | MenhirState186
  | MenhirState185
  | MenhirState183
  | MenhirState178
  | MenhirState175
  | MenhirState168
  | MenhirState159
  | MenhirState155
  | MenhirState153
  | MenhirState151
  | MenhirState147
  | MenhirState140
  | MenhirState137
  | MenhirState135
  | MenhirState132
  | MenhirState128
  | MenhirState125
  | MenhirState124
  | MenhirState116
  | MenhirState112
  | MenhirState100
  | MenhirState95
  | MenhirState93
  | MenhirState91
  | MenhirState89
  | MenhirState87
  | MenhirState85
  | MenhirState83
  | MenhirState81
  | MenhirState79
  | MenhirState77
  | MenhirState74
  | MenhirState72
  | MenhirState70
  | MenhirState68
  | MenhirState66
  | MenhirState63
  | MenhirState61
  | MenhirState58
  | MenhirState56
  | MenhirState55
  | MenhirState54
  | MenhirState52
  | MenhirState51
  | MenhirState50
  | MenhirState48
  | MenhirState44
  | MenhirState37
  | MenhirState29
  | MenhirState25
  | MenhirState22
  | MenhirState11
  | MenhirState9
  | MenhirState8
  | MenhirState4
  | MenhirState0

# 1 "typ/typ/TYPParser.mly"
  
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


# 196 "typ/typ/TYPParser.ml"

let rec _menhir_goto_control : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 77 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 201 "typ/typ/TYPParser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState124 | MenhirState178 | MenhirState137 | MenhirState155 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv787 * _menhir_state * (
# 77 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 211 "typ/typ/TYPParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv785 * _menhir_state * (
# 77 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 217 "typ/typ/TYPParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (c : (
# 77 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 222 "typ/typ/TYPParser.ml"
        )), _startpos_c_) = _menhir_stack in
        let _v : (
# 76 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 227 "typ/typ/TYPParser.ml"
        ) = 
# 361 "typ/typ/TYPParser.mly"
    ( [c] )
# 231 "typ/typ/TYPParser.ml"
         in
        _menhir_goto_block _menhir_env _menhir_stack _menhir_s _v) : 'freshtv786)) : 'freshtv788)
    | MenhirState48 | MenhirState132 | MenhirState191 | MenhirState185 | MenhirState188 | MenhirState186 | MenhirState183 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv793 * _menhir_state * (
# 77 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 239 "typ/typ/TYPParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BREAK ->
            _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState185 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | COLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv789 * _menhir_state * (
# 77 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 251 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState185 in
            ((let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BREAK ->
                _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | CONTINUE ->
                _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EXIT ->
                _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOR ->
                _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IF ->
                _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LABEL _v ->
                _menhir_run138 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MULT ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NOP ->
                _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | PRINT ->
                _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RETURN ->
                _menhir_run125 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TFUN ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TINT ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TSTRING ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | WHILE ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RB ->
                _menhir_reduce64 _menhir_env (Obj.magic _menhir_stack) MenhirState188
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState188) : 'freshtv790)
        | CONTINUE ->
            _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState185 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | EXIT ->
            _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState185 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | FOR ->
            _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState185 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | IF ->
            _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState185 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LABEL _v ->
            _menhir_run138 _menhir_env (Obj.magic _menhir_stack) MenhirState185 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LB ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState185 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MULT ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState185 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NOP ->
            _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState185 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | PRINT ->
            _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState185 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | RETURN ->
            _menhir_run125 _menhir_env (Obj.magic _menhir_stack) MenhirState185 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv791 * _menhir_state * (
# 77 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 319 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState185 in
            ((let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BREAK ->
                _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState186 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | CONTINUE ->
                _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState186 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EXIT ->
                _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState186 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOR ->
                _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState186 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IF ->
                _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState186 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LABEL _v ->
                _menhir_run138 _menhir_env (Obj.magic _menhir_stack) MenhirState186 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState186 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MULT ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState186 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NOP ->
                _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState186 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | PRINT ->
                _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState186 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RETURN ->
                _menhir_run125 _menhir_env (Obj.magic _menhir_stack) MenhirState186 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TFUN ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState186 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TINT ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState186 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TSTRING ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState186 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | WHILE ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState186 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RB ->
                _menhir_reduce64 _menhir_env (Obj.magic _menhir_stack) MenhirState186
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState186) : 'freshtv792)
        | TFUN ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState185 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TINT ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState185 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TSTRING ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState185 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | WHILE ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState185 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | RB ->
            _menhir_reduce64 _menhir_env (Obj.magic _menhir_stack) MenhirState185
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState185) : 'freshtv794)
    | _ ->
        _menhir_fail ()

and _menhir_goto_assign_unop : _menhir_env -> 'ttv_tail -> 'tv_assign_unop -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv783 * _menhir_state * (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 385 "typ/typ/TYPParser.ml"
    ) * Lexing.position) = Obj.magic _menhir_stack in
    let (_v : 'tv_assign_unop) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv781 * _menhir_state * (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 392 "typ/typ/TYPParser.ml"
    ) * Lexing.position) = Obj.magic _menhir_stack in
    let ((op : 'tv_assign_unop) : 'tv_assign_unop) = _v in
    ((let (_menhir_stack, _menhir_s, (l : (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 398 "typ/typ/TYPParser.ml"
    )), _startpos_l_) = _menhir_stack in
    let _startpos = _startpos_l_ in
    let _v : (
# 75 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 404 "typ/typ/TYPParser.ml"
    ) = 
# 328 "typ/typ/TYPParser.mly"
    (
      UnopAssign(l, op)
    )
# 410 "typ/typ/TYPParser.ml"
     in
    _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv782)) : 'freshtv784)

and _menhir_goto_assign_binop : _menhir_env -> 'ttv_tail -> 'tv_assign_binop -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv779 * _menhir_state * (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 421 "typ/typ/TYPParser.ml"
    ) * Lexing.position) * 'tv_assign_binop) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState168
    | BOOL _v ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState168 _v
    | CPL ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState168
    | INT _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState168 _v
    | LABEL _v ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState168 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState168 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState168
    | MULT ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState168 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState168
    | SUB ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState168
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState168) : 'freshtv780)

and _menhir_reduce71 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_loption_separated_nonempty_list_COMMA_expr__ = 
# 142 "/home/fusroman/.opam/4.06.0/lib/menhir/standard.mly"
    ( [] )
# 456 "typ/typ/TYPParser.ml"
     in
    _menhir_goto_loption_separated_nonempty_list_COMMA_expr__ _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_loption_separated_nonempty_list_COMMA_expr__ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_loption_separated_nonempty_list_COMMA_expr__ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState95 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv769 * _menhir_state * (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 469 "typ/typ/TYPParser.ml"
        ) * Lexing.position)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RP ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv765 * _menhir_state * (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 479 "typ/typ/TYPParser.ml"
            ) * Lexing.position)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv763 * _menhir_state * (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 486 "typ/typ/TYPParser.ml"
            ) * Lexing.position)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (f : (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 491 "typ/typ/TYPParser.ml"
            )), _startpos_f_), _, (xs0 : 'tv_loption_separated_nonempty_list_COMMA_expr__)) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 498 "typ/typ/TYPParser.ml"
            ) = let args =
              let xs = xs0 in
              
# 220 "/home/fusroman/.opam/4.06.0/lib/menhir/standard.mly"
    ( xs )
# 504 "typ/typ/TYPParser.ml"
              
            in
            
# 263 "typ/typ/TYPParser.mly"
    ( Call(f, args) )
# 510 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv764)) : 'freshtv766)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv767 * _menhir_state * (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 520 "typ/typ/TYPParser.ml"
            ) * Lexing.position)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv768)) : 'freshtv770)
    | MenhirState159 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv777 * _menhir_state * (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 529 "typ/typ/TYPParser.ml"
        ) * Lexing.position)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RP ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv773 * _menhir_state * (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 539 "typ/typ/TYPParser.ml"
            ) * Lexing.position)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv771 * _menhir_state * (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 546 "typ/typ/TYPParser.ml"
            ) * Lexing.position)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (f : (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 551 "typ/typ/TYPParser.ml"
            )), _startpos_f_), _, (xs0 : 'tv_loption_separated_nonempty_list_COMMA_expr__)) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _startpos = _startpos_f_ in
            let _v : (
# 75 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 559 "typ/typ/TYPParser.ml"
            ) = let args =
              let xs = xs0 in
              
# 220 "/home/fusroman/.opam/4.06.0/lib/menhir/standard.mly"
    ( xs )
# 565 "typ/typ/TYPParser.ml"
              
            in
            
# 333 "typ/typ/TYPParser.mly"
    (
      Call(f, args) 
    )
# 573 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv772)) : 'freshtv774)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv775 * _menhir_state * (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 583 "typ/typ/TYPParser.ml"
            ) * Lexing.position)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv776)) : 'freshtv778)
    | _ ->
        _menhir_fail ()

and _menhir_goto_loption_separated_nonempty_list_COMMA_instruction__ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_loption_separated_nonempty_list_COMMA_instruction__ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState140 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv755 * _menhir_state * Lexing.position)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_instruction__) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv751 * _menhir_state * Lexing.position)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_instruction__) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ADDRESS ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState151
            | BOOL _v ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _v
            | CPL ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState151
            | INT _v ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _v
            | LABEL _v ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LP ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState151
            | MULT ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NOT ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState151
            | SUB ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState151
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState151) : 'freshtv752)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv753 * _menhir_state * Lexing.position)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_instruction__) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv754)) : 'freshtv756)
    | MenhirState153 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv761 * _menhir_state * Lexing.position)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_instruction__)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 642 "typ/typ/TYPParser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_instruction__) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RP ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv757 * _menhir_state * Lexing.position)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_instruction__)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 652 "typ/typ/TYPParser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_instruction__) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BREAK ->
                _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | CONTINUE ->
                _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EXIT ->
                _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOR ->
                _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IF ->
                _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LABEL _v ->
                _menhir_run138 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB ->
                _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MULT ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NOP ->
                _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | PRINT ->
                _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RETURN ->
                _menhir_run125 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TFUN ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TINT ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TSTRING ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | WHILE ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState155) : 'freshtv758)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv759 * _menhir_state * Lexing.position)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_instruction__)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 698 "typ/typ/TYPParser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_instruction__) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv760)) : 'freshtv762)
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_COMMA_instruction_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_COMMA_instruction_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState153 | MenhirState140 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv745) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_COMMA_instruction_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv743) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((x : 'tv_separated_nonempty_list_COMMA_instruction_) : 'tv_separated_nonempty_list_COMMA_instruction_) = _v in
        ((let _v : 'tv_loption_separated_nonempty_list_COMMA_instruction__ = 
# 144 "/home/fusroman/.opam/4.06.0/lib/menhir/standard.mly"
    ( x )
# 720 "typ/typ/TYPParser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_COMMA_instruction__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv744)) : 'freshtv746)
    | MenhirState175 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv749 * _menhir_state * (
# 75 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 728 "typ/typ/TYPParser.ml"
        ) * Lexing.position)) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_COMMA_instruction_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv747 * _menhir_state * (
# 75 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 736 "typ/typ/TYPParser.ml"
        ) * Lexing.position)) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((xs : 'tv_separated_nonempty_list_COMMA_instruction_) : 'tv_separated_nonempty_list_COMMA_instruction_) = _v in
        ((let (_menhir_stack, _menhir_s, (x : (
# 75 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 743 "typ/typ/TYPParser.ml"
        )), _startpos_x_) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_separated_nonempty_list_COMMA_instruction_ = 
# 231 "/home/fusroman/.opam/4.06.0/lib/menhir/standard.mly"
    ( x :: xs )
# 749 "typ/typ/TYPParser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_instruction_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv748)) : 'freshtv750)
    | _ ->
        _menhir_fail ()

and _menhir_goto_block : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 76 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 758 "typ/typ/TYPParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState155 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((((('freshtv725 * _menhir_state * Lexing.position)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_instruction__)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 768 "typ/typ/TYPParser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_instruction__)) * _menhir_state * (
# 76 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 772 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((((('freshtv723 * _menhir_state * Lexing.position)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_instruction__)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 778 "typ/typ/TYPParser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_instruction__)) * _menhir_state * (
# 76 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 782 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (((((_menhir_stack, _menhir_s, _startpos__1_), _, (xs0 : 'tv_loption_separated_nonempty_list_COMMA_instruction__)), _, (cond : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 787 "typ/typ/TYPParser.ml"
        ))), _, (xs1 : 'tv_loption_separated_nonempty_list_COMMA_instruction__)), _, (b : (
# 76 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 791 "typ/typ/TYPParser.ml"
        ))) = _menhir_stack in
        let _8 = () in
        let _6 = () in
        let _4 = () in
        let _2 = () in
        let _1 = () in
        let _startpos = _startpos__1_ in
        let _v : (
# 77 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 802 "typ/typ/TYPParser.ml"
        ) = let it =
          let xs = xs1 in
          
# 220 "/home/fusroman/.opam/4.06.0/lib/menhir/standard.mly"
    ( xs )
# 808 "typ/typ/TYPParser.ml"
          
        in
        let init =
          let xs = xs0 in
          
# 220 "/home/fusroman/.opam/4.06.0/lib/menhir/standard.mly"
    ( xs )
# 816 "typ/typ/TYPParser.ml"
          
        in
        
# 384 "typ/typ/TYPParser.mly"
    (
      For(init, cond, it, b)
    )
# 824 "typ/typ/TYPParser.ml"
         in
        _menhir_goto_control _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv724)) : 'freshtv726)
    | MenhirState137 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv733 * _menhir_state * Lexing.position)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 832 "typ/typ/TYPParser.ml"
        ))) * _menhir_state * (
# 76 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 836 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ELSE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv727 * _menhir_state * Lexing.position)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 846 "typ/typ/TYPParser.ml"
            ))) * _menhir_state * (
# 76 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 850 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BREAK ->
                _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | CONTINUE ->
                _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EXIT ->
                _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOR ->
                _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IF ->
                _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LABEL _v ->
                _menhir_run138 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB ->
                _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MULT ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NOP ->
                _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | PRINT ->
                _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RETURN ->
                _menhir_run125 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TFUN ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TINT ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TSTRING ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | WHILE ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState178) : 'freshtv728)
        | BREAK | COLON | CONTINUE | EXIT | FOR | IF | LABEL _ | LB | MULT | NOP | PRINT | RB | RETURN | SEMI | TFUN | TINT | TSTRING | WHILE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv729 * _menhir_state * Lexing.position)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 894 "typ/typ/TYPParser.ml"
            ))) * _menhir_state * (
# 76 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 898 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, _startpos__1_), _, (e : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 903 "typ/typ/TYPParser.ml"
            ))), _, (b : (
# 76 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 907 "typ/typ/TYPParser.ml"
            ))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _v : (
# 77 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 916 "typ/typ/TYPParser.ml"
            ) = 
# 369 "typ/typ/TYPParser.mly"
    (
      VARTree.If(e, b)
    )
# 922 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_control _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv730)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv731 * _menhir_state * Lexing.position)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 932 "typ/typ/TYPParser.ml"
            ))) * _menhir_state * (
# 76 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 936 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv732)) : 'freshtv734)
    | MenhirState178 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv737 * _menhir_state * Lexing.position)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 945 "typ/typ/TYPParser.ml"
        ))) * _menhir_state * (
# 76 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 949 "typ/typ/TYPParser.ml"
        ))) * _menhir_state * (
# 76 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 953 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv735 * _menhir_state * Lexing.position)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 959 "typ/typ/TYPParser.ml"
        ))) * _menhir_state * (
# 76 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 963 "typ/typ/TYPParser.ml"
        ))) * _menhir_state * (
# 76 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 967 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((((_menhir_stack, _menhir_s, _startpos__1_), _, (c : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 972 "typ/typ/TYPParser.ml"
        ))), _, (t : (
# 76 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 976 "typ/typ/TYPParser.ml"
        ))), _, (e : (
# 76 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 980 "typ/typ/TYPParser.ml"
        ))) = _menhir_stack in
        let _6 = () in
        let _4 = () in
        let _2 = () in
        let _1 = () in
        let _startpos = _startpos__1_ in
        let _v : (
# 77 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 990 "typ/typ/TYPParser.ml"
        ) = 
# 374 "typ/typ/TYPParser.mly"
    ( 
      IfElse(c, t, e)
    )
# 996 "typ/typ/TYPParser.ml"
         in
        _menhir_goto_control _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv736)) : 'freshtv738)
    | MenhirState124 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv741 * _menhir_state * Lexing.position)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 1004 "typ/typ/TYPParser.ml"
        ))) * _menhir_state * (
# 76 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 1008 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv739 * _menhir_state * Lexing.position)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 1014 "typ/typ/TYPParser.ml"
        ))) * _menhir_state * (
# 76 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 1018 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s, _startpos__1_), _, (e : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 1023 "typ/typ/TYPParser.ml"
        ))), _, (b : (
# 76 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 1027 "typ/typ/TYPParser.ml"
        ))) = _menhir_stack in
        let _4 = () in
        let _2 = () in
        let _1 = () in
        let _startpos = _startpos__1_ in
        let _v : (
# 77 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 1036 "typ/typ/TYPParser.ml"
        ) = 
# 379 "typ/typ/TYPParser.mly"
    (
      While(e, b)
    )
# 1042 "typ/typ/TYPParser.ml"
         in
        _menhir_goto_control _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv740)) : 'freshtv742)
    | _ ->
        _menhir_fail ()

and _menhir_run132 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BREAK ->
        _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CONTINUE ->
        _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | EXIT ->
        _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FOR ->
        _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IF ->
        _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LABEL _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv721) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState132 in
        let (_v : (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 1071 "typ/typ/TYPParser.ml"
        )) = _v in
        let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        ((let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLON ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack)
        | LABEL _ | LS | MULT ->
            _menhir_reduce99 _menhir_env (Obj.magic _menhir_stack)
        | ADDASSIGN | ASSIGN | DECR | DIVASSIGN | INCR | LP | MULTASSIGN | SUBASSIGN ->
            _menhir_reduce65 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv719 * _menhir_state * (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 1091 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv720)) : 'freshtv722)
    | LB ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MULT ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOP ->
        _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | PRINT ->
        _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RETURN ->
        _menhir_run125 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TFUN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TINT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TSTRING ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | WHILE ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RB ->
        _menhir_reduce64 _menhir_env (Obj.magic _menhir_stack) MenhirState132
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState132

and _menhir_goto_l_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 1123 "typ/typ/TYPParser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState63 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv661 * _menhir_state) * _menhir_state * (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 1133 "typ/typ/TYPParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv659 * _menhir_state) * _menhir_state * (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 1139 "typ/typ/TYPParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _, (e : (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 1144 "typ/typ/TYPParser.ml"
        )), _startpos_e_) = _menhir_stack in
        let _1 = () in
        let _v : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 1150 "typ/typ/TYPParser.ml"
        ) = 
# 261 "typ/typ/TYPParser.mly"
    ( e )
# 1154 "typ/typ/TYPParser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv660)) : 'freshtv662)
    | MenhirState168 | MenhirState159 | MenhirState151 | MenhirState147 | MenhirState135 | MenhirState128 | MenhirState125 | MenhirState50 | MenhirState51 | MenhirState52 | MenhirState54 | MenhirState116 | MenhirState55 | MenhirState58 | MenhirState100 | MenhirState95 | MenhirState93 | MenhirState91 | MenhirState89 | MenhirState87 | MenhirState85 | MenhirState83 | MenhirState81 | MenhirState79 | MenhirState77 | MenhirState74 | MenhirState72 | MenhirState70 | MenhirState68 | MenhirState66 | MenhirState61 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv683 * _menhir_state * (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 1162 "typ/typ/TYPParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv669 * _menhir_state * (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 1172 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LABEL _v ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv665 * _menhir_state * (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 1182 "typ/typ/TYPParser.ml"
                ) * Lexing.position)) = Obj.magic _menhir_stack in
                let (_v : (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 1187 "typ/typ/TYPParser.ml"
                )) = _v in
                let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv663 * _menhir_state * (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 1195 "typ/typ/TYPParser.ml"
                ) * Lexing.position)) = Obj.magic _menhir_stack in
                let ((l : (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 1200 "typ/typ/TYPParser.ml"
                )) : (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 1204 "typ/typ/TYPParser.ml"
                )) = _v in
                let (_startpos_l_ : Lexing.position) = _startpos in
                ((let (_menhir_stack, _menhir_s, (l_expr : (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 1210 "typ/typ/TYPParser.ml"
                )), _startpos_l_expr_) = _menhir_stack in
                let _2 = () in
                let _v : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 1216 "typ/typ/TYPParser.ml"
                ) = 
# 266 "typ/typ/TYPParser.mly"
    (
      RecordAccess(l_expr, l)
    )
# 1222 "typ/typ/TYPParser.ml"
                 in
                _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv664)) : 'freshtv666)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv667 * _menhir_state * (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 1232 "typ/typ/TYPParser.ml"
                ) * Lexing.position)) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv668)) : 'freshtv670)
        | DOTSIZE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv673 * _menhir_state * (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 1241 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv671 * _menhir_state * (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 1248 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (l_expr : (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 1253 "typ/typ/TYPParser.ml"
            )), _startpos_l_expr_) = _menhir_stack in
            let _2 = () in
            let _v : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 1259 "typ/typ/TYPParser.ml"
            ) = 
# 274 "typ/typ/TYPParser.mly"
    (
      ArrayAccess (l_expr, Int (-1))
    )
# 1265 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv672)) : 'freshtv674)
        | LP ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv675 * _menhir_state * (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 1273 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ADDRESS ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState95
            | BOOL _v ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v
            | CPL ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState95
            | INT _v ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v
            | LABEL _v ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LP ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState95
            | MULT ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NOT ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState95
            | SUB ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState95
            | RP ->
                _menhir_reduce71 _menhir_env (Obj.magic _menhir_stack) MenhirState95
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState95) : 'freshtv676)
        | LS ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv677 * _menhir_state * (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 1309 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ADDRESS ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState66
            | BOOL _v ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
            | CPL ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState66
            | INT _v ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
            | LABEL _v ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LP ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState66
            | MULT ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NOT ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState66
            | SUB ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState66
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState66) : 'freshtv678)
        | ADD | AND | COLON | COMMA | DIV | EQ | GE | GT | LE | LT | MULT | NEQ | OR | RB | REM | RP | RS | SEMI | SUB ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv679 * _menhir_state * (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 1343 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (l : (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 1348 "typ/typ/TYPParser.ml"
            )), _startpos_l_) = _menhir_stack in
            let _v : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 1353 "typ/typ/TYPParser.ml"
            ) = 
# 225 "typ/typ/TYPParser.mly"
    ( Deref l )
# 1357 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv680)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv681 * _menhir_state * (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 1367 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv682)) : 'freshtv684)
    | MenhirState48 | MenhirState124 | MenhirState191 | MenhirState185 | MenhirState188 | MenhirState186 | MenhirState183 | MenhirState132 | MenhirState178 | MenhirState137 | MenhirState140 | MenhirState175 | MenhirState153 | MenhirState155 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv717 * _menhir_state * (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 1376 "typ/typ/TYPParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADDASSIGN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv687) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv685) = Obj.magic _menhir_stack in
            ((let _1 = () in
            let _v : 'tv_assign_binop = 
# 345 "typ/typ/TYPParser.mly"
              ( AddAssign )
# 1391 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_assign_binop _menhir_env _menhir_stack _v) : 'freshtv686)) : 'freshtv688)
        | ASSIGN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv691) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv689) = Obj.magic _menhir_stack in
            ((let _1 = () in
            let _v : 'tv_assign_binop = 
# 344 "typ/typ/TYPParser.mly"
              ( Standard )
# 1404 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_assign_binop _menhir_env _menhir_stack _v) : 'freshtv690)) : 'freshtv692)
        | DECR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv695) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv693) = Obj.magic _menhir_stack in
            ((let _1 = () in
            let _v : 'tv_assign_unop = 
# 353 "typ/typ/TYPParser.mly"
              ( Decr )
# 1417 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_assign_unop _menhir_env _menhir_stack _v) : 'freshtv694)) : 'freshtv696)
        | DIVASSIGN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv699) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv697) = Obj.magic _menhir_stack in
            ((let _1 = () in
            let _v : 'tv_assign_binop = 
# 348 "typ/typ/TYPParser.mly"
              ( DivAssign )
# 1430 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_assign_binop _menhir_env _menhir_stack _v) : 'freshtv698)) : 'freshtv700)
        | INCR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv703) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv701) = Obj.magic _menhir_stack in
            ((let _1 = () in
            let _v : 'tv_assign_unop = 
# 352 "typ/typ/TYPParser.mly"
              ( Incr )
# 1443 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_assign_unop _menhir_env _menhir_stack _v) : 'freshtv702)) : 'freshtv704)
        | LP ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv705 * _menhir_state * (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 1451 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ADDRESS ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState159
            | BOOL _v ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState159 _v
            | CPL ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState159
            | INT _v ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState159 _v
            | LABEL _v ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState159 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState159 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LP ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState159
            | MULT ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState159 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NOT ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState159
            | SUB ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState159
            | RP ->
                _menhir_reduce71 _menhir_env (Obj.magic _menhir_stack) MenhirState159
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState159) : 'freshtv706)
        | MULTASSIGN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv709) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv707) = Obj.magic _menhir_stack in
            ((let _1 = () in
            let _v : 'tv_assign_binop = 
# 347 "typ/typ/TYPParser.mly"
              ( MultAssign )
# 1492 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_assign_binop _menhir_env _menhir_stack _v) : 'freshtv708)) : 'freshtv710)
        | SUBASSIGN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv713) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv711) = Obj.magic _menhir_stack in
            ((let _1 = () in
            let _v : 'tv_assign_binop = 
# 346 "typ/typ/TYPParser.mly"
              ( SubAssign )
# 1505 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_assign_binop _menhir_env _menhir_stack _v) : 'freshtv712)) : 'freshtv714)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv715 * _menhir_state * (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 1515 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv716)) : 'freshtv718)
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_SEMI_field_instanciation_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_SEMI_field_instanciation_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState56 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv653 * _menhir_state * Lexing.position) * _menhir_state * 'tv_separated_nonempty_list_SEMI_field_instanciation_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RB ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv641 * _menhir_state * Lexing.position) * _menhir_state * 'tv_separated_nonempty_list_SEMI_field_instanciation_) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv639 * _menhir_state * Lexing.position) * _menhir_state * 'tv_separated_nonempty_list_SEMI_field_instanciation_) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, _startpos__1_), _, (fields : 'tv_separated_nonempty_list_SEMI_field_instanciation_)) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 1544 "typ/typ/TYPParser.ml"
            ) = 
# 279 "typ/typ/TYPParser.mly"
    (
      NewRecord fields
    )
# 1550 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv640)) : 'freshtv642)
        | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv649 * _menhir_state * Lexing.position) * _menhir_state * 'tv_separated_nonempty_list_SEMI_field_instanciation_) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | RB ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv645 * _menhir_state * Lexing.position) * _menhir_state * 'tv_separated_nonempty_list_SEMI_field_instanciation_)) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv643 * _menhir_state * Lexing.position) * _menhir_state * 'tv_separated_nonempty_list_SEMI_field_instanciation_)) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s, _startpos__1_), _, (fields : 'tv_separated_nonempty_list_SEMI_field_instanciation_)) = _menhir_stack in
                let _4 = () in
                let _3 = () in
                let _1 = () in
                let _v : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 1572 "typ/typ/TYPParser.ml"
                ) = 
# 279 "typ/typ/TYPParser.mly"
    (
      NewRecord fields
    )
# 1578 "typ/typ/TYPParser.ml"
                 in
                _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv644)) : 'freshtv646)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv647 * _menhir_state * Lexing.position) * _menhir_state * 'tv_separated_nonempty_list_SEMI_field_instanciation_)) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv648)) : 'freshtv650)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv651 * _menhir_state * Lexing.position) * _menhir_state * 'tv_separated_nonempty_list_SEMI_field_instanciation_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv652)) : 'freshtv654)
    | MenhirState112 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv657 * _menhir_state * 'tv_field_instanciation)) * _menhir_state * 'tv_separated_nonempty_list_SEMI_field_instanciation_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv655 * _menhir_state * 'tv_field_instanciation)) * _menhir_state * 'tv_separated_nonempty_list_SEMI_field_instanciation_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (x : 'tv_field_instanciation)), _, (xs : 'tv_separated_nonempty_list_SEMI_field_instanciation_)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_separated_nonempty_list_SEMI_field_instanciation_ = 
# 231 "/home/fusroman/.opam/4.06.0/lib/menhir/standard.mly"
    ( x :: xs )
# 1605 "typ/typ/TYPParser.ml"
         in
        _menhir_goto_separated_nonempty_list_SEMI_field_instanciation_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv656)) : 'freshtv658)
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_COMMA_expr_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_COMMA_expr_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState159 | MenhirState95 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv633) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_COMMA_expr_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv631) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((x : 'tv_separated_nonempty_list_COMMA_expr_) : 'tv_separated_nonempty_list_COMMA_expr_) = _v in
        ((let _v : 'tv_loption_separated_nonempty_list_COMMA_expr__ = 
# 144 "/home/fusroman/.opam/4.06.0/lib/menhir/standard.mly"
    ( x )
# 1626 "typ/typ/TYPParser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_COMMA_expr__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv632)) : 'freshtv634)
    | MenhirState100 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv637 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 1634 "typ/typ/TYPParser.ml"
        ))) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_COMMA_expr_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv635 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 1642 "typ/typ/TYPParser.ml"
        ))) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((xs : 'tv_separated_nonempty_list_COMMA_expr_) : 'tv_separated_nonempty_list_COMMA_expr_) = _v in
        ((let (_menhir_stack, _menhir_s, (x : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 1649 "typ/typ/TYPParser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_separated_nonempty_list_COMMA_expr_ = 
# 231 "/home/fusroman/.opam/4.06.0/lib/menhir/standard.mly"
    ( x :: xs )
# 1655 "typ/typ/TYPParser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_expr_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv636)) : 'freshtv638)
    | _ ->
        _menhir_fail ()

and _menhir_run68 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 1664 "typ/typ/TYPParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | BOOL _v ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
    | CPL ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | INT _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
    | LABEL _v ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | MULT ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | SUB ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState68

and _menhir_run70 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 1698 "typ/typ/TYPParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | BOOL _v ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
    | CPL ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | INT _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
    | LABEL _v ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | MULT ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | SUB ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState70

and _menhir_run77 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 1732 "typ/typ/TYPParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | BOOL _v ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
    | CPL ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | INT _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
    | LABEL _v ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | MULT ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | SUB ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState77

and _menhir_run79 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 1766 "typ/typ/TYPParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | BOOL _v ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
    | CPL ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | INT _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
    | LABEL _v ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | MULT ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | SUB ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState79

and _menhir_run72 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 1800 "typ/typ/TYPParser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _startpos ->
    let _menhir_stack = (_menhir_stack, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | BOOL _v ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
    | CPL ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | INT _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
    | LABEL _v ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | MULT ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | SUB ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState72

and _menhir_run83 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 1835 "typ/typ/TYPParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | BOOL _v ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
    | CPL ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | INT _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
    | LABEL _v ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | MULT ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | SUB ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState83

and _menhir_run85 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 1869 "typ/typ/TYPParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | BOOL _v ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v
    | CPL ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | INT _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v
    | LABEL _v ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | MULT ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | SUB ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState85

and _menhir_run87 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 1903 "typ/typ/TYPParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | BOOL _v ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
    | CPL ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | INT _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
    | LABEL _v ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | MULT ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | SUB ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState87

and _menhir_run89 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 1937 "typ/typ/TYPParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | BOOL _v ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
    | CPL ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | INT _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
    | LABEL _v ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | MULT ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | SUB ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState89

and _menhir_run91 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 1971 "typ/typ/TYPParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | BOOL _v ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
    | CPL ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | INT _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
    | LABEL _v ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | MULT ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | SUB ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState91

and _menhir_run74 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 2005 "typ/typ/TYPParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | BOOL _v ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
    | CPL ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | INT _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
    | LABEL _v ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | MULT ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | SUB ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState74

and _menhir_run93 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 2039 "typ/typ/TYPParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | BOOL _v ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v
    | CPL ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | INT _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v
    | LABEL _v ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | MULT ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | SUB ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState93

and _menhir_run81 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 2073 "typ/typ/TYPParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | BOOL _v ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
    | CPL ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | INT _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
    | LABEL _v ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | MULT ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | SUB ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState81

and _menhir_goto_instructions : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 72 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 2107 "typ/typ/TYPParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState132 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv595 * _menhir_state * Lexing.position) * _menhir_state * (
# 72 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 2117 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RB ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv591 * _menhir_state * Lexing.position) * _menhir_state * (
# 72 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 2127 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv589 * _menhir_state * Lexing.position) * _menhir_state * (
# 72 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 2134 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, _startpos__1_), _, (i : (
# 72 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 2139 "typ/typ/TYPParser.ml"
            ))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (
# 76 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 2146 "typ/typ/TYPParser.ml"
            ) = 
# 364 "typ/typ/TYPParser.mly"
    ( i )
# 2150 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_block _menhir_env _menhir_stack _menhir_s _v) : 'freshtv590)) : 'freshtv592)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv593 * _menhir_state * Lexing.position) * _menhir_state * (
# 72 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 2160 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv594)) : 'freshtv596)
    | MenhirState183 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv599 * _menhir_state * (
# 75 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 2169 "typ/typ/TYPParser.ml"
        ) * Lexing.position)) * _menhir_state * (
# 72 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 2173 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv597 * _menhir_state * (
# 75 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 2179 "typ/typ/TYPParser.ml"
        ) * Lexing.position)) * _menhir_state * (
# 72 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 2183 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (i : (
# 75 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 2188 "typ/typ/TYPParser.ml"
        )), _startpos_i_), _, (s : (
# 72 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 2192 "typ/typ/TYPParser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : (
# 72 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 2198 "typ/typ/TYPParser.ml"
        ) = 
# 200 "typ/typ/TYPParser.mly"
    (
      i::s
    )
# 2204 "typ/typ/TYPParser.ml"
         in
        _menhir_goto_instructions _menhir_env _menhir_stack _menhir_s _v) : 'freshtv598)) : 'freshtv600)
    | MenhirState186 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv603 * _menhir_state * (
# 77 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 2212 "typ/typ/TYPParser.ml"
        ) * Lexing.position) * _menhir_state) * _menhir_state * (
# 72 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 2216 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv601 * _menhir_state * (
# 77 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 2222 "typ/typ/TYPParser.ml"
        ) * Lexing.position) * _menhir_state) * _menhir_state * (
# 72 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 2226 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s, (c : (
# 77 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 2231 "typ/typ/TYPParser.ml"
        )), _startpos_c_), _), _, (s : (
# 72 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 2235 "typ/typ/TYPParser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : (
# 72 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 2241 "typ/typ/TYPParser.ml"
        ) = 
# 206 "typ/typ/TYPParser.mly"
    (
      c::s
    )
# 2247 "typ/typ/TYPParser.ml"
         in
        _menhir_goto_instructions _menhir_env _menhir_stack _menhir_s _v) : 'freshtv602)) : 'freshtv604)
    | MenhirState188 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv607 * _menhir_state * (
# 77 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 2255 "typ/typ/TYPParser.ml"
        ) * Lexing.position) * _menhir_state) * _menhir_state * (
# 72 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 2259 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv605 * _menhir_state * (
# 77 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 2265 "typ/typ/TYPParser.ml"
        ) * Lexing.position) * _menhir_state) * _menhir_state * (
# 72 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 2269 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s, (_1 : (
# 77 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 2274 "typ/typ/TYPParser.ml"
        )), _startpos__1_), _), _, (_3 : (
# 72 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 2278 "typ/typ/TYPParser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : (
# 72 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 2284 "typ/typ/TYPParser.ml"
        ) = let _startpos = _startpos__1_ in
        
# 212 "typ/typ/TYPParser.mly"
    (
      raise_syntax_error _startpos "Expected ';', found ':'."
    )
# 2291 "typ/typ/TYPParser.ml"
         in
        _menhir_goto_instructions _menhir_env _menhir_stack _menhir_s _v) : 'freshtv606)) : 'freshtv608)
    | MenhirState185 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv611 * _menhir_state * (
# 77 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 2299 "typ/typ/TYPParser.ml"
        ) * Lexing.position) * _menhir_state * (
# 72 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 2303 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv609 * _menhir_state * (
# 77 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 2309 "typ/typ/TYPParser.ml"
        ) * Lexing.position) * _menhir_state * (
# 72 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 2313 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (c : (
# 77 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 2318 "typ/typ/TYPParser.ml"
        )), _startpos_c_), _, (s : (
# 72 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 2322 "typ/typ/TYPParser.ml"
        ))) = _menhir_stack in
        let _v : (
# 72 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 2327 "typ/typ/TYPParser.ml"
        ) = 
# 206 "typ/typ/TYPParser.mly"
    (
      c::s
    )
# 2333 "typ/typ/TYPParser.ml"
         in
        _menhir_goto_instructions _menhir_env _menhir_stack _menhir_s _v) : 'freshtv610)) : 'freshtv612)
    | MenhirState191 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv615 * _menhir_state * (
# 75 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 2341 "typ/typ/TYPParser.ml"
        ) * Lexing.position)) * _menhir_state * (
# 72 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 2345 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv613 * _menhir_state * (
# 75 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 2351 "typ/typ/TYPParser.ml"
        ) * Lexing.position)) * _menhir_state * (
# 72 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 2355 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (_1 : (
# 75 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 2360 "typ/typ/TYPParser.ml"
        )), _startpos__1_), _, (_3 : (
# 72 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 2364 "typ/typ/TYPParser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : (
# 72 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 2370 "typ/typ/TYPParser.ml"
        ) = let _startpos = _startpos__1_ in
        
# 212 "typ/typ/TYPParser.mly"
    (
      raise_syntax_error _startpos "Expected ';', found ':'."
    )
# 2377 "typ/typ/TYPParser.ml"
         in
        _menhir_goto_instructions _menhir_env _menhir_stack _menhir_s _v) : 'freshtv614)) : 'freshtv616)
    | MenhirState48 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv629 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 2385 "typ/typ/TYPParser.ml"
        ) * Lexing.position) * (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 2389 "typ/typ/TYPParser.ml"
        ) * Lexing.position)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_parameter__)) * Lexing.position) * _menhir_state * (
# 72 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 2393 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RB ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv625 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 2403 "typ/typ/TYPParser.ml"
            ) * Lexing.position) * (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 2407 "typ/typ/TYPParser.ml"
            ) * Lexing.position)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_parameter__)) * Lexing.position) * _menhir_state * (
# 72 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 2411 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv623 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 2418 "typ/typ/TYPParser.ml"
            ) * Lexing.position) * (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 2422 "typ/typ/TYPParser.ml"
            ) * Lexing.position)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_parameter__)) * Lexing.position) * _menhir_state * (
# 72 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 2426 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (((((_menhir_stack, _menhir_s, (return_type : (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 2431 "typ/typ/TYPParser.ml"
            )), _startpos_return_type_), (name : (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 2435 "typ/typ/TYPParser.ml"
            )), _startpos_name_), _, (xs0 : 'tv_loption_separated_nonempty_list_COMMA_parameter__)), _startpos__6_), _, (b : (
# 72 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 2439 "typ/typ/TYPParser.ml"
            ))) = _menhir_stack in
            let _8 = () in
            let _6 = () in
            let _5 = () in
            let _3 = () in
            let _v : (
# 71 "typ/typ/TYPParser.mly"
      (TYPTree.typ_function)
# 2448 "typ/typ/TYPParser.ml"
            ) = let params =
              let xs = xs0 in
              
# 220 "/home/fusroman/.opam/4.06.0/lib/menhir/standard.mly"
    ( xs )
# 2454 "typ/typ/TYPParser.ml"
              
            in
            let _startpos = _startpos_return_type_ in
            
# 130 "typ/typ/TYPParser.mly"
    (
      {name = make_node _startpos name; params; block = b; return_type}
    )
# 2463 "typ/typ/TYPParser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv621) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 71 "typ/typ/TYPParser.mly"
      (TYPTree.typ_function)
# 2471 "typ/typ/TYPParser.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv619) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 71 "typ/typ/TYPParser.mly"
      (TYPTree.typ_function)
# 2479 "typ/typ/TYPParser.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv617) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let ((def_fun : (
# 71 "typ/typ/TYPParser.mly"
      (TYPTree.typ_function)
# 2487 "typ/typ/TYPParser.ml"
            )) : (
# 71 "typ/typ/TYPParser.mly"
      (TYPTree.typ_function)
# 2491 "typ/typ/TYPParser.ml"
            )) = _v in
            ((let _v : 'tv_global_declaration = 
# 184 "typ/typ/TYPParser.mly"
    (
      Fun f
    )
# 2498 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_global_declaration _menhir_env _menhir_stack _menhir_s _v) : 'freshtv618)) : 'freshtv620)) : 'freshtv622)) : 'freshtv624)) : 'freshtv626)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv627 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 2508 "typ/typ/TYPParser.ml"
            ) * Lexing.position) * (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 2512 "typ/typ/TYPParser.ml"
            ) * Lexing.position)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_parameter__)) * Lexing.position) * _menhir_state * (
# 72 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 2516 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv628)) : 'freshtv630)
    | _ ->
        _menhir_fail ()

and _menhir_reduce73 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_loption_separated_nonempty_list_COMMA_instruction__ = 
# 142 "/home/fusroman/.opam/4.06.0/lib/menhir/standard.mly"
    ( [] )
# 2528 "typ/typ/TYPParser.ml"
     in
    _menhir_goto_loption_separated_nonempty_list_COMMA_instruction__ _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_instruction : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 75 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 2535 "typ/typ/TYPParser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState124 | MenhirState178 | MenhirState137 | MenhirState155 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv571 * _menhir_state * (
# 75 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 2545 "typ/typ/TYPParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv567 * _menhir_state * (
# 75 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 2555 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv565 * _menhir_state * (
# 75 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 2562 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (i : (
# 75 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 2567 "typ/typ/TYPParser.ml"
            )), _startpos_i_) = _menhir_stack in
            let _2 = () in
            let _v : (
# 76 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 2573 "typ/typ/TYPParser.ml"
            ) = 
# 358 "typ/typ/TYPParser.mly"
    ( [i] )
# 2577 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_block _menhir_env _menhir_stack _menhir_s _v) : 'freshtv566)) : 'freshtv568)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv569 * _menhir_state * (
# 75 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 2587 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv570)) : 'freshtv572)
    | MenhirState140 | MenhirState175 | MenhirState153 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv579 * _menhir_state * (
# 75 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 2596 "typ/typ/TYPParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COMMA ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv573 * _menhir_state * (
# 75 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 2606 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BREAK ->
                _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState175 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | CONTINUE ->
                _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState175 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EXIT ->
                _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState175 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LABEL _v ->
                _menhir_run138 _menhir_env (Obj.magic _menhir_stack) MenhirState175 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState175 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MULT ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState175 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NOP ->
                _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState175 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | PRINT ->
                _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState175 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RETURN ->
                _menhir_run125 _menhir_env (Obj.magic _menhir_stack) MenhirState175 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TFUN ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState175 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TINT ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState175 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TSTRING ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState175 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState175) : 'freshtv574)
        | RP | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv575 * _menhir_state * (
# 75 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 2644 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (x : (
# 75 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 2649 "typ/typ/TYPParser.ml"
            )), _startpos_x_) = _menhir_stack in
            let _v : 'tv_separated_nonempty_list_COMMA_instruction_ = 
# 229 "/home/fusroman/.opam/4.06.0/lib/menhir/standard.mly"
    ( [ x ] )
# 2654 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_separated_nonempty_list_COMMA_instruction_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv576)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv577 * _menhir_state * (
# 75 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 2664 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv578)) : 'freshtv580)
    | MenhirState48 | MenhirState191 | MenhirState185 | MenhirState188 | MenhirState186 | MenhirState183 | MenhirState132 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv587 * _menhir_state * (
# 75 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 2673 "typ/typ/TYPParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv581 * _menhir_state * (
# 75 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 2683 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BREAK ->
                _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState191 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | CONTINUE ->
                _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState191 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EXIT ->
                _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState191 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOR ->
                _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState191 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IF ->
                _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState191 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LABEL _v ->
                _menhir_run138 _menhir_env (Obj.magic _menhir_stack) MenhirState191 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState191 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MULT ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState191 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NOP ->
                _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState191 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | PRINT ->
                _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState191 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RETURN ->
                _menhir_run125 _menhir_env (Obj.magic _menhir_stack) MenhirState191 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TFUN ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState191 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TINT ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState191 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TSTRING ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState191 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | WHILE ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState191 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RB ->
                _menhir_reduce64 _menhir_env (Obj.magic _menhir_stack) MenhirState191
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState191) : 'freshtv582)
        | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv583 * _menhir_state * (
# 75 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 2729 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BREAK ->
                _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState183 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | CONTINUE ->
                _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState183 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EXIT ->
                _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState183 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOR ->
                _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState183 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IF ->
                _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState183 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LABEL _v ->
                _menhir_run138 _menhir_env (Obj.magic _menhir_stack) MenhirState183 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState183 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MULT ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState183 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NOP ->
                _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState183 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | PRINT ->
                _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState183 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RETURN ->
                _menhir_run125 _menhir_env (Obj.magic _menhir_stack) MenhirState183 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TFUN ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState183 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TINT ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState183 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TSTRING ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState183 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | WHILE ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState183 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RB ->
                _menhir_reduce64 _menhir_env (Obj.magic _menhir_stack) MenhirState183
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState183) : 'freshtv584)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv585 * _menhir_state * (
# 75 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 2777 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv586)) : 'freshtv588)
    | _ ->
        _menhir_fail ()

and _menhir_run57 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 2787 "typ/typ/TYPParser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ASSIGN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv561 * _menhir_state * (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 2799 "typ/typ/TYPParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADDRESS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | BOOL _v ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
        | CPL ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | INT _v ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
        | LABEL _v ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LB ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LP ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | MULT ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NOT ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | SUB ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState58) : 'freshtv562)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv563 * _menhir_state * (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 2835 "typ/typ/TYPParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv564)

and _menhir_reduce65 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 2843 "typ/typ/TYPParser.ml"
) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (t : (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 2849 "typ/typ/TYPParser.ml"
    )), _startpos_t_) = _menhir_stack in
    let _startpos = _startpos_t_ in
    let _v : (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 2855 "typ/typ/TYPParser.ml"
    ) = let _startpos = _startpos_t_ in
    
# 297 "typ/typ/TYPParser.mly"
    ( Id (make_node _startpos t) )
# 2860 "typ/typ/TYPParser.ml"
     in
    _menhir_goto_l_expr _menhir_env _menhir_stack _menhir_s _v _startpos

and _menhir_goto_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 2867 "typ/typ/TYPParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState66 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv363 * _menhir_state * (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 2877 "typ/typ/TYPParser.ml"
        ) * Lexing.position)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 2881 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADD ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NEQ ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | RS ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv359 * _menhir_state * (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 2915 "typ/typ/TYPParser.ml"
            ) * Lexing.position)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 2919 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv357 * _menhir_state * (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 2926 "typ/typ/TYPParser.ml"
            ) * Lexing.position)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 2930 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (l_expr : (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 2935 "typ/typ/TYPParser.ml"
            )), _startpos_l_expr_), _, (e : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 2939 "typ/typ/TYPParser.ml"
            ))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 2946 "typ/typ/TYPParser.ml"
            ) = 
# 270 "typ/typ/TYPParser.mly"
    (
      ArrayAccess (l_expr, e)
    )
# 2952 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv358)) : 'freshtv360)
        | SUB ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv361 * _menhir_state * (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 2964 "typ/typ/TYPParser.ml"
            ) * Lexing.position)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 2968 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv362)) : 'freshtv364)
    | MenhirState68 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv369 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 2977 "typ/typ/TYPParser.ml"
        ))) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 2981 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIV ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | REM ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | ADD | AND | COLON | COMMA | EQ | GE | GT | LE | LT | NEQ | OR | RB | RP | RS | SEMI | SUB ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv365 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 2997 "typ/typ/TYPParser.ml"
            ))) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3001 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (e1 : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3006 "typ/typ/TYPParser.ml"
            ))), _, (e2 : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3010 "typ/typ/TYPParser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3016 "typ/typ/TYPParser.ml"
            ) = 
# 231 "typ/typ/TYPParser.mly"
    ( Binop(e1, Sub, e2) )
# 3020 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv366)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv367 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3030 "typ/typ/TYPParser.ml"
            ))) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3034 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv368)) : 'freshtv370)
    | MenhirState70 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv373 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3043 "typ/typ/TYPParser.ml"
        ))) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3047 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv371 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3053 "typ/typ/TYPParser.ml"
        ))) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3057 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (e1 : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3062 "typ/typ/TYPParser.ml"
        ))), _, (e2 : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3066 "typ/typ/TYPParser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3072 "typ/typ/TYPParser.ml"
        ) = 
# 237 "typ/typ/TYPParser.mly"
    ( Binop(e1, Rem, e2) )
# 3076 "typ/typ/TYPParser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv372)) : 'freshtv374)
    | MenhirState72 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv377 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3084 "typ/typ/TYPParser.ml"
        )) * Lexing.position) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3088 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv375 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3094 "typ/typ/TYPParser.ml"
        )) * Lexing.position) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3098 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s, (e1 : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3103 "typ/typ/TYPParser.ml"
        ))), _startpos__2_), _, (e2 : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3107 "typ/typ/TYPParser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3113 "typ/typ/TYPParser.ml"
        ) = 
# 233 "typ/typ/TYPParser.mly"
    ( Binop(e1, Mult, e2) )
# 3117 "typ/typ/TYPParser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv376)) : 'freshtv378)
    | MenhirState74 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv381 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3125 "typ/typ/TYPParser.ml"
        ))) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3129 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv379 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3135 "typ/typ/TYPParser.ml"
        ))) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3139 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (e1 : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3144 "typ/typ/TYPParser.ml"
        ))), _, (e2 : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3148 "typ/typ/TYPParser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3154 "typ/typ/TYPParser.ml"
        ) = 
# 235 "typ/typ/TYPParser.mly"
    ( Binop(e1, Div, e2) )
# 3158 "typ/typ/TYPParser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv380)) : 'freshtv382)
    | MenhirState77 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv387 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3166 "typ/typ/TYPParser.ml"
        ))) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3170 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADD ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NEQ ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | AND | COLON | COMMA | OR | RB | RP | RS | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv383 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3202 "typ/typ/TYPParser.ml"
            ))) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3206 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (e1 : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3211 "typ/typ/TYPParser.ml"
            ))), _, (e2 : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3215 "typ/typ/TYPParser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3221 "typ/typ/TYPParser.ml"
            ) = 
# 241 "typ/typ/TYPParser.mly"
    ( Binop(e1, Or, e2) )
# 3225 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv384)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv385 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3235 "typ/typ/TYPParser.ml"
            ))) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3239 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv386)) : 'freshtv388)
    | MenhirState79 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv393 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3248 "typ/typ/TYPParser.ml"
        ))) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3252 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADD ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | REM ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | AND | COLON | COMMA | EQ | GE | GT | LE | LT | NEQ | OR | RB | RP | RS | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv389 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3272 "typ/typ/TYPParser.ml"
            ))) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3276 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (e1 : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3281 "typ/typ/TYPParser.ml"
            ))), _, (e2 : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3285 "typ/typ/TYPParser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3291 "typ/typ/TYPParser.ml"
            ) = 
# 253 "typ/typ/TYPParser.mly"
    ( Binop(e1, Neq, e2) )
# 3295 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv390)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv391 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3305 "typ/typ/TYPParser.ml"
            ))) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3309 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv392)) : 'freshtv394)
    | MenhirState81 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv399 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3318 "typ/typ/TYPParser.ml"
        ))) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3322 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIV ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | REM ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | ADD | AND | COLON | COMMA | EQ | GE | GT | LE | LT | NEQ | OR | RB | RP | RS | SEMI | SUB ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv395 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3338 "typ/typ/TYPParser.ml"
            ))) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3342 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (e1 : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3347 "typ/typ/TYPParser.ml"
            ))), _, (e2 : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3351 "typ/typ/TYPParser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3357 "typ/typ/TYPParser.ml"
            ) = 
# 229 "typ/typ/TYPParser.mly"
    ( Binop(e1, Add, e2) )
# 3361 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv396)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv397 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3371 "typ/typ/TYPParser.ml"
            ))) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3375 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv398)) : 'freshtv400)
    | MenhirState83 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv405 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3384 "typ/typ/TYPParser.ml"
        ))) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3388 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADD ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | REM ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | AND | COLON | COMMA | EQ | GE | GT | LE | LT | NEQ | OR | RB | RP | RS | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv401 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3408 "typ/typ/TYPParser.ml"
            ))) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3412 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (e1 : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3417 "typ/typ/TYPParser.ml"
            ))), _, (e2 : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3421 "typ/typ/TYPParser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3427 "typ/typ/TYPParser.ml"
            ) = 
# 243 "typ/typ/TYPParser.mly"
    ( Binop(e1, Lt, e2) )
# 3431 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv402)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv403 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3441 "typ/typ/TYPParser.ml"
            ))) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3445 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv404)) : 'freshtv406)
    | MenhirState85 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv411 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3454 "typ/typ/TYPParser.ml"
        ))) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3458 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADD ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | REM ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | AND | COLON | COMMA | EQ | GE | GT | LE | LT | NEQ | OR | RB | RP | RS | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv407 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3478 "typ/typ/TYPParser.ml"
            ))) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3482 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (e1 : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3487 "typ/typ/TYPParser.ml"
            ))), _, (e2 : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3491 "typ/typ/TYPParser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3497 "typ/typ/TYPParser.ml"
            ) = 
# 245 "typ/typ/TYPParser.mly"
    ( Binop(e1, Le, e2) )
# 3501 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv408)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv409 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3511 "typ/typ/TYPParser.ml"
            ))) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3515 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv410)) : 'freshtv412)
    | MenhirState87 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv417 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3524 "typ/typ/TYPParser.ml"
        ))) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3528 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADD ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | REM ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | AND | COLON | COMMA | EQ | GE | GT | LE | LT | NEQ | OR | RB | RP | RS | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv413 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3548 "typ/typ/TYPParser.ml"
            ))) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3552 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (e1 : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3557 "typ/typ/TYPParser.ml"
            ))), _, (e2 : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3561 "typ/typ/TYPParser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3567 "typ/typ/TYPParser.ml"
            ) = 
# 247 "typ/typ/TYPParser.mly"
    ( Binop(e1, Gt, e2) )
# 3571 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv414)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv415 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3581 "typ/typ/TYPParser.ml"
            ))) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3585 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv416)) : 'freshtv418)
    | MenhirState89 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv423 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3594 "typ/typ/TYPParser.ml"
        ))) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3598 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADD ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | REM ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | AND | COLON | COMMA | EQ | GE | GT | LE | LT | NEQ | OR | RB | RP | RS | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv419 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3618 "typ/typ/TYPParser.ml"
            ))) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3622 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (e1 : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3627 "typ/typ/TYPParser.ml"
            ))), _, (e2 : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3631 "typ/typ/TYPParser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3637 "typ/typ/TYPParser.ml"
            ) = 
# 249 "typ/typ/TYPParser.mly"
    ( Binop(e1, Ge, e2) )
# 3641 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv420)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv421 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3651 "typ/typ/TYPParser.ml"
            ))) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3655 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv422)) : 'freshtv424)
    | MenhirState91 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv429 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3664 "typ/typ/TYPParser.ml"
        ))) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3668 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADD ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | REM ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | AND | COLON | COMMA | EQ | GE | GT | LE | LT | NEQ | OR | RB | RP | RS | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv425 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3688 "typ/typ/TYPParser.ml"
            ))) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3692 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (e1 : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3697 "typ/typ/TYPParser.ml"
            ))), _, (e2 : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3701 "typ/typ/TYPParser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3707 "typ/typ/TYPParser.ml"
            ) = 
# 251 "typ/typ/TYPParser.mly"
    ( Binop(e1, Eq, e2) )
# 3711 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv426)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv427 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3721 "typ/typ/TYPParser.ml"
            ))) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3725 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv428)) : 'freshtv430)
    | MenhirState93 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv435 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3734 "typ/typ/TYPParser.ml"
        ))) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3738 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADD ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NEQ ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | AND | COLON | COMMA | OR | RB | RP | RS | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv431 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3770 "typ/typ/TYPParser.ml"
            ))) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3774 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (e1 : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3779 "typ/typ/TYPParser.ml"
            ))), _, (e2 : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3783 "typ/typ/TYPParser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3789 "typ/typ/TYPParser.ml"
            ) = 
# 239 "typ/typ/TYPParser.mly"
    ( Binop(e1, And, e2) )
# 3793 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv432)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv433 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3803 "typ/typ/TYPParser.ml"
            ))) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3807 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv434)) : 'freshtv436)
    | MenhirState159 | MenhirState100 | MenhirState95 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv443 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3816 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADD ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack)
        | COMMA ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv437 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3830 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ADDRESS ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState100
            | BOOL _v ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
            | CPL ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState100
            | INT _v ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
            | LABEL _v ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LP ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState100
            | MULT ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NOT ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState100
            | SUB ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState100
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState100) : 'freshtv438)
        | DIV ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NEQ ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | RP ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv439 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3886 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (x : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3891 "typ/typ/TYPParser.ml"
            ))) = _menhir_stack in
            let _v : 'tv_separated_nonempty_list_COMMA_expr_ = 
# 229 "/home/fusroman/.opam/4.06.0/lib/menhir/standard.mly"
    ( [ x ] )
# 3896 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_separated_nonempty_list_COMMA_expr_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv440)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv441 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3906 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv442)) : 'freshtv444)
    | MenhirState61 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv447 * _menhir_state) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3915 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv445 * _menhir_state) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3921 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _, (e : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3926 "typ/typ/TYPParser.ml"
        ))) = _menhir_stack in
        let _1 = () in
        let _v : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3932 "typ/typ/TYPParser.ml"
        ) = 
# 257 "typ/typ/TYPParser.mly"
    ( Unop(Cpl, e) )
# 3936 "typ/typ/TYPParser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv446)) : 'freshtv448)
    | MenhirState58 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv463 * _menhir_state * (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 3944 "typ/typ/TYPParser.ml"
        ) * Lexing.position)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3948 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADD ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NEQ ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | RB | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv459 * _menhir_state * (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 3984 "typ/typ/TYPParser.ml"
            ) * Lexing.position)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3988 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (f : (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 3993 "typ/typ/TYPParser.ml"
            )), _startpos_f_), _, (e : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3997 "typ/typ/TYPParser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_field_instanciation = let _startpos = _startpos_f_ in
            
# 290 "typ/typ/TYPParser.mly"
    (
      (make_node _startpos f,e)
    )
# 4006 "typ/typ/TYPParser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv457) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_field_instanciation) = _v in
            ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv455 * _menhir_state * 'tv_field_instanciation) = Obj.magic _menhir_stack in
            ((assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | SEMI ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv449 * _menhir_state * 'tv_field_instanciation) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | LABEL _v ->
                    _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState112) : 'freshtv450)
            | RB ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv451 * _menhir_state * 'tv_field_instanciation) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, (x : 'tv_field_instanciation)) = _menhir_stack in
                let _v : 'tv_separated_nonempty_list_SEMI_field_instanciation_ = 
# 229 "/home/fusroman/.opam/4.06.0/lib/menhir/standard.mly"
    ( [ x ] )
# 4037 "typ/typ/TYPParser.ml"
                 in
                _menhir_goto_separated_nonempty_list_SEMI_field_instanciation_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv452)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv453 * _menhir_state * 'tv_field_instanciation) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv454)) : 'freshtv456)) : 'freshtv458)) : 'freshtv460)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv461 * _menhir_state * (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 4054 "typ/typ/TYPParser.ml"
            ) * Lexing.position)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4058 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv462)) : 'freshtv464)
    | MenhirState55 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv475 * _menhir_state) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4067 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADD ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NEQ ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | RP ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv471 * _menhir_state) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4101 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LS ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv465 * _menhir_state) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4111 "typ/typ/TYPParser.ml"
                ))) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | ADDRESS ->
                    _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState116
                | BOOL _v ->
                    _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _v
                | CPL ->
                    _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState116
                | INT _v ->
                    _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _v
                | LABEL _v ->
                    _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | LB ->
                    _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | LP ->
                    _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState116
                | MULT ->
                    _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | NOT ->
                    _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState116
                | SUB ->
                    _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState116
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState116) : 'freshtv466)
            | ADD | AND | COLON | COMMA | DIV | EQ | GE | GT | LE | LT | MULT | NEQ | OR | RB | REM | RP | RS | SEMI | SUB ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv467 * _menhir_state) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4145 "typ/typ/TYPParser.ml"
                ))) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s), _, (e : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4150 "typ/typ/TYPParser.ml"
                ))) = _menhir_stack in
                let _3 = () in
                let _1 = () in
                let _v : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4157 "typ/typ/TYPParser.ml"
                ) = 
# 227 "typ/typ/TYPParser.mly"
    ( e )
# 4161 "typ/typ/TYPParser.ml"
                 in
                _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv468)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv469 * _menhir_state) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4171 "typ/typ/TYPParser.ml"
                ))) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv470)) : 'freshtv472)
        | SUB ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv473 * _menhir_state) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4184 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv474)) : 'freshtv476)
    | MenhirState116 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv483 * _menhir_state) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4193 "typ/typ/TYPParser.ml"
        )))) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4197 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADD ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NEQ ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | RS ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv479 * _menhir_state) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4231 "typ/typ/TYPParser.ml"
            )))) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4235 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv477 * _menhir_state) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4242 "typ/typ/TYPParser.ml"
            )))) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4246 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s), _, (size : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4251 "typ/typ/TYPParser.ml"
            ))), _, (init_elt : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4255 "typ/typ/TYPParser.ml"
            ))) = _menhir_stack in
            let _6 = () in
            let _4 = () in
            let _3 = () in
            let _1 = () in
            let _v : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4264 "typ/typ/TYPParser.ml"
            ) = 
# 283 "typ/typ/TYPParser.mly"
    (
      NewArray (size,init_elt)
    )
# 4270 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv478)) : 'freshtv480)
        | SUB ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv481 * _menhir_state) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4282 "typ/typ/TYPParser.ml"
            )))) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4286 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv482)) : 'freshtv484)
    | MenhirState54 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv491 * _menhir_state * Lexing.position)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4295 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADD ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NEQ ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | RP ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv487 * _menhir_state * Lexing.position)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4329 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv485 * _menhir_state * Lexing.position)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4336 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, _startpos__1_), _, (l : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4341 "typ/typ/TYPParser.ml"
            ))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _v : (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4350 "typ/typ/TYPParser.ml"
            ) = 
# 299 "typ/typ/TYPParser.mly"
    ( l )
# 4354 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_l_expr _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv486)) : 'freshtv488)
        | SUB ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv489 * _menhir_state * Lexing.position)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4366 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv490)) : 'freshtv492)
    | MenhirState52 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv495 * _menhir_state) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4375 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv493 * _menhir_state) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4381 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _, (e : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4386 "typ/typ/TYPParser.ml"
        ))) = _menhir_stack in
        let _1 = () in
        let _v : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4392 "typ/typ/TYPParser.ml"
        ) = 
# 259 "typ/typ/TYPParser.mly"
    ( Unop(Not, e) )
# 4396 "typ/typ/TYPParser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv494)) : 'freshtv496)
    | MenhirState51 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv501 * _menhir_state) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4404 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIV ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | REM ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | ADD | AND | COLON | COMMA | EQ | GE | GT | LE | LT | NEQ | OR | RB | RP | RS | SEMI | SUB ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv497 * _menhir_state) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4420 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (e : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4425 "typ/typ/TYPParser.ml"
            ))) = _menhir_stack in
            let _1 = () in
            let _v : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4431 "typ/typ/TYPParser.ml"
            ) = 
# 255 "typ/typ/TYPParser.mly"
    ( Unop(Minus, e) )
# 4435 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv498)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv499 * _menhir_state) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4445 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv500)) : 'freshtv502)
    | MenhirState50 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv507 * _menhir_state * Lexing.position)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4454 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADD ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NEQ ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | RP ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv503 * _menhir_state * Lexing.position)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4488 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BREAK ->
                _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | CONTINUE ->
                _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EXIT ->
                _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOR ->
                _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IF ->
                _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LABEL _v ->
                _menhir_run138 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB ->
                _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MULT ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NOP ->
                _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | PRINT ->
                _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RETURN ->
                _menhir_run125 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TFUN ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TINT ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TSTRING ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | WHILE ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState124) : 'freshtv504)
        | SUB ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv505 * _menhir_state * Lexing.position)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4536 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv506)) : 'freshtv508)
    | MenhirState125 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv513 * _menhir_state * Lexing.position) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4545 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADD ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NEQ ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | COLON | COMMA | RP | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv509 * _menhir_state * Lexing.position) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4581 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, _startpos__1_), _, (e : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4586 "typ/typ/TYPParser.ml"
            ))) = _menhir_stack in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _v : (
# 75 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 4593 "typ/typ/TYPParser.ml"
            ) = 
# 320 "typ/typ/TYPParser.mly"
    ( Return e )
# 4597 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv510)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv511 * _menhir_state * Lexing.position) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4607 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv512)) : 'freshtv514)
    | MenhirState128 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv521 * _menhir_state * Lexing.position)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4616 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADD ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NEQ ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | RP ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv517 * _menhir_state * Lexing.position)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4650 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv515 * _menhir_state * Lexing.position)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4657 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, _startpos__1_), _, (e : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4662 "typ/typ/TYPParser.ml"
            ))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _v : (
# 75 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 4671 "typ/typ/TYPParser.ml"
            ) = 
# 308 "typ/typ/TYPParser.mly"
    ( Print e )
# 4675 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv516)) : 'freshtv518)
        | SUB ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv519 * _menhir_state * Lexing.position)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4687 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv520)) : 'freshtv522)
    | MenhirState135 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv527 * _menhir_state * Lexing.position)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4696 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADD ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NEQ ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | RP ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv523 * _menhir_state * Lexing.position)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4730 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BREAK ->
                _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | CONTINUE ->
                _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EXIT ->
                _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOR ->
                _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IF ->
                _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LABEL _v ->
                _menhir_run138 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB ->
                _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MULT ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NOP ->
                _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | PRINT ->
                _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RETURN ->
                _menhir_run125 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TFUN ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TINT ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TSTRING ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | WHILE ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState137) : 'freshtv524)
        | SUB ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv525 * _menhir_state * Lexing.position)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4778 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv526)) : 'freshtv528)
    | MenhirState147 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv547 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 4787 "typ/typ/TYPParser.ml"
        ) * Lexing.position) * (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 4791 "typ/typ/TYPParser.ml"
        ) * Lexing.position)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4795 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADD ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NEQ ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | COLON | COMMA | RP | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv543 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 4831 "typ/typ/TYPParser.ml"
            ) * Lexing.position) * (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 4835 "typ/typ/TYPParser.ml"
            ) * Lexing.position)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4839 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (type_var : (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 4844 "typ/typ/TYPParser.ml"
            )), _startpos_type_var_), (name : (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 4848 "typ/typ/TYPParser.ml"
            )), _startpos_name_), _, (e : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4852 "typ/typ/TYPParser.ml"
            ))) = _menhir_stack in
            let _3 = () in
            let _startpos = _startpos_type_var_ in
            let _v : (
# 69 "typ/typ/TYPParser.mly"
      (TYPTree.variable)
# 4859 "typ/typ/TYPParser.ml"
            ) = let _startpos = _startpos_type_var_ in
            
# 111 "typ/typ/TYPParser.mly"
    (
      (type_var,make_node _startpos name, e)
    )
# 4866 "typ/typ/TYPParser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv541) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 69 "typ/typ/TYPParser.mly"
      (TYPTree.variable)
# 4874 "typ/typ/TYPParser.ml"
            )) = _v in
            let (_startpos : Lexing.position) = _startpos in
            ((let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
            match _menhir_s with
            | MenhirState203 | MenhirState0 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv535 * _menhir_state * (
# 69 "typ/typ/TYPParser.mly"
      (TYPTree.variable)
# 4884 "typ/typ/TYPParser.ml"
                ) * Lexing.position) = Obj.magic _menhir_stack in
                ((assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | SEMI ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : 'freshtv531 * _menhir_state * (
# 69 "typ/typ/TYPParser.mly"
      (TYPTree.variable)
# 4894 "typ/typ/TYPParser.ml"
                    ) * Lexing.position) = Obj.magic _menhir_stack in
                    ((let _menhir_env = _menhir_discard _menhir_env in
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : 'freshtv529 * _menhir_state * (
# 69 "typ/typ/TYPParser.mly"
      (TYPTree.variable)
# 4901 "typ/typ/TYPParser.ml"
                    ) * Lexing.position) = Obj.magic _menhir_stack in
                    ((let (_menhir_stack, _menhir_s, (var : (
# 69 "typ/typ/TYPParser.mly"
      (TYPTree.variable)
# 4906 "typ/typ/TYPParser.ml"
                    )), _startpos_var_) = _menhir_stack in
                    let _2 = () in
                    let _v : 'tv_global_declaration = 
# 189 "typ/typ/TYPParser.mly"
    (
      Var v
    )
# 4914 "typ/typ/TYPParser.ml"
                     in
                    _menhir_goto_global_declaration _menhir_env _menhir_stack _menhir_s _v) : 'freshtv530)) : 'freshtv532)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : 'freshtv533 * _menhir_state * (
# 69 "typ/typ/TYPParser.mly"
      (TYPTree.variable)
# 4924 "typ/typ/TYPParser.ml"
                    ) * Lexing.position) = Obj.magic _menhir_stack in
                    ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv534)) : 'freshtv536)
            | MenhirState48 | MenhirState124 | MenhirState191 | MenhirState185 | MenhirState188 | MenhirState186 | MenhirState183 | MenhirState132 | MenhirState178 | MenhirState137 | MenhirState175 | MenhirState155 | MenhirState153 | MenhirState140 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv539 * _menhir_state * (
# 69 "typ/typ/TYPParser.mly"
      (TYPTree.variable)
# 4933 "typ/typ/TYPParser.ml"
                ) * Lexing.position) = Obj.magic _menhir_stack in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv537 * _menhir_state * (
# 69 "typ/typ/TYPParser.mly"
      (TYPTree.variable)
# 4939 "typ/typ/TYPParser.ml"
                ) * Lexing.position) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, (v : (
# 69 "typ/typ/TYPParser.mly"
      (TYPTree.variable)
# 4944 "typ/typ/TYPParser.ml"
                )), _startpos_v_) = _menhir_stack in
                let _startpos = _startpos_v_ in
                let _v : (
# 75 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 4950 "typ/typ/TYPParser.ml"
                ) = 
# 338 "typ/typ/TYPParser.mly"
    ( 
      Declaration v 
    )
# 4956 "typ/typ/TYPParser.ml"
                 in
                _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv538)) : 'freshtv540)
            | _ ->
                _menhir_fail ()) : 'freshtv542)) : 'freshtv544)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv545 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 4968 "typ/typ/TYPParser.ml"
            ) * Lexing.position) * (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 4972 "typ/typ/TYPParser.ml"
            ) * Lexing.position)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4976 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv546)) : 'freshtv548)
    | MenhirState151 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv553 * _menhir_state * Lexing.position)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_instruction__)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4985 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADD ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NEQ ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv549 * _menhir_state * Lexing.position)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_instruction__)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 5019 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BREAK ->
                _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | CONTINUE ->
                _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EXIT ->
                _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LABEL _v ->
                _menhir_run138 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MULT ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NOP ->
                _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | PRINT ->
                _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RETURN ->
                _menhir_run125 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TFUN ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TINT ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TSTRING ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RP ->
                _menhir_reduce73 _menhir_env (Obj.magic _menhir_stack) MenhirState153
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState153) : 'freshtv550)
        | SUB ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv551 * _menhir_state * Lexing.position)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_instruction__)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 5063 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv552)) : 'freshtv554)
    | MenhirState168 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv559 * _menhir_state * (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 5072 "typ/typ/TYPParser.ml"
        ) * Lexing.position) * 'tv_assign_binop) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 5076 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADD ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NEQ ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | COLON | COMMA | RP | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv555 * _menhir_state * (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 5112 "typ/typ/TYPParser.ml"
            ) * Lexing.position) * 'tv_assign_binop) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 5116 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (l : (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 5121 "typ/typ/TYPParser.ml"
            )), _startpos_l_), (op : 'tv_assign_binop)), _, (e : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 5125 "typ/typ/TYPParser.ml"
            ))) = _menhir_stack in
            let _startpos = _startpos_l_ in
            let _v : (
# 75 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 5131 "typ/typ/TYPParser.ml"
            ) = 
# 323 "typ/typ/TYPParser.mly"
    (
      BinopAssign(l, op, e)
    )
# 5137 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv556)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv557 * _menhir_state * (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 5147 "typ/typ/TYPParser.ml"
            ) * Lexing.position) * 'tv_assign_binop) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 5151 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv558)) : 'freshtv560)
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_COMMA_parameter_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_COMMA_parameter_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState37 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv351) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_COMMA_parameter_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv349) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((x : 'tv_separated_nonempty_list_COMMA_parameter_) : 'tv_separated_nonempty_list_COMMA_parameter_) = _v in
        ((let _v : 'tv_loption_separated_nonempty_list_COMMA_parameter__ = 
# 144 "/home/fusroman/.opam/4.06.0/lib/menhir/standard.mly"
    ( x )
# 5173 "typ/typ/TYPParser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_COMMA_parameter__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv350)) : 'freshtv352)
    | MenhirState44 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv355 * _menhir_state * (
# 70 "typ/typ/TYPParser.mly"
      (TYPTree.parameter)
# 5181 "typ/typ/TYPParser.ml"
        ))) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_COMMA_parameter_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv353 * _menhir_state * (
# 70 "typ/typ/TYPParser.mly"
      (TYPTree.parameter)
# 5189 "typ/typ/TYPParser.ml"
        ))) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((xs : 'tv_separated_nonempty_list_COMMA_parameter_) : 'tv_separated_nonempty_list_COMMA_parameter_) = _v in
        ((let (_menhir_stack, _menhir_s, (x : (
# 70 "typ/typ/TYPParser.mly"
      (TYPTree.parameter)
# 5196 "typ/typ/TYPParser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_separated_nonempty_list_COMMA_parameter_ = 
# 231 "/home/fusroman/.opam/4.06.0/lib/menhir/standard.mly"
    ( x :: xs )
# 5202 "typ/typ/TYPParser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_parameter_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv354)) : 'freshtv356)
    | _ ->
        _menhir_fail ()

and _menhir_reduce64 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (
# 72 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 5213 "typ/typ/TYPParser.ml"
    ) = 
# 216 "typ/typ/TYPParser.mly"
    ( [] )
# 5217 "typ/typ/TYPParser.ml"
     in
    _menhir_goto_instructions _menhir_env _menhir_stack _menhir_s _v

and _menhir_run49 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LP ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv345 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADDRESS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | BOOL _v ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
        | CPL ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | INT _v ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
        | LABEL _v ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LB ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LP ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | MULT ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NOT ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | SUB ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState50) : 'freshtv346)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv347 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv348)

and _menhir_run125 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | BOOL _v ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState125 _v
    | CPL ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | INT _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState125 _v
    | LABEL _v ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState125 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState125 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | MULT ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState125 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | SUB ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState125

and _menhir_run127 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LP ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv341 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADDRESS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | BOOL _v ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _v
        | CPL ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | INT _v ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _v
        | LABEL _v ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LB ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LP ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | MULT ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NOT ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | SUB ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState128) : 'freshtv342)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv343 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv344)

and _menhir_run131 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv339) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_startpos__1_ : Lexing.position) = _startpos in
    ((let _1 = () in
    let _startpos = _startpos__1_ in
    let _v : (
# 75 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 5352 "typ/typ/TYPParser.ml"
    ) = 
# 304 "typ/typ/TYPParser.mly"
    ( Nop )
# 5356 "typ/typ/TYPParser.ml"
     in
    _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv340)

and _menhir_run138 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 5363 "typ/typ/TYPParser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LABEL _ | LS | MULT ->
        _menhir_reduce99 _menhir_env (Obj.magic _menhir_stack)
    | ADDASSIGN | ASSIGN | DECR | DIVASSIGN | INCR | LP | MULTASSIGN | SUBASSIGN ->
        _menhir_reduce65 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv337 * _menhir_state * (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 5381 "typ/typ/TYPParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv338)

and _menhir_run134 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LP ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv333 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADDRESS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | BOOL _v ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState135 _v
        | CPL ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | INT _v ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState135 _v
        | LABEL _v ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState135 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LB ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState135 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LP ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | MULT ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState135 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NOT ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | SUB ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState135) : 'freshtv334)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv335 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv336)

and _menhir_run139 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LP ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv329 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BREAK ->
            _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | CONTINUE ->
            _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | EXIT ->
            _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LABEL _v ->
            _menhir_run138 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LB ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MULT ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NOP ->
            _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | PRINT ->
            _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | RETURN ->
            _menhir_run125 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TFUN ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TINT ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TSTRING ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | SEMI ->
            _menhir_reduce73 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState140) : 'freshtv330)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv331 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv332)

and _menhir_run141 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv327) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_startpos__1_ : Lexing.position) = _startpos in
    ((let _1 = () in
    let _startpos = _startpos__1_ in
    let _v : (
# 75 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 5492 "typ/typ/TYPParser.ml"
    ) = 
# 306 "typ/typ/TYPParser.mly"
    ( Exit )
# 5496 "typ/typ/TYPParser.ml"
     in
    _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv328)

and _menhir_run142 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv325) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_startpos__1_ : Lexing.position) = _startpos in
    ((let _1 = () in
    let _startpos = _startpos__1_ in
    let _v : (
# 75 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 5512 "typ/typ/TYPParser.ml"
    ) = let _startpos = _startpos__1_ in
    
# 315 "typ/typ/TYPParser.mly"
    ( 
      Continue (make_node _startpos ())
    )
# 5519 "typ/typ/TYPParser.ml"
     in
    _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv326)

and _menhir_run143 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv323) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_startpos__1_ : Lexing.position) = _startpos in
    ((let _1 = () in
    let _startpos = _startpos__1_ in
    let _v : (
# 75 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 5535 "typ/typ/TYPParser.ml"
    ) = let _startpos = _startpos__1_ in
    
# 310 "typ/typ/TYPParser.mly"
    ( 
      Break (make_node _startpos ())
    )
# 5542 "typ/typ/TYPParser.ml"
     in
    _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv324)

and _menhir_run51 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | BOOL _v ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
    | CPL ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | INT _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
    | LABEL _v ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | MULT ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | SUB ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState51

and _menhir_run52 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | BOOL _v ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
    | CPL ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | INT _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
    | LABEL _v ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | MULT ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | SUB ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState52

and _menhir_run53 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LP ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv319 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADDRESS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | BOOL _v ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
        | CPL ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | INT _v ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
        | LABEL _v ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LB ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LP ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | MULT ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NOT ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | SUB ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState54) : 'freshtv320)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv321 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv322)

and _menhir_run55 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | BOOL _v ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
    | CPL ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | INT _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
    | LABEL _v ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | MULT ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | SUB ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState55

and _menhir_run56 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LABEL _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState56

and _menhir_run59 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 5699 "typ/typ/TYPParser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce65 _menhir_env (Obj.magic _menhir_stack)

and _menhir_run60 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 63 "typ/typ/TYPParser.mly"
       (int)
# 5709 "typ/typ/TYPParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv317) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((i : (
# 63 "typ/typ/TYPParser.mly"
       (int)
# 5719 "typ/typ/TYPParser.ml"
    )) : (
# 63 "typ/typ/TYPParser.mly"
       (int)
# 5723 "typ/typ/TYPParser.ml"
    )) = _v in
    ((let _v : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 5728 "typ/typ/TYPParser.ml"
    ) = 
# 221 "typ/typ/TYPParser.mly"
    ( Int i )
# 5732 "typ/typ/TYPParser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv318)

and _menhir_run61 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | BOOL _v ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
    | CPL ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | INT _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
    | LABEL _v ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | MULT ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | SUB ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState61

and _menhir_run62 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 62 "typ/typ/TYPParser.mly"
       (bool)
# 5770 "typ/typ/TYPParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv315) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((b : (
# 62 "typ/typ/TYPParser.mly"
       (bool)
# 5780 "typ/typ/TYPParser.ml"
    )) : (
# 62 "typ/typ/TYPParser.mly"
       (bool)
# 5784 "typ/typ/TYPParser.ml"
    )) = _v in
    ((let _v : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 5789 "typ/typ/TYPParser.ml"
    ) = 
# 223 "typ/typ/TYPParser.mly"
    ( Bool b )
# 5793 "typ/typ/TYPParser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv316)

and _menhir_run63 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LABEL _v ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MULT ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState63

and _menhir_goto_global_declaration : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_global_declaration -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv313 * _menhir_state * 'tv_global_declaration) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LABEL _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState203 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState203 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TFUN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState203 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TINT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState203 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TSTRING ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState203 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TYPE ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState203 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | SEMI ->
        _menhir_reduce69 _menhir_env (Obj.magic _menhir_stack) MenhirState203
    | EOF ->
        _menhir_reduce67 _menhir_env (Obj.magic _menhir_stack) MenhirState203
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState203) : 'freshtv314)

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_parameter : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 70 "typ/typ/TYPParser.mly"
      (TYPTree.parameter)
# 5849 "typ/typ/TYPParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv311 * _menhir_state * (
# 70 "typ/typ/TYPParser.mly"
      (TYPTree.parameter)
# 5857 "typ/typ/TYPParser.ml"
    )) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COMMA ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv305 * _menhir_state * (
# 70 "typ/typ/TYPParser.mly"
      (TYPTree.parameter)
# 5867 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LABEL _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LB ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TFUN ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TINT ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TSTRING ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState44) : 'freshtv306)
    | RP ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv307 * _menhir_state * (
# 70 "typ/typ/TYPParser.mly"
      (TYPTree.parameter)
# 5891 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (x : (
# 70 "typ/typ/TYPParser.mly"
      (TYPTree.parameter)
# 5896 "typ/typ/TYPParser.ml"
        ))) = _menhir_stack in
        let _v : 'tv_separated_nonempty_list_COMMA_parameter_ = 
# 229 "/home/fusroman/.opam/4.06.0/lib/menhir/standard.mly"
    ( [ x ] )
# 5901 "typ/typ/TYPParser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_parameter_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv308)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv309 * _menhir_state * (
# 70 "typ/typ/TYPParser.mly"
      (TYPTree.parameter)
# 5911 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv310)) : 'freshtv312)

and _menhir_goto_loption_separated_nonempty_list_COMMA_parameter__ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_loption_separated_nonempty_list_COMMA_parameter__ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ((('freshtv303 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 5923 "typ/typ/TYPParser.ml"
    ) * Lexing.position) * (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 5927 "typ/typ/TYPParser.ml"
    ) * Lexing.position)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_parameter__) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RP ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv299 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 5937 "typ/typ/TYPParser.ml"
        ) * Lexing.position) * (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 5941 "typ/typ/TYPParser.ml"
        ) * Lexing.position)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_parameter__) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LB ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv295 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 5951 "typ/typ/TYPParser.ml"
            ) * Lexing.position) * (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 5955 "typ/typ/TYPParser.ml"
            ) * Lexing.position)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_parameter__)) = Obj.magic _menhir_stack in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_stack = (_menhir_stack, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BREAK ->
                _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | CONTINUE ->
                _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EXIT ->
                _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOR ->
                _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IF ->
                _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LABEL _v ->
                _menhir_run138 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MULT ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NOP ->
                _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | PRINT ->
                _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RETURN ->
                _menhir_run125 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TFUN ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TINT ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TSTRING ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | WHILE ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RB ->
                _menhir_reduce64 _menhir_env (Obj.magic _menhir_stack) MenhirState48
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState48) : 'freshtv296)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv297 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6005 "typ/typ/TYPParser.ml"
            ) * Lexing.position) * (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 6009 "typ/typ/TYPParser.ml"
            ) * Lexing.position)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_parameter__)) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv298)) : 'freshtv300)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv301 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6020 "typ/typ/TYPParser.ml"
        ) * Lexing.position) * (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 6024 "typ/typ/TYPParser.ml"
        ) * Lexing.position)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_parameter__) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv302)) : 'freshtv304)

and _menhir_run147 : _menhir_env -> ('ttv_tail * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6032 "typ/typ/TYPParser.ml"
) * Lexing.position) * (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 6036 "typ/typ/TYPParser.ml"
) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState147
    | BOOL _v ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _v
    | CPL ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState147
    | INT _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _v
    | LABEL _v ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState147
    | MULT ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState147
    | SUB ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState147
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState147

and _menhir_goto_separated_nonempty_list_COMMA_type_expr_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_COMMA_type_expr_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState25 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv289 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6075 "typ/typ/TYPParser.ml"
        ) * Lexing.position)) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_COMMA_type_expr_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv287 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6083 "typ/typ/TYPParser.ml"
        ) * Lexing.position)) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((xs : 'tv_separated_nonempty_list_COMMA_type_expr_) : 'tv_separated_nonempty_list_COMMA_type_expr_) = _v in
        ((let (_menhir_stack, _menhir_s, (x : (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6090 "typ/typ/TYPParser.ml"
        )), _startpos_x_) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_separated_nonempty_list_COMMA_type_expr_ = 
# 231 "/home/fusroman/.opam/4.06.0/lib/menhir/standard.mly"
    ( x :: xs )
# 6096 "typ/typ/TYPParser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_type_expr_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv288)) : 'freshtv290)
    | MenhirState8 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv293) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_COMMA_type_expr_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv291) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((x : 'tv_separated_nonempty_list_COMMA_type_expr_) : 'tv_separated_nonempty_list_COMMA_type_expr_) = _v in
        ((let _v : 'tv_loption_separated_nonempty_list_COMMA_type_expr__ = 
# 144 "/home/fusroman/.opam/4.06.0/lib/menhir/standard.mly"
    ( x )
# 6111 "typ/typ/TYPParser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_COMMA_type_expr__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv292)) : 'freshtv294)
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_SEMI_field_declaration_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_SEMI_field_declaration_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState132 | MenhirState9 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv281 * _menhir_state * Lexing.position) * _menhir_state * 'tv_separated_nonempty_list_SEMI_field_declaration_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RB ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv269 * _menhir_state * Lexing.position) * _menhir_state * 'tv_separated_nonempty_list_SEMI_field_declaration_) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv267 * _menhir_state * Lexing.position) * _menhir_state * 'tv_separated_nonempty_list_SEMI_field_declaration_) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, _startpos__1_), _, (fields : 'tv_separated_nonempty_list_SEMI_field_declaration_)) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _v : (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6140 "typ/typ/TYPParser.ml"
            ) = 
# 162 "typ/typ/TYPParser.mly"
  (
    let e = List.fold_left (fun (acc, i) (s, t) ->
      if StringMap.mem s.contents acc then
         raise (SyntaxError(
           Printf.sprintf "Field '%s' has been declared twice in the same record type" s.contents,
           s.line, s.column
         ))
      else
        ((StringMap.add s.contents (t, i) acc), i + 1)
    ) (StringMap.empty, 0) fields in
    Record e
  )
# 6155 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_type_expr _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv268)) : 'freshtv270)
        | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv277 * _menhir_state * Lexing.position) * _menhir_state * 'tv_separated_nonempty_list_SEMI_field_declaration_) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | RB ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv273 * _menhir_state * Lexing.position) * _menhir_state * 'tv_separated_nonempty_list_SEMI_field_declaration_)) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv271 * _menhir_state * Lexing.position) * _menhir_state * 'tv_separated_nonempty_list_SEMI_field_declaration_)) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s, _startpos__1_), _, (fields : 'tv_separated_nonempty_list_SEMI_field_declaration_)) = _menhir_stack in
                let _4 = () in
                let _3 = () in
                let _1 = () in
                let _startpos = _startpos__1_ in
                let _v : (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6178 "typ/typ/TYPParser.ml"
                ) = 
# 162 "typ/typ/TYPParser.mly"
  (
    let e = List.fold_left (fun (acc, i) (s, t) ->
      if StringMap.mem s.contents acc then
         raise (SyntaxError(
           Printf.sprintf "Field '%s' has been declared twice in the same record type" s.contents,
           s.line, s.column
         ))
      else
        ((StringMap.add s.contents (t, i) acc), i + 1)
    ) (StringMap.empty, 0) fields in
    Record e
  )
# 6193 "typ/typ/TYPParser.ml"
                 in
                _menhir_goto_type_expr _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv272)) : 'freshtv274)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv275 * _menhir_state * Lexing.position) * _menhir_state * 'tv_separated_nonempty_list_SEMI_field_declaration_)) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv276)) : 'freshtv278)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv279 * _menhir_state * Lexing.position) * _menhir_state * 'tv_separated_nonempty_list_SEMI_field_declaration_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv280)) : 'freshtv282)
    | MenhirState22 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv285 * _menhir_state * 'tv_field_declaration)) * _menhir_state * 'tv_separated_nonempty_list_SEMI_field_declaration_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv283 * _menhir_state * 'tv_field_declaration)) * _menhir_state * 'tv_separated_nonempty_list_SEMI_field_declaration_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (x : 'tv_field_declaration)), _, (xs : 'tv_separated_nonempty_list_SEMI_field_declaration_)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_separated_nonempty_list_SEMI_field_declaration_ = 
# 231 "/home/fusroman/.opam/4.06.0/lib/menhir/standard.mly"
    ( x :: xs )
# 6220 "typ/typ/TYPParser.ml"
         in
        _menhir_goto_separated_nonempty_list_SEMI_field_declaration_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv284)) : 'freshtv286)
    | _ ->
        _menhir_fail ()

and _menhir_run14 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6229 "typ/typ/TYPParser.ml"
) * Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv265 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6237 "typ/typ/TYPParser.ml"
    ) * Lexing.position) = Obj.magic _menhir_stack in
    let (_startpos__2_ : Lexing.position) = _startpos in
    ((let (_menhir_stack, _menhir_s, (t : (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6243 "typ/typ/TYPParser.ml"
    )), _startpos_t_) = _menhir_stack in
    let _2 = () in
    let _startpos = _startpos_t_ in
    let _v : (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6250 "typ/typ/TYPParser.ml"
    ) = 
# 157 "typ/typ/TYPParser.mly"
  (
    Pointer t
  )
# 6256 "typ/typ/TYPParser.ml"
     in
    _menhir_goto_type_expr _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv266)

and _menhir_run15 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6263 "typ/typ/TYPParser.ml"
) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RS ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv261 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6274 "typ/typ/TYPParser.ml"
        ) * Lexing.position)) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv259 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6281 "typ/typ/TYPParser.ml"
        ) * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (t : (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6286 "typ/typ/TYPParser.ml"
        )), _startpos_t_) = _menhir_stack in
        let _3 = () in
        let _2 = () in
        let _startpos = _startpos_t_ in
        let _v : (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6294 "typ/typ/TYPParser.ml"
        ) = 
# 153 "typ/typ/TYPParser.mly"
  (
    Array (t)
  )
# 6300 "typ/typ/TYPParser.ml"
         in
        _menhir_goto_type_expr _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv260)) : 'freshtv262)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv263 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6310 "typ/typ/TYPParser.ml"
        ) * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv264)

and _menhir_run11 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 6318 "typ/typ/TYPParser.ml"
) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LABEL _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TFUN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TINT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TSTRING ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState11

and _menhir_goto_program : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 68 "typ/typ/TYPParser.mly"
      (TYPTree.typ_prog TYPTree.program)
# 6342 "typ/typ/TYPParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv257) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : (
# 68 "typ/typ/TYPParser.mly"
      (TYPTree.typ_prog TYPTree.program)
# 6351 "typ/typ/TYPParser.ml"
    )) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv255) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : (
# 68 "typ/typ/TYPParser.mly"
      (TYPTree.typ_prog TYPTree.program)
# 6359 "typ/typ/TYPParser.ml"
    )) : (
# 68 "typ/typ/TYPParser.mly"
      (TYPTree.typ_prog TYPTree.program)
# 6363 "typ/typ/TYPParser.ml"
    )) = _v in
    (Obj.magic _1 : 'freshtv256)) : 'freshtv258)

and _menhir_goto_list_global_declaration_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_list_global_declaration_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv249 * _menhir_state * 'tv_list_global_declaration_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EOF ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv245 * _menhir_state * 'tv_list_global_declaration_) = Obj.magic _menhir_stack in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv243 * _menhir_state * 'tv_list_global_declaration_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (globals : 'tv_list_global_declaration_)) = _menhir_stack in
            let _2 = () in
            let _v : (
# 68 "typ/typ/TYPParser.mly"
      (TYPTree.typ_prog TYPTree.program)
# 6387 "typ/typ/TYPParser.ml"
            ) = 
# 92 "typ/typ/TYPParser.mly"
    (
      let (genv, _type, tree) = make_env globals in
      {genv, _type, tree}
    )
# 6394 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_program _menhir_env _menhir_stack _menhir_s _v) : 'freshtv244)) : 'freshtv246)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv247 * _menhir_state * 'tv_list_global_declaration_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv248)) : 'freshtv250)
    | MenhirState203 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv253 * _menhir_state * 'tv_global_declaration) * _menhir_state * 'tv_list_global_declaration_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv251 * _menhir_state * 'tv_global_declaration) * _menhir_state * 'tv_list_global_declaration_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (x : 'tv_global_declaration)), _, (xs : 'tv_list_global_declaration_)) = _menhir_stack in
        let _v : 'tv_list_global_declaration_ = 
# 201 "/home/fusroman/.opam/4.06.0/lib/menhir/standard.mly"
    ( x :: xs )
# 6413 "typ/typ/TYPParser.ml"
         in
        _menhir_goto_list_global_declaration_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv252)) : 'freshtv254)
    | _ ->
        _menhir_fail ()

and _menhir_goto_list_type_declaration_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_list_type_declaration_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState196 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv233 * _menhir_state * 'tv_type_declaration) * _menhir_state * 'tv_list_type_declaration_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv231 * _menhir_state * 'tv_type_declaration) * _menhir_state * 'tv_list_type_declaration_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (x : 'tv_type_declaration)), _, (xs : 'tv_list_type_declaration_)) = _menhir_stack in
        let _v : 'tv_list_type_declaration_ = 
# 201 "/home/fusroman/.opam/4.06.0/lib/menhir/standard.mly"
    ( x :: xs )
# 6432 "typ/typ/TYPParser.ml"
         in
        _menhir_goto_list_type_declaration_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv232)) : 'freshtv234)
    | MenhirState203 | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv241 * _menhir_state * 'tv_list_type_declaration_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv237 * _menhir_state * 'tv_list_type_declaration_) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv235 * _menhir_state * 'tv_list_type_declaration_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (t : 'tv_list_type_declaration_)) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_global_declaration = 
# 193 "typ/typ/TYPParser.mly"
    ( 
      Type t
    )
# 6454 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_global_declaration _menhir_env _menhir_stack _menhir_s _v) : 'freshtv236)) : 'freshtv238)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv239 * _menhir_state * 'tv_list_type_declaration_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv240)) : 'freshtv242)
    | _ ->
        _menhir_fail ()

and _menhir_goto_type_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6470 "typ/typ/TYPParser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState11 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv167 * _menhir_state * (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 6480 "typ/typ/TYPParser.ml"
        ) * Lexing.position)) * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6484 "typ/typ/TYPParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LS ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | RB | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv163 * _menhir_state * (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 6498 "typ/typ/TYPParser.ml"
            ) * Lexing.position)) * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6502 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (f : (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 6507 "typ/typ/TYPParser.ml"
            )), _startpos_f_), _, (t : (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6511 "typ/typ/TYPParser.ml"
            )), _startpos_t_) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_field_declaration = let _startpos = _startpos_f_ in
            
# 178 "typ/typ/TYPParser.mly"
  (
    ((make_node _startpos f), t)
  )
# 6520 "typ/typ/TYPParser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv161) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_field_declaration) = _v in
            ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv159 * _menhir_state * 'tv_field_declaration) = Obj.magic _menhir_stack in
            ((assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | SEMI ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv153 * _menhir_state * 'tv_field_declaration) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | LABEL _v ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState22) : 'freshtv154)
            | RB ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv155 * _menhir_state * 'tv_field_declaration) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, (x : 'tv_field_declaration)) = _menhir_stack in
                let _v : 'tv_separated_nonempty_list_SEMI_field_declaration_ = 
# 229 "/home/fusroman/.opam/4.06.0/lib/menhir/standard.mly"
    ( [ x ] )
# 6551 "typ/typ/TYPParser.ml"
                 in
                _menhir_goto_separated_nonempty_list_SEMI_field_declaration_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv156)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv157 * _menhir_state * 'tv_field_declaration) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv158)) : 'freshtv160)) : 'freshtv162)) : 'freshtv164)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv165 * _menhir_state * (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 6568 "typ/typ/TYPParser.ml"
            ) * Lexing.position)) * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6572 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv166)) : 'freshtv168)
    | MenhirState25 | MenhirState8 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv175 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6581 "typ/typ/TYPParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COMMA ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv169 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6591 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LABEL _v ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TFUN ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TINT ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TSTRING ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState25) : 'freshtv170)
        | LS ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | ARROW ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv171 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6619 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (x : (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6624 "typ/typ/TYPParser.ml"
            )), _startpos_x_) = _menhir_stack in
            let _v : 'tv_separated_nonempty_list_COMMA_type_expr_ = 
# 229 "/home/fusroman/.opam/4.06.0/lib/menhir/standard.mly"
    ( [ x ] )
# 6629 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_separated_nonempty_list_COMMA_type_expr_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv172)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv173 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6639 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv174)) : 'freshtv176)
    | MenhirState29 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv183 * _menhir_state * Lexing.position)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_type_expr__)) * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6648 "typ/typ/TYPParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LS ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | RP ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv179 * _menhir_state * Lexing.position)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_type_expr__)) * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6662 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv177 * _menhir_state * Lexing.position)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_type_expr__)) * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6669 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, _startpos__1_), _, (xs0 : 'tv_loption_separated_nonempty_list_COMMA_type_expr__)), _, (ty : (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6674 "typ/typ/TYPParser.ml"
            )), _startpos_ty_) = _menhir_stack in
            let _6 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _v : (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6684 "typ/typ/TYPParser.ml"
            ) = let ps =
              let xs = xs0 in
              
# 220 "/home/fusroman/.opam/4.06.0/lib/menhir/standard.mly"
    ( xs )
# 6690 "typ/typ/TYPParser.ml"
              
            in
            
# 145 "typ/typ/TYPParser.mly"
  (
    Fun (ps, ty)
  )
# 6698 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_type_expr _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv178)) : 'freshtv180)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv181 * _menhir_state * Lexing.position)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_type_expr__)) * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6708 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv182)) : 'freshtv184)
    | MenhirState4 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv193 * _menhir_state * Lexing.position) * (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 6717 "typ/typ/TYPParser.ml"
        ) * Lexing.position)) * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6721 "typ/typ/TYPParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LS ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | SEMI | TYPE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv189 * _menhir_state * Lexing.position) * (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 6735 "typ/typ/TYPParser.ml"
            ) * Lexing.position)) * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6739 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, _startpos__1_), (name_type : (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 6744 "typ/typ/TYPParser.ml"
            )), _startpos_name_type_), _, (_type : (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6748 "typ/typ/TYPParser.ml"
            )), _startpos__type_) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_type_declaration = let _startpos = _startpos__1_ in
            
# 105 "typ/typ/TYPParser.mly"
  (
    (make_node _startpos name_type, _type)
  )
# 6758 "typ/typ/TYPParser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv187) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_type_declaration) = _v in
            ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv185 * _menhir_state * 'tv_type_declaration) = Obj.magic _menhir_stack in
            ((assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | TYPE ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState196 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | SEMI ->
                _menhir_reduce69 _menhir_env (Obj.magic _menhir_stack) MenhirState196
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState196) : 'freshtv186)) : 'freshtv188)) : 'freshtv190)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv191 * _menhir_state * Lexing.position) * (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 6785 "typ/typ/TYPParser.ml"
            ) * Lexing.position)) * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6789 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv192)) : 'freshtv194)
    | MenhirState203 | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv205 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6798 "typ/typ/TYPParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LABEL _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv201 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6808 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            let (_v : (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 6813 "typ/typ/TYPParser.ml"
            )) = _v in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_stack = (_menhir_stack, _v, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ASSIGN ->
                _menhir_run147 _menhir_env (Obj.magic _menhir_stack)
            | LP ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv197 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6827 "typ/typ/TYPParser.ml"
                ) * Lexing.position) * (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 6831 "typ/typ/TYPParser.ml"
                ) * Lexing.position) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | LABEL _v ->
                    _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | LB ->
                    _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TFUN ->
                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TINT ->
                    _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TSTRING ->
                    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | RP ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : 'freshtv195) = Obj.magic _menhir_stack in
                    let (_menhir_s : _menhir_state) = MenhirState37 in
                    ((let _v : 'tv_loption_separated_nonempty_list_COMMA_parameter__ = 
# 142 "/home/fusroman/.opam/4.06.0/lib/menhir/standard.mly"
    ( [] )
# 6853 "typ/typ/TYPParser.ml"
                     in
                    _menhir_goto_loption_separated_nonempty_list_COMMA_parameter__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv196)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState37) : 'freshtv198)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv199 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6867 "typ/typ/TYPParser.ml"
                ) * Lexing.position) * (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 6871 "typ/typ/TYPParser.ml"
                ) * Lexing.position) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s, _, _), _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv200)) : 'freshtv202)
        | LS ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv203 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6886 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv204)) : 'freshtv206)
    | MenhirState44 | MenhirState37 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv221 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6895 "typ/typ/TYPParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADDRESS ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv213 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6905 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LABEL _v ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv209 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6915 "typ/typ/TYPParser.ml"
                ) * Lexing.position)) = Obj.magic _menhir_stack in
                let (_v : (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 6920 "typ/typ/TYPParser.ml"
                )) = _v in
                let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv207 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6928 "typ/typ/TYPParser.ml"
                ) * Lexing.position)) = Obj.magic _menhir_stack in
                let ((name : (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 6933 "typ/typ/TYPParser.ml"
                )) : (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 6937 "typ/typ/TYPParser.ml"
                )) = _v in
                let (_startpos_name_ : Lexing.position) = _startpos in
                ((let (_menhir_stack, _menhir_s, (params_type : (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6943 "typ/typ/TYPParser.ml"
                )), _startpos_params_type_) = _menhir_stack in
                let _2 = () in
                let _v : (
# 70 "typ/typ/TYPParser.mly"
      (TYPTree.parameter)
# 6949 "typ/typ/TYPParser.ml"
                ) = let _startpos = _startpos_params_type_ in
                
# 123 "typ/typ/TYPParser.mly"
    (
      {name = make_node _startpos name; reference = true; params_type}
    )
# 6956 "typ/typ/TYPParser.ml"
                 in
                _menhir_goto_parameter _menhir_env _menhir_stack _menhir_s _v) : 'freshtv208)) : 'freshtv210)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv211 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6966 "typ/typ/TYPParser.ml"
                ) * Lexing.position)) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv212)) : 'freshtv214)
        | LABEL _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv217 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6975 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            let (_v : (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 6980 "typ/typ/TYPParser.ml"
            )) = _v in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv215 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6988 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            let ((name : (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 6993 "typ/typ/TYPParser.ml"
            )) : (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 6997 "typ/typ/TYPParser.ml"
            )) = _v in
            let (_startpos_name_ : Lexing.position) = _startpos in
            ((let (_menhir_stack, _menhir_s, (params_type : (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 7003 "typ/typ/TYPParser.ml"
            )), _startpos_params_type_) = _menhir_stack in
            let _v : (
# 70 "typ/typ/TYPParser.mly"
      (TYPTree.parameter)
# 7008 "typ/typ/TYPParser.ml"
            ) = let _startpos = _startpos_params_type_ in
            
# 118 "typ/typ/TYPParser.mly"
    (
      {name = make_node _startpos name; reference = false; params_type}
    )
# 7015 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_parameter _menhir_env _menhir_stack _menhir_s _v) : 'freshtv216)) : 'freshtv218)
        | LS ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv219 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 7029 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv220)) : 'freshtv222)
    | MenhirState48 | MenhirState124 | MenhirState191 | MenhirState185 | MenhirState188 | MenhirState186 | MenhirState183 | MenhirState132 | MenhirState178 | MenhirState137 | MenhirState175 | MenhirState155 | MenhirState153 | MenhirState140 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv229 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 7038 "typ/typ/TYPParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LABEL _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv225 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 7048 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            let (_v : (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 7053 "typ/typ/TYPParser.ml"
            )) = _v in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_stack = (_menhir_stack, _v, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ASSIGN ->
                _menhir_run147 _menhir_env (Obj.magic _menhir_stack)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv223 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 7069 "typ/typ/TYPParser.ml"
                ) * Lexing.position) * (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 7073 "typ/typ/TYPParser.ml"
                ) * Lexing.position) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s, _, _), _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv224)) : 'freshtv226)
        | LS ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv227 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 7088 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv228)) : 'freshtv230)
    | _ ->
        _menhir_fail ()

and _menhir_goto_loption_separated_nonempty_list_COMMA_type_expr__ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_loption_separated_nonempty_list_COMMA_type_expr__ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : (('freshtv151 * _menhir_state * Lexing.position)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_type_expr__) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ARROW ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv147 * _menhir_state * Lexing.position)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_type_expr__) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LABEL _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LB ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TFUN ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TINT ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TSTRING ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState29) : 'freshtv148)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv149 * _menhir_state * Lexing.position)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_type_expr__) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv150)) : 'freshtv152)

and _menhir_run10 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 7134 "typ/typ/TYPParser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COLON ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv145 * _menhir_state * (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 7150 "typ/typ/TYPParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv146)

and _menhir_reduce99 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 7158 "typ/typ/TYPParser.ml"
) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (l : (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 7164 "typ/typ/TYPParser.ml"
    )), _startpos_l_) = _menhir_stack in
    let _startpos = _startpos_l_ in
    let _v : (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 7170 "typ/typ/TYPParser.ml"
    ) = let _startpos = _startpos_l_ in
    
# 149 "typ/typ/TYPParser.mly"
  (
    Unknown (make_node _startpos l)
  )
# 7177 "typ/typ/TYPParser.ml"
     in
    _menhir_goto_type_expr _menhir_env _menhir_stack _menhir_s _v _startpos

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState203 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv21 * _menhir_state * 'tv_global_declaration) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv22)
    | MenhirState196 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv23 * _menhir_state * 'tv_type_declaration) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv24)
    | MenhirState191 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv25 * _menhir_state * (
# 75 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 7199 "typ/typ/TYPParser.ml"
        ) * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv26)
    | MenhirState188 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv27 * _menhir_state * (
# 77 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 7208 "typ/typ/TYPParser.ml"
        ) * Lexing.position) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv28)
    | MenhirState186 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv29 * _menhir_state * (
# 77 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 7217 "typ/typ/TYPParser.ml"
        ) * Lexing.position) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv30)
    | MenhirState185 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv31 * _menhir_state * (
# 77 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 7226 "typ/typ/TYPParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv32)
    | MenhirState183 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv33 * _menhir_state * (
# 75 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 7235 "typ/typ/TYPParser.ml"
        ) * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv34)
    | MenhirState178 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv35 * _menhir_state * Lexing.position)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 7244 "typ/typ/TYPParser.ml"
        ))) * _menhir_state * (
# 76 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 7248 "typ/typ/TYPParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv36)
    | MenhirState175 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv37 * _menhir_state * (
# 75 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 7257 "typ/typ/TYPParser.ml"
        ) * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv38)
    | MenhirState168 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv39 * _menhir_state * (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 7266 "typ/typ/TYPParser.ml"
        ) * Lexing.position) * 'tv_assign_binop) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv40)
    | MenhirState159 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv41 * _menhir_state * (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 7275 "typ/typ/TYPParser.ml"
        ) * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv42)
    | MenhirState155 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((('freshtv43 * _menhir_state * Lexing.position)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_instruction__)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 7284 "typ/typ/TYPParser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_instruction__)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv44)
    | MenhirState153 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv45 * _menhir_state * Lexing.position)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_instruction__)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 7293 "typ/typ/TYPParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv46)
    | MenhirState151 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv47 * _menhir_state * Lexing.position)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_instruction__)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv48)
    | MenhirState147 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv49 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 7307 "typ/typ/TYPParser.ml"
        ) * Lexing.position) * (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 7311 "typ/typ/TYPParser.ml"
        ) * Lexing.position)) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _, _), _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv50)
    | MenhirState140 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv51 * _menhir_state * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv52)
    | MenhirState137 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv53 * _menhir_state * Lexing.position)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 7325 "typ/typ/TYPParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv54)
    | MenhirState135 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv55 * _menhir_state * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv56)
    | MenhirState132 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv57 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv58)
    | MenhirState128 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv59 * _menhir_state * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv60)
    | MenhirState125 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv61 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv62)
    | MenhirState124 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv63 * _menhir_state * Lexing.position)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 7354 "typ/typ/TYPParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv64)
    | MenhirState116 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv65 * _menhir_state) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 7363 "typ/typ/TYPParser.ml"
        )))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv66)
    | MenhirState112 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv67 * _menhir_state * 'tv_field_instanciation)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv68)
    | MenhirState100 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv69 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 7377 "typ/typ/TYPParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv70)
    | MenhirState95 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv71 * _menhir_state * (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 7386 "typ/typ/TYPParser.ml"
        ) * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv72)
    | MenhirState93 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv73 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 7395 "typ/typ/TYPParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv74)
    | MenhirState91 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv75 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 7404 "typ/typ/TYPParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv76)
    | MenhirState89 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv77 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 7413 "typ/typ/TYPParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv78)
    | MenhirState87 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv79 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 7422 "typ/typ/TYPParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv80)
    | MenhirState85 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv81 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 7431 "typ/typ/TYPParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv82)
    | MenhirState83 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv83 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 7440 "typ/typ/TYPParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv84)
    | MenhirState81 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv85 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 7449 "typ/typ/TYPParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv86)
    | MenhirState79 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv87 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 7458 "typ/typ/TYPParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv88)
    | MenhirState77 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv89 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 7467 "typ/typ/TYPParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv90)
    | MenhirState74 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv91 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 7476 "typ/typ/TYPParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv92)
    | MenhirState72 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv93 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 7485 "typ/typ/TYPParser.ml"
        )) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv94)
    | MenhirState70 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv95 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 7494 "typ/typ/TYPParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv96)
    | MenhirState68 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv97 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 7503 "typ/typ/TYPParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv98)
    | MenhirState66 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv99 * _menhir_state * (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 7512 "typ/typ/TYPParser.ml"
        ) * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv100)
    | MenhirState63 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv101 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv102)
    | MenhirState61 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv103 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv104)
    | MenhirState58 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv105 * _menhir_state * (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 7531 "typ/typ/TYPParser.ml"
        ) * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv106)
    | MenhirState56 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv107 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv108)
    | MenhirState55 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv109 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv110)
    | MenhirState54 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv111 * _menhir_state * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv112)
    | MenhirState52 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv113 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv114)
    | MenhirState51 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv115 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv116)
    | MenhirState50 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv117 * _menhir_state * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv118)
    | MenhirState48 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv119 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 7570 "typ/typ/TYPParser.ml"
        ) * Lexing.position) * (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 7574 "typ/typ/TYPParser.ml"
        ) * Lexing.position)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_parameter__)) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv120)
    | MenhirState44 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv121 * _menhir_state * (
# 70 "typ/typ/TYPParser.mly"
      (TYPTree.parameter)
# 7583 "typ/typ/TYPParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv122)
    | MenhirState37 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv123 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 7592 "typ/typ/TYPParser.ml"
        ) * Lexing.position) * (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 7596 "typ/typ/TYPParser.ml"
        ) * Lexing.position)) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _, _), _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv124)
    | MenhirState29 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv125 * _menhir_state * Lexing.position)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_type_expr__)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv126)
    | MenhirState25 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv127 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 7610 "typ/typ/TYPParser.ml"
        ) * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv128)
    | MenhirState22 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv129 * _menhir_state * 'tv_field_declaration)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv130)
    | MenhirState11 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv131 * _menhir_state * (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 7624 "typ/typ/TYPParser.ml"
        ) * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv132)
    | MenhirState9 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv133 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv134)
    | MenhirState8 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv135 * _menhir_state * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv136)
    | MenhirState4 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv137 * _menhir_state * Lexing.position) * (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 7643 "typ/typ/TYPParser.ml"
        ) * Lexing.position)) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv138)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv143) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv141) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState0 in
        let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv139) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_startpos__1_ : Lexing.position) = _startpos in
        ((let _1 = () in
        let _v : (
# 68 "typ/typ/TYPParser.mly"
      (TYPTree.typ_prog TYPTree.program)
# 7662 "typ/typ/TYPParser.ml"
        ) = let _startpos = _startpos__1_ in
        
# 98 "typ/typ/TYPParser.mly"
    ( 
      raise_syntax_error _startpos "Syntax error" 
    )
# 7669 "typ/typ/TYPParser.ml"
         in
        _menhir_goto_program _menhir_env _menhir_stack _menhir_s _v) : 'freshtv140)) : 'freshtv142)) : 'freshtv144)

and _menhir_reduce67 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_list_global_declaration_ = 
# 199 "/home/fusroman/.opam/4.06.0/lib/menhir/standard.mly"
    ( [] )
# 7678 "typ/typ/TYPParser.ml"
     in
    _menhir_goto_list_global_declaration_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce69 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_list_type_declaration_ = 
# 199 "/home/fusroman/.opam/4.06.0/lib/menhir/standard.mly"
    ( [] )
# 7687 "typ/typ/TYPParser.ml"
     in
    _menhir_goto_list_type_declaration_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run2 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LABEL _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv17 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        let (_v : (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 7703 "typ/typ/TYPParser.ml"
        )) = _v in
        let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        ((let _menhir_stack = (_menhir_stack, _v, _startpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ASSIGN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv13 * _menhir_state * Lexing.position) * (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 7715 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LABEL _v ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TFUN ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TINT ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TSTRING ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState4) : 'freshtv14)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv15 * _menhir_state * Lexing.position) * (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 7741 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, _), _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv16)) : 'freshtv18)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv19 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv20)

and _menhir_run5 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv11) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_startpos__1_ : Lexing.position) = _startpos in
    ((let _1 = () in
    let _startpos = _startpos__1_ in
    let _v : (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 7765 "typ/typ/TYPParser.ml"
    ) = 
# 141 "typ/typ/TYPParser.mly"
  (
    Array (Int)
  )
# 7771 "typ/typ/TYPParser.ml"
     in
    _menhir_goto_type_expr _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv12)

and _menhir_run6 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv9) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_startpos__1_ : Lexing.position) = _startpos in
    ((let _1 = () in
    let _startpos = _startpos__1_ in
    let _v : (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 7787 "typ/typ/TYPParser.ml"
    ) = 
# 137 "typ/typ/TYPParser.mly"
  (
    Int
  )
# 7793 "typ/typ/TYPParser.ml"
     in
    _menhir_goto_type_expr _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv10)

and _menhir_run7 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LP ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv5 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LABEL _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LB ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TFUN ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TINT ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TSTRING ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | ARROW ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv3) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState8 in
            ((let _v : 'tv_loption_separated_nonempty_list_COMMA_type_expr__ = 
# 142 "/home/fusroman/.opam/4.06.0/lib/menhir/standard.mly"
    ( [] )
# 7826 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_loption_separated_nonempty_list_COMMA_type_expr__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv4)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState8) : 'freshtv6)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv7 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv8)

and _menhir_run9 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LABEL _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState9

and _menhir_run12 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 7857 "typ/typ/TYPParser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce99 _menhir_env (Obj.magic _menhir_stack)

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
# 68 "typ/typ/TYPParser.mly"
      (TYPTree.typ_prog TYPTree.program)
# 7879 "typ/typ/TYPParser.ml"
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
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TFUN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TINT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TSTRING ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TYPE ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | SEMI ->
        _menhir_reduce69 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | EOF ->
        _menhir_reduce67 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0) : 'freshtv2))

# 233 "/home/fusroman/.opam/4.06.0/lib/menhir/standard.mly"
  

# 7922 "typ/typ/TYPParser.ml"
