
module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | WHILE
    | VAR
    | TYPE
    | TSTRING
    | TINT
    | TFUN
    | TCHAR
    | TBOOL
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
# 40 "typ/typ/TYPParser.ml"
  )
    | INT of (
# 63 "typ/typ/TYPParser.mly"
       (int)
# 45 "typ/typ/TYPParser.ml"
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
# 68 "typ/typ/TYPParser.ml"
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
  | MenhirState138
  | MenhirState136
  | MenhirState133
  | MenhirState129
  | MenhirState126
  | MenhirState125
  | MenhirState117
  | MenhirState110
  | MenhirState100
  | MenhirState98
  | MenhirState96
  | MenhirState94
  | MenhirState92
  | MenhirState90
  | MenhirState88
  | MenhirState86
  | MenhirState84
  | MenhirState82
  | MenhirState80
  | MenhirState78
  | MenhirState76
  | MenhirState74
  | MenhirState69
  | MenhirState67
  | MenhirState65
  | MenhirState63
  | MenhirState60
  | MenhirState58
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


# 197 "typ/typ/TYPParser.ml"

let rec _menhir_goto_control : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 77 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 202 "typ/typ/TYPParser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState125 | MenhirState178 | MenhirState138 | MenhirState155 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv789 * _menhir_state * (
# 77 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 212 "typ/typ/TYPParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv787 * _menhir_state * (
# 77 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 218 "typ/typ/TYPParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (c : (
# 77 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 223 "typ/typ/TYPParser.ml"
        )), _startpos_c_) = _menhir_stack in
        let _v : (
# 76 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 228 "typ/typ/TYPParser.ml"
        ) = 
# 358 "typ/typ/TYPParser.mly"
    ( [c] )
# 232 "typ/typ/TYPParser.ml"
         in
        _menhir_goto_block _menhir_env _menhir_stack _menhir_s _v) : 'freshtv788)) : 'freshtv790)
    | MenhirState48 | MenhirState133 | MenhirState191 | MenhirState185 | MenhirState188 | MenhirState186 | MenhirState183 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv795 * _menhir_state * (
# 77 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 240 "typ/typ/TYPParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BREAK ->
            _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState185 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | COLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv791 * _menhir_state * (
# 77 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 252 "typ/typ/TYPParser.ml"
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
                _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LABEL _v ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MULT ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NOP ->
                _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | PRINT ->
                _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RETURN ->
                _menhir_run126 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TFUN ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TINT ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TSTRING ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | WHILE ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RB ->
                _menhir_reduce61 _menhir_env (Obj.magic _menhir_stack) MenhirState188
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState188) : 'freshtv792)
        | CONTINUE ->
            _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState185 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | EXIT ->
            _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState185 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | FOR ->
            _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState185 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | IF ->
            _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState185 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LABEL _v ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState185 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LB ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState185 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MULT ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState185 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NOP ->
            _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState185 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | PRINT ->
            _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState185 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | RETURN ->
            _menhir_run126 _menhir_env (Obj.magic _menhir_stack) MenhirState185 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv793 * _menhir_state * (
# 77 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 320 "typ/typ/TYPParser.ml"
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
                _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState186 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LABEL _v ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState186 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState186 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MULT ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState186 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NOP ->
                _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState186 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | PRINT ->
                _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState186 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RETURN ->
                _menhir_run126 _menhir_env (Obj.magic _menhir_stack) MenhirState186 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TFUN ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState186 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TINT ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState186 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TSTRING ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState186 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | WHILE ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState186 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RB ->
                _menhir_reduce61 _menhir_env (Obj.magic _menhir_stack) MenhirState186
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState186) : 'freshtv794)
        | TFUN ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState185 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TINT ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState185 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TSTRING ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState185 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | WHILE ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState185 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | RB ->
            _menhir_reduce61 _menhir_env (Obj.magic _menhir_stack) MenhirState185
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState185) : 'freshtv796)
    | _ ->
        _menhir_fail ()

and _menhir_goto_loption_separated_nonempty_list_COMMA_expr__ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_loption_separated_nonempty_list_COMMA_expr__ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState69 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv777 * _menhir_state * (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 389 "typ/typ/TYPParser.ml"
        ) * Lexing.position)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RP ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv773 * _menhir_state * (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 399 "typ/typ/TYPParser.ml"
            ) * Lexing.position)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv771 * _menhir_state * (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 406 "typ/typ/TYPParser.ml"
            ) * Lexing.position)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (f : (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 411 "typ/typ/TYPParser.ml"
            )), _startpos_f_), _, (xs0 : 'tv_loption_separated_nonempty_list_COMMA_expr__)) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 418 "typ/typ/TYPParser.ml"
            ) = let args =
              let xs = xs0 in
              
# 220 "/home/fusroman/.opam/4.06.0/lib/menhir/standard.mly"
    ( xs )
# 424 "typ/typ/TYPParser.ml"
              
            in
            
# 280 "typ/typ/TYPParser.mly"
    ( Call(f, args) )
# 430 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv772)) : 'freshtv774)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv775 * _menhir_state * (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 440 "typ/typ/TYPParser.ml"
            ) * Lexing.position)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv776)) : 'freshtv778)
    | MenhirState159 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv785 * _menhir_state * (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 449 "typ/typ/TYPParser.ml"
        ) * Lexing.position)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RP ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv781 * _menhir_state * (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 459 "typ/typ/TYPParser.ml"
            ) * Lexing.position)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv779 * _menhir_state * (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 466 "typ/typ/TYPParser.ml"
            ) * Lexing.position)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (f : (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 471 "typ/typ/TYPParser.ml"
            )), _startpos_f_), _, (xs0 : 'tv_loption_separated_nonempty_list_COMMA_expr__)) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _startpos = _startpos_f_ in
            let _v : (
# 75 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 479 "typ/typ/TYPParser.ml"
            ) = let args =
              let xs = xs0 in
              
# 220 "/home/fusroman/.opam/4.06.0/lib/menhir/standard.mly"
    ( xs )
# 485 "typ/typ/TYPParser.ml"
              
            in
            
# 330 "typ/typ/TYPParser.mly"
    (
      Call(f, args)
    )
# 493 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv780)) : 'freshtv782)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv783 * _menhir_state * (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 503 "typ/typ/TYPParser.ml"
            ) * Lexing.position)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv784)) : 'freshtv786)
    | _ ->
        _menhir_fail ()

and _menhir_goto_assign_unop : _menhir_env -> 'ttv_tail -> 'tv_assign_unop -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv769 * _menhir_state * (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 516 "typ/typ/TYPParser.ml"
    ) * Lexing.position) = Obj.magic _menhir_stack in
    let (_v : 'tv_assign_unop) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv767 * _menhir_state * (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 523 "typ/typ/TYPParser.ml"
    ) * Lexing.position) = Obj.magic _menhir_stack in
    let ((op : 'tv_assign_unop) : 'tv_assign_unop) = _v in
    ((let (_menhir_stack, _menhir_s, (l : (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 529 "typ/typ/TYPParser.ml"
    )), _startpos_l_) = _menhir_stack in
    let _startpos = _startpos_l_ in
    let _v : (
# 75 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 535 "typ/typ/TYPParser.ml"
    ) = 
# 325 "typ/typ/TYPParser.mly"
    (
      UnopAssign(l, op)
    )
# 541 "typ/typ/TYPParser.ml"
     in
    _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv768)) : 'freshtv770)

and _menhir_goto_assign_binop : _menhir_env -> 'ttv_tail -> 'tv_assign_binop -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv765 * _menhir_state * (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 552 "typ/typ/TYPParser.ml"
    ) * Lexing.position) * 'tv_assign_binop) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState168
    | BOOL _v ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState168 _v
    | CPL ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState168
    | INT _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState168 _v
    | LABEL _v ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState168 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState168 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState168
    | MULT ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState168 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState168
    | SUB ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState168
    | TFUN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState168 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TINT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState168 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TSTRING ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState168 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState168) : 'freshtv766)

and _menhir_reduce70 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_loption_separated_nonempty_list_COMMA_expr__ = 
# 142 "/home/fusroman/.opam/4.06.0/lib/menhir/standard.mly"
    ( [] )
# 593 "typ/typ/TYPParser.ml"
     in
    _menhir_goto_loption_separated_nonempty_list_COMMA_expr__ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run63 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 600 "typ/typ/TYPParser.ml"
) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | BOOL _v ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | CPL ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | INT _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | LABEL _v ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | MULT ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | SUB ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | TFUN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TINT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TSTRING ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState63

and _menhir_run102 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 640 "typ/typ/TYPParser.ml"
) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LABEL _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv761 * _menhir_state * (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 651 "typ/typ/TYPParser.ml"
        ) * Lexing.position)) = Obj.magic _menhir_stack in
        let (_v : (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 656 "typ/typ/TYPParser.ml"
        )) = _v in
        let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv759 * _menhir_state * (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 664 "typ/typ/TYPParser.ml"
        ) * Lexing.position)) = Obj.magic _menhir_stack in
        let ((l : (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 669 "typ/typ/TYPParser.ml"
        )) : (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 673 "typ/typ/TYPParser.ml"
        )) = _v in
        let (_startpos_l_ : Lexing.position) = _startpos in
        ((let (_menhir_stack, _menhir_s, (l_expr : (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 679 "typ/typ/TYPParser.ml"
        )), _startpos_l_expr_) = _menhir_stack in
        let _2 = () in
        let _startpos = _startpos_l_expr_ in
        let _v : (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 686 "typ/typ/TYPParser.ml"
        ) = 
# 227 "typ/typ/TYPParser.mly"
    (
      RecordAccess(l_expr, l)
    )
# 692 "typ/typ/TYPParser.ml"
         in
        _menhir_goto_l_expr _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv760)) : 'freshtv762)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv763 * _menhir_state * (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 702 "typ/typ/TYPParser.ml"
        ) * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv764)

and _menhir_goto_loption_separated_nonempty_list_COMMA_instruction__ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_loption_separated_nonempty_list_COMMA_instruction__ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState140 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv751 * _menhir_state * Lexing.position)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_instruction__) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv747 * _menhir_state * Lexing.position)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_instruction__) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ADDRESS ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState151
            | BOOL _v ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _v
            | CPL ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState151
            | INT _v ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _v
            | LABEL _v ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LP ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState151
            | MULT ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NOT ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState151
            | SUB ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState151
            | TFUN ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TINT ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TSTRING ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState151) : 'freshtv748)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv749 * _menhir_state * Lexing.position)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_instruction__) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv750)) : 'freshtv752)
    | MenhirState153 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv757 * _menhir_state * Lexing.position)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_instruction__)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 765 "typ/typ/TYPParser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_instruction__) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RP ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv753 * _menhir_state * Lexing.position)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_instruction__)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 775 "typ/typ/TYPParser.ml"
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
                _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LABEL _v ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB ->
                _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MULT ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NOP ->
                _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | PRINT ->
                _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RETURN ->
                _menhir_run126 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
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
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState155) : 'freshtv754)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv755 * _menhir_state * Lexing.position)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_instruction__)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 821 "typ/typ/TYPParser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_instruction__) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv756)) : 'freshtv758)
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_COMMA_instruction_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_COMMA_instruction_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState153 | MenhirState140 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv741) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_COMMA_instruction_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv739) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((x : 'tv_separated_nonempty_list_COMMA_instruction_) : 'tv_separated_nonempty_list_COMMA_instruction_) = _v in
        ((let _v : 'tv_loption_separated_nonempty_list_COMMA_instruction__ = 
# 144 "/home/fusroman/.opam/4.06.0/lib/menhir/standard.mly"
    ( x )
# 843 "typ/typ/TYPParser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_COMMA_instruction__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv740)) : 'freshtv742)
    | MenhirState175 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv745 * _menhir_state * (
# 75 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 851 "typ/typ/TYPParser.ml"
        ) * Lexing.position)) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_COMMA_instruction_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv743 * _menhir_state * (
# 75 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 859 "typ/typ/TYPParser.ml"
        ) * Lexing.position)) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((xs : 'tv_separated_nonempty_list_COMMA_instruction_) : 'tv_separated_nonempty_list_COMMA_instruction_) = _v in
        ((let (_menhir_stack, _menhir_s, (x : (
# 75 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 866 "typ/typ/TYPParser.ml"
        )), _startpos_x_) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_separated_nonempty_list_COMMA_instruction_ = 
# 231 "/home/fusroman/.opam/4.06.0/lib/menhir/standard.mly"
    ( x :: xs )
# 872 "typ/typ/TYPParser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_instruction_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv744)) : 'freshtv746)
    | _ ->
        _menhir_fail ()

and _menhir_goto_block : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 76 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 881 "typ/typ/TYPParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState155 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((((('freshtv721 * _menhir_state * Lexing.position)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_instruction__)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 891 "typ/typ/TYPParser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_instruction__)) * _menhir_state * (
# 76 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 895 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((((('freshtv719 * _menhir_state * Lexing.position)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_instruction__)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 901 "typ/typ/TYPParser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_instruction__)) * _menhir_state * (
# 76 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 905 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (((((_menhir_stack, _menhir_s, _startpos__1_), _, (xs0 : 'tv_loption_separated_nonempty_list_COMMA_instruction__)), _, (cond : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 910 "typ/typ/TYPParser.ml"
        ))), _, (xs1 : 'tv_loption_separated_nonempty_list_COMMA_instruction__)), _, (b : (
# 76 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 914 "typ/typ/TYPParser.ml"
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
# 925 "typ/typ/TYPParser.ml"
        ) = let it =
          let xs = xs1 in
          
# 220 "/home/fusroman/.opam/4.06.0/lib/menhir/standard.mly"
    ( xs )
# 931 "typ/typ/TYPParser.ml"
          
        in
        let init =
          let xs = xs0 in
          
# 220 "/home/fusroman/.opam/4.06.0/lib/menhir/standard.mly"
    ( xs )
# 939 "typ/typ/TYPParser.ml"
          
        in
        
# 381 "typ/typ/TYPParser.mly"
    (
      For(init, cond, it, b)
    )
# 947 "typ/typ/TYPParser.ml"
         in
        _menhir_goto_control _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv720)) : 'freshtv722)
    | MenhirState138 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv729 * _menhir_state * Lexing.position)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 955 "typ/typ/TYPParser.ml"
        ))) * _menhir_state * (
# 76 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 959 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ELSE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv723 * _menhir_state * Lexing.position)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 969 "typ/typ/TYPParser.ml"
            ))) * _menhir_state * (
# 76 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 973 "typ/typ/TYPParser.ml"
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
                _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LABEL _v ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB ->
                _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MULT ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NOP ->
                _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | PRINT ->
                _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RETURN ->
                _menhir_run126 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
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
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState178) : 'freshtv724)
        | BREAK | COLON | CONTINUE | EXIT | FOR | IF | LABEL _ | LB | MULT | NOP | PRINT | RB | RETURN | SEMI | TFUN | TINT | TSTRING | WHILE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv725 * _menhir_state * Lexing.position)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 1017 "typ/typ/TYPParser.ml"
            ))) * _menhir_state * (
# 76 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 1021 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, _startpos__1_), _, (e : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 1026 "typ/typ/TYPParser.ml"
            ))), _, (b : (
# 76 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 1030 "typ/typ/TYPParser.ml"
            ))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _v : (
# 77 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 1039 "typ/typ/TYPParser.ml"
            ) = 
# 366 "typ/typ/TYPParser.mly"
    (
      VARTree.If(e, b)
    )
# 1045 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_control _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv726)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv727 * _menhir_state * Lexing.position)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 1055 "typ/typ/TYPParser.ml"
            ))) * _menhir_state * (
# 76 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 1059 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv728)) : 'freshtv730)
    | MenhirState178 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv733 * _menhir_state * Lexing.position)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 1068 "typ/typ/TYPParser.ml"
        ))) * _menhir_state * (
# 76 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 1072 "typ/typ/TYPParser.ml"
        ))) * _menhir_state * (
# 76 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 1076 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv731 * _menhir_state * Lexing.position)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 1082 "typ/typ/TYPParser.ml"
        ))) * _menhir_state * (
# 76 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 1086 "typ/typ/TYPParser.ml"
        ))) * _menhir_state * (
# 76 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 1090 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((((_menhir_stack, _menhir_s, _startpos__1_), _, (c : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 1095 "typ/typ/TYPParser.ml"
        ))), _, (t : (
# 76 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 1099 "typ/typ/TYPParser.ml"
        ))), _, (e : (
# 76 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 1103 "typ/typ/TYPParser.ml"
        ))) = _menhir_stack in
        let _6 = () in
        let _4 = () in
        let _2 = () in
        let _1 = () in
        let _startpos = _startpos__1_ in
        let _v : (
# 77 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 1113 "typ/typ/TYPParser.ml"
        ) = 
# 371 "typ/typ/TYPParser.mly"
    ( 
      IfElse(c, t, e)
    )
# 1119 "typ/typ/TYPParser.ml"
         in
        _menhir_goto_control _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv732)) : 'freshtv734)
    | MenhirState125 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv737 * _menhir_state * Lexing.position)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 1127 "typ/typ/TYPParser.ml"
        ))) * _menhir_state * (
# 76 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 1131 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv735 * _menhir_state * Lexing.position)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 1137 "typ/typ/TYPParser.ml"
        ))) * _menhir_state * (
# 76 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 1141 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s, _startpos__1_), _, (e : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 1146 "typ/typ/TYPParser.ml"
        ))), _, (b : (
# 76 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 1150 "typ/typ/TYPParser.ml"
        ))) = _menhir_stack in
        let _4 = () in
        let _2 = () in
        let _1 = () in
        let _startpos = _startpos__1_ in
        let _v : (
# 77 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 1159 "typ/typ/TYPParser.ml"
        ) = 
# 376 "typ/typ/TYPParser.mly"
    (
      While(e, b)
    )
# 1165 "typ/typ/TYPParser.ml"
         in
        _menhir_goto_control _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv736)) : 'freshtv738)
    | _ ->
        _menhir_fail ()

and _menhir_run133 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BREAK ->
        _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState133 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CONTINUE ->
        _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState133 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | EXIT ->
        _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState133 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FOR ->
        _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState133 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IF ->
        _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState133 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LABEL _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv717) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState133 in
        let (_v : (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 1194 "typ/typ/TYPParser.ml"
        )) = _v in
        let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        ((let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLON ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack)
        | LABEL _ | LS | MULT ->
            _menhir_reduce98 _menhir_env (Obj.magic _menhir_stack)
        | ADDASSIGN | ASSIGN | DECR | DIVASSIGN | DOT | INCR | LP | MULTASSIGN | SUBASSIGN ->
            _menhir_reduce62 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv715 * _menhir_state * (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 1214 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv716)) : 'freshtv718)
    | LB ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState133 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MULT ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState133 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOP ->
        _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState133 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | PRINT ->
        _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState133 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RETURN ->
        _menhir_run126 _menhir_env (Obj.magic _menhir_stack) MenhirState133 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TFUN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState133 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TINT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState133 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TSTRING ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState133 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | WHILE ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState133 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RB ->
        _menhir_reduce61 _menhir_env (Obj.magic _menhir_stack) MenhirState133
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState133

and _menhir_goto_separated_nonempty_list_SEMI_field_instanciation_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_SEMI_field_instanciation_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState65 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv709 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 1252 "typ/typ/TYPParser.ml"
        ) * Lexing.position) * Lexing.position) * _menhir_state * 'tv_separated_nonempty_list_SEMI_field_instanciation_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RB ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv697 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 1262 "typ/typ/TYPParser.ml"
            ) * Lexing.position) * Lexing.position) * _menhir_state * 'tv_separated_nonempty_list_SEMI_field_instanciation_) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv695 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 1269 "typ/typ/TYPParser.ml"
            ) * Lexing.position) * Lexing.position) * _menhir_state * 'tv_separated_nonempty_list_SEMI_field_instanciation_) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (t_e : (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 1274 "typ/typ/TYPParser.ml"
            )), _startpos_t_e_), _startpos__2_), _, (fields : 'tv_separated_nonempty_list_SEMI_field_instanciation_)) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 1281 "typ/typ/TYPParser.ml"
            ) = 
# 283 "typ/typ/TYPParser.mly"
    (
      NewRecord (t_e,fields)
    )
# 1287 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv696)) : 'freshtv698)
        | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv705 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 1295 "typ/typ/TYPParser.ml"
            ) * Lexing.position) * Lexing.position) * _menhir_state * 'tv_separated_nonempty_list_SEMI_field_instanciation_) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | RB ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((('freshtv701 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 1305 "typ/typ/TYPParser.ml"
                ) * Lexing.position) * Lexing.position) * _menhir_state * 'tv_separated_nonempty_list_SEMI_field_instanciation_)) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((('freshtv699 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 1312 "typ/typ/TYPParser.ml"
                ) * Lexing.position) * Lexing.position) * _menhir_state * 'tv_separated_nonempty_list_SEMI_field_instanciation_)) = Obj.magic _menhir_stack in
                ((let (((_menhir_stack, _menhir_s, (t_e : (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 1317 "typ/typ/TYPParser.ml"
                )), _startpos_t_e_), _startpos__2_), _, (fields : 'tv_separated_nonempty_list_SEMI_field_instanciation_)) = _menhir_stack in
                let _5 = () in
                let _4 = () in
                let _2 = () in
                let _v : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 1325 "typ/typ/TYPParser.ml"
                ) = 
# 283 "typ/typ/TYPParser.mly"
    (
      NewRecord (t_e,fields)
    )
# 1331 "typ/typ/TYPParser.ml"
                 in
                _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv700)) : 'freshtv702)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((('freshtv703 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 1341 "typ/typ/TYPParser.ml"
                ) * Lexing.position) * Lexing.position) * _menhir_state * 'tv_separated_nonempty_list_SEMI_field_instanciation_)) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv704)) : 'freshtv706)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv707 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 1352 "typ/typ/TYPParser.ml"
            ) * Lexing.position) * Lexing.position) * _menhir_state * 'tv_separated_nonempty_list_SEMI_field_instanciation_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv708)) : 'freshtv710)
    | MenhirState110 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv713 * _menhir_state * 'tv_field_instanciation)) * _menhir_state * 'tv_separated_nonempty_list_SEMI_field_instanciation_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv711 * _menhir_state * 'tv_field_instanciation)) * _menhir_state * 'tv_separated_nonempty_list_SEMI_field_instanciation_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (x : 'tv_field_instanciation)), _, (xs : 'tv_separated_nonempty_list_SEMI_field_instanciation_)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_separated_nonempty_list_SEMI_field_instanciation_ = 
# 231 "/home/fusroman/.opam/4.06.0/lib/menhir/standard.mly"
    ( x :: xs )
# 1366 "typ/typ/TYPParser.ml"
         in
        _menhir_goto_separated_nonempty_list_SEMI_field_instanciation_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv712)) : 'freshtv714)
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_COMMA_expr_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_COMMA_expr_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState159 | MenhirState69 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv689) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_COMMA_expr_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv687) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((x : 'tv_separated_nonempty_list_COMMA_expr_) : 'tv_separated_nonempty_list_COMMA_expr_) = _v in
        ((let _v : 'tv_loption_separated_nonempty_list_COMMA_expr__ = 
# 144 "/home/fusroman/.opam/4.06.0/lib/menhir/standard.mly"
    ( x )
# 1387 "typ/typ/TYPParser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_COMMA_expr__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv688)) : 'freshtv690)
    | MenhirState98 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv693 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 1395 "typ/typ/TYPParser.ml"
        ))) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_COMMA_expr_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv691 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 1403 "typ/typ/TYPParser.ml"
        ))) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((xs : 'tv_separated_nonempty_list_COMMA_expr_) : 'tv_separated_nonempty_list_COMMA_expr_) = _v in
        ((let (_menhir_stack, _menhir_s, (x : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 1410 "typ/typ/TYPParser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_separated_nonempty_list_COMMA_expr_ = 
# 231 "/home/fusroman/.opam/4.06.0/lib/menhir/standard.mly"
    ( x :: xs )
# 1416 "typ/typ/TYPParser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_expr_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv692)) : 'freshtv694)
    | _ ->
        _menhir_fail ()

and _menhir_run74 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 1425 "typ/typ/TYPParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | BOOL _v ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
    | CPL ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | INT _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
    | LABEL _v ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | MULT ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | SUB ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | TFUN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TINT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TSTRING ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState74

and _menhir_run76 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 1465 "typ/typ/TYPParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | BOOL _v ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
    | CPL ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | INT _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
    | LABEL _v ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | MULT ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | SUB ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | TFUN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TINT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TSTRING ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState76

and _menhir_run82 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 1505 "typ/typ/TYPParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | BOOL _v ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
    | CPL ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | INT _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
    | LABEL _v ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | MULT ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | SUB ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | TFUN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TINT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TSTRING ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState82

and _menhir_run84 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 1545 "typ/typ/TYPParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | BOOL _v ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
    | CPL ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | INT _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
    | LABEL _v ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | MULT ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | SUB ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | TFUN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TINT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TSTRING ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState84

and _menhir_run78 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 1585 "typ/typ/TYPParser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _startpos ->
    let _menhir_stack = (_menhir_stack, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | BOOL _v ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
    | CPL ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | INT _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
    | LABEL _v ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | MULT ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | SUB ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | TFUN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TINT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TSTRING ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState78

and _menhir_run88 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 1626 "typ/typ/TYPParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | BOOL _v ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | CPL ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | INT _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | LABEL _v ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | MULT ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | SUB ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | TFUN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TINT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TSTRING ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState88

and _menhir_run90 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 1666 "typ/typ/TYPParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | BOOL _v ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | CPL ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | INT _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | LABEL _v ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | MULT ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | SUB ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | TFUN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TINT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TSTRING ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState90

and _menhir_run92 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 1706 "typ/typ/TYPParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | BOOL _v ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
    | CPL ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | INT _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
    | LABEL _v ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | MULT ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | SUB ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | TFUN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TINT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TSTRING ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState92

and _menhir_run94 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 1746 "typ/typ/TYPParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | BOOL _v ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
    | CPL ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | INT _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
    | LABEL _v ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | MULT ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | SUB ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | TFUN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TINT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TSTRING ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState94

and _menhir_run96 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 1786 "typ/typ/TYPParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState96
    | BOOL _v ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
    | CPL ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState96
    | INT _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
    | LABEL _v ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState96
    | MULT ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState96
    | SUB ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState96
    | TFUN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TINT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TSTRING ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState96

and _menhir_run80 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 1826 "typ/typ/TYPParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | BOOL _v ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
    | CPL ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | INT _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
    | LABEL _v ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | MULT ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | SUB ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | TFUN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TINT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TSTRING ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState80

and _menhir_run100 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 1866 "typ/typ/TYPParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | BOOL _v ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
    | CPL ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | INT _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
    | LABEL _v ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | MULT ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | SUB ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | TFUN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TINT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TSTRING ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState100

and _menhir_run86 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 1906 "typ/typ/TYPParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | BOOL _v ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
    | CPL ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | INT _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
    | LABEL _v ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | MULT ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | SUB ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | TFUN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TINT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TSTRING ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState86

and _menhir_goto_l_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 1946 "typ/typ/TYPParser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState60 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv643 * _menhir_state) * _menhir_state * (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 1956 "typ/typ/TYPParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack)
        | LS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | ADD | AND | COLON | COMMA | DIV | EQ | GE | GT | LE | LT | MULT | NEQ | OR | RB | REM | RP | RS | SEMI | SUB ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv639 * _menhir_state) * _menhir_state * (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 1970 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (e : (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 1975 "typ/typ/TYPParser.ml"
            )), _startpos_e_) = _menhir_stack in
            let _1 = () in
            let _v : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 1981 "typ/typ/TYPParser.ml"
            ) = 
# 278 "typ/typ/TYPParser.mly"
    ( e )
# 1985 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv640)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv641 * _menhir_state) * _menhir_state * (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 1995 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv642)) : 'freshtv644)
    | MenhirState168 | MenhirState159 | MenhirState151 | MenhirState147 | MenhirState136 | MenhirState129 | MenhirState126 | MenhirState50 | MenhirState51 | MenhirState52 | MenhirState54 | MenhirState117 | MenhirState55 | MenhirState58 | MenhirState63 | MenhirState100 | MenhirState98 | MenhirState96 | MenhirState94 | MenhirState92 | MenhirState90 | MenhirState88 | MenhirState86 | MenhirState84 | MenhirState82 | MenhirState80 | MenhirState78 | MenhirState76 | MenhirState74 | MenhirState69 | MenhirState67 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv651 * _menhir_state * (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 2004 "typ/typ/TYPParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack)
        | LP ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv645 * _menhir_state * (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 2016 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ADDRESS ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState69
            | BOOL _v ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
            | CPL ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState69
            | INT _v ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
            | LABEL _v ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LP ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState69
            | MULT ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NOT ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState69
            | SUB ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState69
            | TFUN ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TINT ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TSTRING ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RP ->
                _menhir_reduce70 _menhir_env (Obj.magic _menhir_stack) MenhirState69
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState69) : 'freshtv646)
        | LS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | ADD | AND | COLON | COMMA | DIV | EQ | GE | GT | LE | LT | MULT | NEQ | OR | RB | REM | RP | RS | SEMI | SUB ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv647 * _menhir_state * (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 2060 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (l : (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 2065 "typ/typ/TYPParser.ml"
            )), _startpos_l_) = _menhir_stack in
            let _v : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 2070 "typ/typ/TYPParser.ml"
            ) = 
# 242 "typ/typ/TYPParser.mly"
    ( Deref l )
# 2074 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv648)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv649 * _menhir_state * (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 2084 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv650)) : 'freshtv652)
    | MenhirState48 | MenhirState125 | MenhirState191 | MenhirState185 | MenhirState188 | MenhirState186 | MenhirState183 | MenhirState133 | MenhirState178 | MenhirState138 | MenhirState140 | MenhirState175 | MenhirState153 | MenhirState155 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv685 * _menhir_state * (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 2093 "typ/typ/TYPParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADDASSIGN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv655) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv653) = Obj.magic _menhir_stack in
            ((let _1 = () in
            let _v : 'tv_assign_binop = 
# 342 "typ/typ/TYPParser.mly"
              ( AddAssign )
# 2108 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_assign_binop _menhir_env _menhir_stack _v) : 'freshtv654)) : 'freshtv656)
        | ASSIGN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv659) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv657) = Obj.magic _menhir_stack in
            ((let _1 = () in
            let _v : 'tv_assign_binop = 
# 341 "typ/typ/TYPParser.mly"
              ( Standard )
# 2121 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_assign_binop _menhir_env _menhir_stack _v) : 'freshtv658)) : 'freshtv660)
        | DECR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv663) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv661) = Obj.magic _menhir_stack in
            ((let _1 = () in
            let _v : 'tv_assign_unop = 
# 350 "typ/typ/TYPParser.mly"
              ( Decr )
# 2134 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_assign_unop _menhir_env _menhir_stack _v) : 'freshtv662)) : 'freshtv664)
        | DIVASSIGN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv667) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv665) = Obj.magic _menhir_stack in
            ((let _1 = () in
            let _v : 'tv_assign_binop = 
# 345 "typ/typ/TYPParser.mly"
              ( DivAssign )
# 2147 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_assign_binop _menhir_env _menhir_stack _v) : 'freshtv666)) : 'freshtv668)
        | DOT ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv671) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv669) = Obj.magic _menhir_stack in
            ((let _1 = () in
            let _v : 'tv_assign_unop = 
# 349 "typ/typ/TYPParser.mly"
              ( Incr )
# 2162 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_assign_unop _menhir_env _menhir_stack _v) : 'freshtv670)) : 'freshtv672)
        | LP ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv673 * _menhir_state * (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 2170 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ADDRESS ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState159
            | BOOL _v ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState159 _v
            | CPL ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState159
            | INT _v ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState159 _v
            | LABEL _v ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState159 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState159 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LP ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState159
            | MULT ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState159 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NOT ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState159
            | SUB ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState159
            | TFUN ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState159 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TINT ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState159 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TSTRING ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState159 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RP ->
                _menhir_reduce70 _menhir_env (Obj.magic _menhir_stack) MenhirState159
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState159) : 'freshtv674)
        | LS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | MULTASSIGN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv677) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv675) = Obj.magic _menhir_stack in
            ((let _1 = () in
            let _v : 'tv_assign_binop = 
# 344 "typ/typ/TYPParser.mly"
              ( MultAssign )
# 2219 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_assign_binop _menhir_env _menhir_stack _v) : 'freshtv676)) : 'freshtv678)
        | SUBASSIGN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv681) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv679) = Obj.magic _menhir_stack in
            ((let _1 = () in
            let _v : 'tv_assign_binop = 
# 343 "typ/typ/TYPParser.mly"
              ( SubAssign )
# 2232 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_assign_binop _menhir_env _menhir_stack _v) : 'freshtv680)) : 'freshtv682)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv683 * _menhir_state * (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 2242 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv684)) : 'freshtv686)
    | _ ->
        _menhir_fail ()

and _menhir_goto_instructions : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 72 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 2252 "typ/typ/TYPParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState133 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv603 * _menhir_state * Lexing.position) * _menhir_state * (
# 72 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 2262 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RB ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv599 * _menhir_state * Lexing.position) * _menhir_state * (
# 72 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 2272 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv597 * _menhir_state * Lexing.position) * _menhir_state * (
# 72 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 2279 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, _startpos__1_), _, (i : (
# 72 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 2284 "typ/typ/TYPParser.ml"
            ))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (
# 76 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 2291 "typ/typ/TYPParser.ml"
            ) = 
# 361 "typ/typ/TYPParser.mly"
    ( i )
# 2295 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_block _menhir_env _menhir_stack _menhir_s _v) : 'freshtv598)) : 'freshtv600)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv601 * _menhir_state * Lexing.position) * _menhir_state * (
# 72 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 2305 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv602)) : 'freshtv604)
    | MenhirState183 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv607 * _menhir_state * (
# 75 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 2314 "typ/typ/TYPParser.ml"
        ) * Lexing.position)) * _menhir_state * (
# 72 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 2318 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv605 * _menhir_state * (
# 75 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 2324 "typ/typ/TYPParser.ml"
        ) * Lexing.position)) * _menhir_state * (
# 72 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 2328 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (i : (
# 75 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 2333 "typ/typ/TYPParser.ml"
        )), _startpos_i_), _, (s : (
# 72 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 2337 "typ/typ/TYPParser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : (
# 72 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 2343 "typ/typ/TYPParser.ml"
        ) = 
# 202 "typ/typ/TYPParser.mly"
    (
      i::s
    )
# 2349 "typ/typ/TYPParser.ml"
         in
        _menhir_goto_instructions _menhir_env _menhir_stack _menhir_s _v) : 'freshtv606)) : 'freshtv608)
    | MenhirState186 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv611 * _menhir_state * (
# 77 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 2357 "typ/typ/TYPParser.ml"
        ) * Lexing.position) * _menhir_state) * _menhir_state * (
# 72 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 2361 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv609 * _menhir_state * (
# 77 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 2367 "typ/typ/TYPParser.ml"
        ) * Lexing.position) * _menhir_state) * _menhir_state * (
# 72 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 2371 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s, (c : (
# 77 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 2376 "typ/typ/TYPParser.ml"
        )), _startpos_c_), _), _, (s : (
# 72 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 2380 "typ/typ/TYPParser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : (
# 72 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 2386 "typ/typ/TYPParser.ml"
        ) = 
# 208 "typ/typ/TYPParser.mly"
    (
      c::s
    )
# 2392 "typ/typ/TYPParser.ml"
         in
        _menhir_goto_instructions _menhir_env _menhir_stack _menhir_s _v) : 'freshtv610)) : 'freshtv612)
    | MenhirState188 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv615 * _menhir_state * (
# 77 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 2400 "typ/typ/TYPParser.ml"
        ) * Lexing.position) * _menhir_state) * _menhir_state * (
# 72 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 2404 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv613 * _menhir_state * (
# 77 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 2410 "typ/typ/TYPParser.ml"
        ) * Lexing.position) * _menhir_state) * _menhir_state * (
# 72 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 2414 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s, (_1 : (
# 77 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 2419 "typ/typ/TYPParser.ml"
        )), _startpos__1_), _), _, (_3 : (
# 72 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 2423 "typ/typ/TYPParser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : (
# 72 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 2429 "typ/typ/TYPParser.ml"
        ) = let _startpos = _startpos__1_ in
        
# 214 "typ/typ/TYPParser.mly"
    (
      raise_syntax_error _startpos "Expected ';', found ':'."
    )
# 2436 "typ/typ/TYPParser.ml"
         in
        _menhir_goto_instructions _menhir_env _menhir_stack _menhir_s _v) : 'freshtv614)) : 'freshtv616)
    | MenhirState185 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv619 * _menhir_state * (
# 77 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 2444 "typ/typ/TYPParser.ml"
        ) * Lexing.position) * _menhir_state * (
# 72 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 2448 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv617 * _menhir_state * (
# 77 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 2454 "typ/typ/TYPParser.ml"
        ) * Lexing.position) * _menhir_state * (
# 72 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 2458 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (c : (
# 77 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 2463 "typ/typ/TYPParser.ml"
        )), _startpos_c_), _, (s : (
# 72 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 2467 "typ/typ/TYPParser.ml"
        ))) = _menhir_stack in
        let _v : (
# 72 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 2472 "typ/typ/TYPParser.ml"
        ) = 
# 208 "typ/typ/TYPParser.mly"
    (
      c::s
    )
# 2478 "typ/typ/TYPParser.ml"
         in
        _menhir_goto_instructions _menhir_env _menhir_stack _menhir_s _v) : 'freshtv618)) : 'freshtv620)
    | MenhirState191 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv623 * _menhir_state * (
# 75 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 2486 "typ/typ/TYPParser.ml"
        ) * Lexing.position)) * _menhir_state * (
# 72 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 2490 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv621 * _menhir_state * (
# 75 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 2496 "typ/typ/TYPParser.ml"
        ) * Lexing.position)) * _menhir_state * (
# 72 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 2500 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (_1 : (
# 75 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 2505 "typ/typ/TYPParser.ml"
        )), _startpos__1_), _, (_3 : (
# 72 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 2509 "typ/typ/TYPParser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : (
# 72 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 2515 "typ/typ/TYPParser.ml"
        ) = let _startpos = _startpos__1_ in
        
# 214 "typ/typ/TYPParser.mly"
    (
      raise_syntax_error _startpos "Expected ';', found ':'."
    )
# 2522 "typ/typ/TYPParser.ml"
         in
        _menhir_goto_instructions _menhir_env _menhir_stack _menhir_s _v) : 'freshtv622)) : 'freshtv624)
    | MenhirState48 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv637 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 2530 "typ/typ/TYPParser.ml"
        ) * Lexing.position) * (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 2534 "typ/typ/TYPParser.ml"
        ) * Lexing.position)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_parameter__)) * Lexing.position) * _menhir_state * (
# 72 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 2538 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RB ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv633 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 2548 "typ/typ/TYPParser.ml"
            ) * Lexing.position) * (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 2552 "typ/typ/TYPParser.ml"
            ) * Lexing.position)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_parameter__)) * Lexing.position) * _menhir_state * (
# 72 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 2556 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv631 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 2563 "typ/typ/TYPParser.ml"
            ) * Lexing.position) * (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 2567 "typ/typ/TYPParser.ml"
            ) * Lexing.position)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_parameter__)) * Lexing.position) * _menhir_state * (
# 72 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 2571 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (((((_menhir_stack, _menhir_s, (return_type : (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 2576 "typ/typ/TYPParser.ml"
            )), _startpos_return_type_), (name : (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 2580 "typ/typ/TYPParser.ml"
            )), _startpos_name_), _, (xs0 : 'tv_loption_separated_nonempty_list_COMMA_parameter__)), _startpos__6_), _, (b : (
# 72 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 2584 "typ/typ/TYPParser.ml"
            ))) = _menhir_stack in
            let _8 = () in
            let _6 = () in
            let _5 = () in
            let _3 = () in
            let _v : (
# 71 "typ/typ/TYPParser.mly"
      (TYPTree.typ_function)
# 2593 "typ/typ/TYPParser.ml"
            ) = let params =
              let xs = xs0 in
              
# 220 "/home/fusroman/.opam/4.06.0/lib/menhir/standard.mly"
    ( xs )
# 2599 "typ/typ/TYPParser.ml"
              
            in
            let _startpos = _startpos_return_type_ in
            
# 147 "typ/typ/TYPParser.mly"
    (
      {name = make_node _startpos name; params; block = b; return_type}
    )
# 2608 "typ/typ/TYPParser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv629) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 71 "typ/typ/TYPParser.mly"
      (TYPTree.typ_function)
# 2616 "typ/typ/TYPParser.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv627) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 71 "typ/typ/TYPParser.mly"
      (TYPTree.typ_function)
# 2624 "typ/typ/TYPParser.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv625) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let ((def_fun : (
# 71 "typ/typ/TYPParser.mly"
      (TYPTree.typ_function)
# 2632 "typ/typ/TYPParser.ml"
            )) : (
# 71 "typ/typ/TYPParser.mly"
      (TYPTree.typ_function)
# 2636 "typ/typ/TYPParser.ml"
            )) = _v in
            ((let _v : 'tv_global_declaration = 
# 105 "typ/typ/TYPParser.mly"
    (
      Fun f
    )
# 2643 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_global_declaration _menhir_env _menhir_stack _menhir_s _v) : 'freshtv626)) : 'freshtv628)) : 'freshtv630)) : 'freshtv632)) : 'freshtv634)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv635 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 2653 "typ/typ/TYPParser.ml"
            ) * Lexing.position) * (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 2657 "typ/typ/TYPParser.ml"
            ) * Lexing.position)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_parameter__)) * Lexing.position) * _menhir_state * (
# 72 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 2661 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv636)) : 'freshtv638)
    | _ ->
        _menhir_fail ()

and _menhir_reduce72 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_loption_separated_nonempty_list_COMMA_instruction__ = 
# 142 "/home/fusroman/.opam/4.06.0/lib/menhir/standard.mly"
    ( [] )
# 2673 "typ/typ/TYPParser.ml"
     in
    _menhir_goto_loption_separated_nonempty_list_COMMA_instruction__ _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_instruction : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 75 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 2680 "typ/typ/TYPParser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState125 | MenhirState178 | MenhirState138 | MenhirState155 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv579 * _menhir_state * (
# 75 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 2690 "typ/typ/TYPParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv575 * _menhir_state * (
# 75 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 2700 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv573 * _menhir_state * (
# 75 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 2707 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (i : (
# 75 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 2712 "typ/typ/TYPParser.ml"
            )), _startpos_i_) = _menhir_stack in
            let _2 = () in
            let _v : (
# 76 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 2718 "typ/typ/TYPParser.ml"
            ) = 
# 355 "typ/typ/TYPParser.mly"
    ( [i] )
# 2722 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_block _menhir_env _menhir_stack _menhir_s _v) : 'freshtv574)) : 'freshtv576)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv577 * _menhir_state * (
# 75 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 2732 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv578)) : 'freshtv580)
    | MenhirState140 | MenhirState175 | MenhirState153 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv587 * _menhir_state * (
# 75 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 2741 "typ/typ/TYPParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COMMA ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv581 * _menhir_state * (
# 75 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 2751 "typ/typ/TYPParser.ml"
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
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState175 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState175 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MULT ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState175 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NOP ->
                _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState175 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | PRINT ->
                _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState175 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RETURN ->
                _menhir_run126 _menhir_env (Obj.magic _menhir_stack) MenhirState175 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TFUN ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState175 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TINT ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState175 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TSTRING ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState175 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState175) : 'freshtv582)
        | RP | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv583 * _menhir_state * (
# 75 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 2789 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (x : (
# 75 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 2794 "typ/typ/TYPParser.ml"
            )), _startpos_x_) = _menhir_stack in
            let _v : 'tv_separated_nonempty_list_COMMA_instruction_ = 
# 229 "/home/fusroman/.opam/4.06.0/lib/menhir/standard.mly"
    ( [ x ] )
# 2799 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_separated_nonempty_list_COMMA_instruction_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv584)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv585 * _menhir_state * (
# 75 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 2809 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv586)) : 'freshtv588)
    | MenhirState48 | MenhirState191 | MenhirState185 | MenhirState188 | MenhirState186 | MenhirState183 | MenhirState133 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv595 * _menhir_state * (
# 75 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 2818 "typ/typ/TYPParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv589 * _menhir_state * (
# 75 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 2828 "typ/typ/TYPParser.ml"
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
                _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState191 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LABEL _v ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState191 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState191 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MULT ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState191 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NOP ->
                _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState191 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | PRINT ->
                _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState191 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RETURN ->
                _menhir_run126 _menhir_env (Obj.magic _menhir_stack) MenhirState191 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TFUN ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState191 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TINT ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState191 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TSTRING ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState191 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | WHILE ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState191 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RB ->
                _menhir_reduce61 _menhir_env (Obj.magic _menhir_stack) MenhirState191
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState191) : 'freshtv590)
        | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv591 * _menhir_state * (
# 75 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 2874 "typ/typ/TYPParser.ml"
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
                _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState183 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LABEL _v ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState183 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState183 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MULT ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState183 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NOP ->
                _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState183 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | PRINT ->
                _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState183 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RETURN ->
                _menhir_run126 _menhir_env (Obj.magic _menhir_stack) MenhirState183 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TFUN ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState183 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TINT ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState183 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TSTRING ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState183 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | WHILE ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState183 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RB ->
                _menhir_reduce61 _menhir_env (Obj.magic _menhir_stack) MenhirState183
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState183) : 'freshtv592)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv593 * _menhir_state * (
# 75 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 2922 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv594)) : 'freshtv596)
    | _ ->
        _menhir_fail ()

and _menhir_goto_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 2932 "typ/typ/TYPParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState159 | MenhirState98 | MenhirState69 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv375 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 2942 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADD ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack)
        | COMMA ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv369 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 2956 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ADDRESS ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState98
            | BOOL _v ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _v
            | CPL ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState98
            | INT _v ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _v
            | LABEL _v ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LP ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState98
            | MULT ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NOT ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState98
            | SUB ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState98
            | TFUN ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TINT ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TSTRING ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState98) : 'freshtv370)
        | DIV ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NEQ ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | RP ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv371 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3018 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (x : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3023 "typ/typ/TYPParser.ml"
            ))) = _menhir_stack in
            let _v : 'tv_separated_nonempty_list_COMMA_expr_ = 
# 229 "/home/fusroman/.opam/4.06.0/lib/menhir/standard.mly"
    ( [ x ] )
# 3028 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_separated_nonempty_list_COMMA_expr_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv372)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv373 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3038 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv374)) : 'freshtv376)
    | MenhirState74 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv381 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3047 "typ/typ/TYPParser.ml"
        ))) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3051 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIV ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | REM ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | ADD | AND | COLON | COMMA | EQ | GE | GT | LE | LT | NEQ | OR | RB | RP | RS | SEMI | SUB ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv377 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3067 "typ/typ/TYPParser.ml"
            ))) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3071 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (e1 : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3076 "typ/typ/TYPParser.ml"
            ))), _, (e2 : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3080 "typ/typ/TYPParser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3086 "typ/typ/TYPParser.ml"
            ) = 
# 248 "typ/typ/TYPParser.mly"
    ( Binop(e1, Sub, e2) )
# 3090 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv378)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv379 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3100 "typ/typ/TYPParser.ml"
            ))) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3104 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv380)) : 'freshtv382)
    | MenhirState76 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv385 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3113 "typ/typ/TYPParser.ml"
        ))) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3117 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv383 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3123 "typ/typ/TYPParser.ml"
        ))) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3127 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (e1 : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3132 "typ/typ/TYPParser.ml"
        ))), _, (e2 : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3136 "typ/typ/TYPParser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3142 "typ/typ/TYPParser.ml"
        ) = 
# 254 "typ/typ/TYPParser.mly"
    ( Binop(e1, Rem, e2) )
# 3146 "typ/typ/TYPParser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv384)) : 'freshtv386)
    | MenhirState78 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv389 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3154 "typ/typ/TYPParser.ml"
        )) * Lexing.position) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3158 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv387 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3164 "typ/typ/TYPParser.ml"
        )) * Lexing.position) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3168 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s, (e1 : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3173 "typ/typ/TYPParser.ml"
        ))), _startpos__2_), _, (e2 : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3177 "typ/typ/TYPParser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3183 "typ/typ/TYPParser.ml"
        ) = 
# 250 "typ/typ/TYPParser.mly"
    ( Binop(e1, Mult, e2) )
# 3187 "typ/typ/TYPParser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv388)) : 'freshtv390)
    | MenhirState80 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv393 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3195 "typ/typ/TYPParser.ml"
        ))) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3199 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv391 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3205 "typ/typ/TYPParser.ml"
        ))) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3209 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (e1 : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3214 "typ/typ/TYPParser.ml"
        ))), _, (e2 : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3218 "typ/typ/TYPParser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3224 "typ/typ/TYPParser.ml"
        ) = 
# 252 "typ/typ/TYPParser.mly"
    ( Binop(e1, Div, e2) )
# 3228 "typ/typ/TYPParser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv392)) : 'freshtv394)
    | MenhirState82 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv399 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3236 "typ/typ/TYPParser.ml"
        ))) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3240 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADD ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NEQ ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | AND | COLON | COMMA | OR | RB | RP | RS | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv395 * _menhir_state * (
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
# 258 "typ/typ/TYPParser.mly"
    ( Binop(e1, Or, e2) )
# 3295 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv396)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv397 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3305 "typ/typ/TYPParser.ml"
            ))) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3309 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv398)) : 'freshtv400)
    | MenhirState84 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv405 * _menhir_state * (
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
        | ADD ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | REM ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | AND | COLON | COMMA | EQ | GE | GT | LE | LT | NEQ | OR | RB | RP | RS | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv401 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3342 "typ/typ/TYPParser.ml"
            ))) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3346 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (e1 : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3351 "typ/typ/TYPParser.ml"
            ))), _, (e2 : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3355 "typ/typ/TYPParser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3361 "typ/typ/TYPParser.ml"
            ) = 
# 270 "typ/typ/TYPParser.mly"
    ( Binop(e1, Neq, e2) )
# 3365 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv402)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv403 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3375 "typ/typ/TYPParser.ml"
            ))) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3379 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv404)) : 'freshtv406)
    | MenhirState86 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv411 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3388 "typ/typ/TYPParser.ml"
        ))) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3392 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIV ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | REM ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | ADD | AND | COLON | COMMA | EQ | GE | GT | LE | LT | NEQ | OR | RB | RP | RS | SEMI | SUB ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv407 * _menhir_state * (
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
# 246 "typ/typ/TYPParser.mly"
    ( Binop(e1, Add, e2) )
# 3431 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv408)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv409 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3441 "typ/typ/TYPParser.ml"
            ))) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3445 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv410)) : 'freshtv412)
    | MenhirState88 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv417 * _menhir_state * (
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
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | REM ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | AND | COLON | COMMA | EQ | GE | GT | LE | LT | NEQ | OR | RB | RP | RS | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv413 * _menhir_state * (
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
# 260 "typ/typ/TYPParser.mly"
    ( Binop(e1, Lt, e2) )
# 3501 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv414)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv415 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3511 "typ/typ/TYPParser.ml"
            ))) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3515 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv416)) : 'freshtv418)
    | MenhirState90 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv423 * _menhir_state * (
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
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | REM ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | AND | COLON | COMMA | EQ | GE | GT | LE | LT | NEQ | OR | RB | RP | RS | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv419 * _menhir_state * (
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
# 262 "typ/typ/TYPParser.mly"
    ( Binop(e1, Le, e2) )
# 3571 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv420)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv421 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3581 "typ/typ/TYPParser.ml"
            ))) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3585 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv422)) : 'freshtv424)
    | MenhirState92 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv429 * _menhir_state * (
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
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | REM ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | AND | COLON | COMMA | EQ | GE | GT | LE | LT | NEQ | OR | RB | RP | RS | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv425 * _menhir_state * (
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
# 264 "typ/typ/TYPParser.mly"
    ( Binop(e1, Gt, e2) )
# 3641 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv426)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv427 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3651 "typ/typ/TYPParser.ml"
            ))) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3655 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv428)) : 'freshtv430)
    | MenhirState94 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv435 * _menhir_state * (
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
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | REM ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | AND | COLON | COMMA | EQ | GE | GT | LE | LT | NEQ | OR | RB | RP | RS | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv431 * _menhir_state * (
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
# 266 "typ/typ/TYPParser.mly"
    ( Binop(e1, Ge, e2) )
# 3711 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv432)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv433 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3721 "typ/typ/TYPParser.ml"
            ))) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3725 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv434)) : 'freshtv436)
    | MenhirState96 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv441 * _menhir_state * (
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
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | REM ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | AND | COLON | COMMA | EQ | GE | GT | LE | LT | NEQ | OR | RB | RP | RS | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv437 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3758 "typ/typ/TYPParser.ml"
            ))) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3762 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (e1 : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3767 "typ/typ/TYPParser.ml"
            ))), _, (e2 : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3771 "typ/typ/TYPParser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3777 "typ/typ/TYPParser.ml"
            ) = 
# 268 "typ/typ/TYPParser.mly"
    ( Binop(e1, Eq, e2) )
# 3781 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv438)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv439 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3791 "typ/typ/TYPParser.ml"
            ))) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3795 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv440)) : 'freshtv442)
    | MenhirState100 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv447 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3804 "typ/typ/TYPParser.ml"
        ))) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3808 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADD ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NEQ ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | AND | COLON | COMMA | OR | RB | RP | RS | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv443 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3840 "typ/typ/TYPParser.ml"
            ))) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3844 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (e1 : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3849 "typ/typ/TYPParser.ml"
            ))), _, (e2 : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3853 "typ/typ/TYPParser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3859 "typ/typ/TYPParser.ml"
            ) = 
# 256 "typ/typ/TYPParser.mly"
    ( Binop(e1, And, e2) )
# 3863 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv444)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv445 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3873 "typ/typ/TYPParser.ml"
            ))) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3877 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv446)) : 'freshtv448)
    | MenhirState67 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv463 * _menhir_state * (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 3886 "typ/typ/TYPParser.ml"
        ) * Lexing.position)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3890 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADD ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NEQ ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | RB | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv459 * _menhir_state * (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 3926 "typ/typ/TYPParser.ml"
            ) * Lexing.position)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3930 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (f : (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 3935 "typ/typ/TYPParser.ml"
            )), _startpos_f_), _, (e : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3939 "typ/typ/TYPParser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_field_instanciation = let _startpos = _startpos_f_ in
            
# 294 "typ/typ/TYPParser.mly"
    (
      (make_node _startpos f,e)
    )
# 3948 "typ/typ/TYPParser.ml"
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
                    _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState110) : 'freshtv450)
            | RB ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv451 * _menhir_state * 'tv_field_instanciation) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, (x : 'tv_field_instanciation)) = _menhir_stack in
                let _v : 'tv_separated_nonempty_list_SEMI_field_instanciation_ = 
# 229 "/home/fusroman/.opam/4.06.0/lib/menhir/standard.mly"
    ( [ x ] )
# 3979 "typ/typ/TYPParser.ml"
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
# 3996 "typ/typ/TYPParser.ml"
            ) * Lexing.position)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4000 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv462)) : 'freshtv464)
    | MenhirState63 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv471 * _menhir_state * (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4009 "typ/typ/TYPParser.ml"
        ) * Lexing.position)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4013 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADD ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NEQ ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | RS ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv467 * _menhir_state * (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4047 "typ/typ/TYPParser.ml"
            ) * Lexing.position)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4051 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv465 * _menhir_state * (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4058 "typ/typ/TYPParser.ml"
            ) * Lexing.position)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4062 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (l_expr : (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4067 "typ/typ/TYPParser.ml"
            )), _startpos_l_expr_), _, (e : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4071 "typ/typ/TYPParser.ml"
            ))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _startpos = _startpos_l_expr_ in
            let _v : (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4079 "typ/typ/TYPParser.ml"
            ) = 
# 231 "typ/typ/TYPParser.mly"
    (
      ArrayAccess (l_expr, e)
    )
# 4085 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_l_expr _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv466)) : 'freshtv468)
        | SUB ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv469 * _menhir_state * (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4097 "typ/typ/TYPParser.ml"
            ) * Lexing.position)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4101 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv470)) : 'freshtv472)
    | MenhirState58 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv475 * _menhir_state) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4110 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv473 * _menhir_state) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4116 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _, (e : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4121 "typ/typ/TYPParser.ml"
        ))) = _menhir_stack in
        let _1 = () in
        let _v : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4127 "typ/typ/TYPParser.ml"
        ) = 
# 274 "typ/typ/TYPParser.mly"
    ( Unop(Cpl, e) )
# 4131 "typ/typ/TYPParser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv474)) : 'freshtv476)
    | MenhirState55 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv487 * _menhir_state) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4139 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADD ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NEQ ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | RP ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv483 * _menhir_state) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4173 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LS ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv477 * _menhir_state) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4183 "typ/typ/TYPParser.ml"
                ))) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | ADDRESS ->
                    _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState117
                | BOOL _v ->
                    _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _v
                | CPL ->
                    _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState117
                | INT _v ->
                    _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _v
                | LABEL _v ->
                    _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | LB ->
                    _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | LP ->
                    _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState117
                | MULT ->
                    _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | NOT ->
                    _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState117
                | SUB ->
                    _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState117
                | TFUN ->
                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TINT ->
                    _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TSTRING ->
                    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState117) : 'freshtv478)
            | ADD | AND | COLON | COMMA | DIV | EQ | GE | GT | LE | LT | MULT | NEQ | OR | RB | REM | RP | RS | SEMI | SUB ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv479 * _menhir_state) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4223 "typ/typ/TYPParser.ml"
                ))) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s), _, (e : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4228 "typ/typ/TYPParser.ml"
                ))) = _menhir_stack in
                let _3 = () in
                let _1 = () in
                let _v : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4235 "typ/typ/TYPParser.ml"
                ) = 
# 244 "typ/typ/TYPParser.mly"
    ( e )
# 4239 "typ/typ/TYPParser.ml"
                 in
                _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv480)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv481 * _menhir_state) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4249 "typ/typ/TYPParser.ml"
                ))) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv482)) : 'freshtv484)
        | SUB ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv485 * _menhir_state) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4262 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv486)) : 'freshtv488)
    | MenhirState117 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv495 * _menhir_state) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4271 "typ/typ/TYPParser.ml"
        )))) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4275 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADD ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NEQ ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | RS ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv491 * _menhir_state) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4309 "typ/typ/TYPParser.ml"
            )))) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4313 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv489 * _menhir_state) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4320 "typ/typ/TYPParser.ml"
            )))) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4324 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s), _, (size : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4329 "typ/typ/TYPParser.ml"
            ))), _, (init_elt : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4333 "typ/typ/TYPParser.ml"
            ))) = _menhir_stack in
            let _6 = () in
            let _4 = () in
            let _3 = () in
            let _1 = () in
            let _v : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4342 "typ/typ/TYPParser.ml"
            ) = 
# 287 "typ/typ/TYPParser.mly"
    (
      NewArray (size,init_elt)
    )
# 4348 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv490)) : 'freshtv492)
        | SUB ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv493 * _menhir_state) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4360 "typ/typ/TYPParser.ml"
            )))) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4364 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv494)) : 'freshtv496)
    | MenhirState54 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv503 * _menhir_state * Lexing.position)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4373 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADD ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NEQ ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | RP ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv499 * _menhir_state * Lexing.position)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4407 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv497 * _menhir_state * Lexing.position)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4414 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, _startpos__1_), _, (l : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4419 "typ/typ/TYPParser.ml"
            ))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _v : (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4428 "typ/typ/TYPParser.ml"
            ) = 
# 225 "typ/typ/TYPParser.mly"
    ( l )
# 4432 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_l_expr _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv498)) : 'freshtv500)
        | SUB ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv501 * _menhir_state * Lexing.position)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4444 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv502)) : 'freshtv504)
    | MenhirState52 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv507 * _menhir_state) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4453 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv505 * _menhir_state) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4459 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _, (e : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4464 "typ/typ/TYPParser.ml"
        ))) = _menhir_stack in
        let _1 = () in
        let _v : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4470 "typ/typ/TYPParser.ml"
        ) = 
# 276 "typ/typ/TYPParser.mly"
    ( Unop(Not, e) )
# 4474 "typ/typ/TYPParser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv506)) : 'freshtv508)
    | MenhirState51 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv513 * _menhir_state) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4482 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIV ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | REM ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | ADD | AND | COLON | COMMA | EQ | GE | GT | LE | LT | NEQ | OR | RB | RP | RS | SEMI | SUB ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv509 * _menhir_state) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4498 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (e : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4503 "typ/typ/TYPParser.ml"
            ))) = _menhir_stack in
            let _1 = () in
            let _v : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4509 "typ/typ/TYPParser.ml"
            ) = 
# 272 "typ/typ/TYPParser.mly"
    ( Unop(Minus, e) )
# 4513 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv510)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv511 * _menhir_state) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4523 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv512)) : 'freshtv514)
    | MenhirState50 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv519 * _menhir_state * Lexing.position)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4532 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADD ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NEQ ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | RP ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv515 * _menhir_state * Lexing.position)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4566 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BREAK ->
                _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState125 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | CONTINUE ->
                _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState125 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EXIT ->
                _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState125 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOR ->
                _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState125 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IF ->
                _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState125 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LABEL _v ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState125 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB ->
                _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState125 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MULT ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState125 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NOP ->
                _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState125 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | PRINT ->
                _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState125 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RETURN ->
                _menhir_run126 _menhir_env (Obj.magic _menhir_stack) MenhirState125 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TFUN ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState125 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TINT ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState125 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TSTRING ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState125 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | WHILE ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState125 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState125) : 'freshtv516)
        | SUB ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv517 * _menhir_state * Lexing.position)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4614 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv518)) : 'freshtv520)
    | MenhirState126 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv525 * _menhir_state * Lexing.position) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4623 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADD ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NEQ ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | COLON | COMMA | RP | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv521 * _menhir_state * Lexing.position) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4659 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, _startpos__1_), _, (e : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4664 "typ/typ/TYPParser.ml"
            ))) = _menhir_stack in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _v : (
# 75 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 4671 "typ/typ/TYPParser.ml"
            ) = 
# 317 "typ/typ/TYPParser.mly"
    ( Return e )
# 4675 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv522)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv523 * _menhir_state * Lexing.position) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4685 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv524)) : 'freshtv526)
    | MenhirState129 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv533 * _menhir_state * Lexing.position)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4694 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADD ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NEQ ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | RP ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv529 * _menhir_state * Lexing.position)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4728 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv527 * _menhir_state * Lexing.position)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4735 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, _startpos__1_), _, (e : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4740 "typ/typ/TYPParser.ml"
            ))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _v : (
# 75 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 4749 "typ/typ/TYPParser.ml"
            ) = 
# 305 "typ/typ/TYPParser.mly"
    ( Print e )
# 4753 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv528)) : 'freshtv530)
        | SUB ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv531 * _menhir_state * Lexing.position)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4765 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv532)) : 'freshtv534)
    | MenhirState136 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv539 * _menhir_state * Lexing.position)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4774 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADD ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NEQ ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | RP ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv535 * _menhir_state * Lexing.position)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4808 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BREAK ->
                _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState138 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | CONTINUE ->
                _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState138 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EXIT ->
                _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState138 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOR ->
                _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState138 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IF ->
                _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState138 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LABEL _v ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState138 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB ->
                _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState138 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MULT ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState138 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NOP ->
                _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState138 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | PRINT ->
                _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState138 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RETURN ->
                _menhir_run126 _menhir_env (Obj.magic _menhir_stack) MenhirState138 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TFUN ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState138 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TINT ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState138 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TSTRING ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState138 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | WHILE ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState138 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState138) : 'freshtv536)
        | SUB ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv537 * _menhir_state * Lexing.position)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4856 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv538)) : 'freshtv540)
    | MenhirState147 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv559 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 4865 "typ/typ/TYPParser.ml"
        ) * Lexing.position) * (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 4869 "typ/typ/TYPParser.ml"
        ) * Lexing.position)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4873 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADD ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NEQ ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | COLON | COMMA | RP | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv555 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 4909 "typ/typ/TYPParser.ml"
            ) * Lexing.position) * (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 4913 "typ/typ/TYPParser.ml"
            ) * Lexing.position)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4917 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (type_variable : (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 4922 "typ/typ/TYPParser.ml"
            )), _startpos_type_variable_), (name : (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 4926 "typ/typ/TYPParser.ml"
            )), _startpos_name_), _, (e : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4930 "typ/typ/TYPParser.ml"
            ))) = _menhir_stack in
            let _3 = () in
            let _startpos = _startpos_type_variable_ in
            let _v : (
# 69 "typ/typ/TYPParser.mly"
      (TYPTree.variable)
# 4937 "typ/typ/TYPParser.ml"
            ) = let _startpos = _startpos_type_variable_ in
            
# 128 "typ/typ/TYPParser.mly"
    (
      (type_variable,make_node _startpos name, e)
    )
# 4944 "typ/typ/TYPParser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv553) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 69 "typ/typ/TYPParser.mly"
      (TYPTree.variable)
# 4952 "typ/typ/TYPParser.ml"
            )) = _v in
            let (_startpos : Lexing.position) = _startpos in
            ((let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
            match _menhir_s with
            | MenhirState203 | MenhirState0 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv547 * _menhir_state * (
# 69 "typ/typ/TYPParser.mly"
      (TYPTree.variable)
# 4962 "typ/typ/TYPParser.ml"
                ) * Lexing.position) = Obj.magic _menhir_stack in
                ((assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | SEMI ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : 'freshtv543 * _menhir_state * (
# 69 "typ/typ/TYPParser.mly"
      (TYPTree.variable)
# 4972 "typ/typ/TYPParser.ml"
                    ) * Lexing.position) = Obj.magic _menhir_stack in
                    ((let _menhir_env = _menhir_discard _menhir_env in
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : 'freshtv541 * _menhir_state * (
# 69 "typ/typ/TYPParser.mly"
      (TYPTree.variable)
# 4979 "typ/typ/TYPParser.ml"
                    ) * Lexing.position) = Obj.magic _menhir_stack in
                    ((let (_menhir_stack, _menhir_s, (var : (
# 69 "typ/typ/TYPParser.mly"
      (TYPTree.variable)
# 4984 "typ/typ/TYPParser.ml"
                    )), _startpos_var_) = _menhir_stack in
                    let _2 = () in
                    let _v : 'tv_global_declaration = 
# 110 "typ/typ/TYPParser.mly"
    (
      Var v
    )
# 4992 "typ/typ/TYPParser.ml"
                     in
                    _menhir_goto_global_declaration _menhir_env _menhir_stack _menhir_s _v) : 'freshtv542)) : 'freshtv544)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : 'freshtv545 * _menhir_state * (
# 69 "typ/typ/TYPParser.mly"
      (TYPTree.variable)
# 5002 "typ/typ/TYPParser.ml"
                    ) * Lexing.position) = Obj.magic _menhir_stack in
                    ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv546)) : 'freshtv548)
            | MenhirState48 | MenhirState125 | MenhirState191 | MenhirState185 | MenhirState188 | MenhirState186 | MenhirState183 | MenhirState133 | MenhirState178 | MenhirState138 | MenhirState175 | MenhirState155 | MenhirState153 | MenhirState140 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv551 * _menhir_state * (
# 69 "typ/typ/TYPParser.mly"
      (TYPTree.variable)
# 5011 "typ/typ/TYPParser.ml"
                ) * Lexing.position) = Obj.magic _menhir_stack in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv549 * _menhir_state * (
# 69 "typ/typ/TYPParser.mly"
      (TYPTree.variable)
# 5017 "typ/typ/TYPParser.ml"
                ) * Lexing.position) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, (v : (
# 69 "typ/typ/TYPParser.mly"
      (TYPTree.variable)
# 5022 "typ/typ/TYPParser.ml"
                )), _startpos_v_) = _menhir_stack in
                let _startpos = _startpos_v_ in
                let _v : (
# 75 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 5028 "typ/typ/TYPParser.ml"
                ) = 
# 335 "typ/typ/TYPParser.mly"
    ( 
      Declaration v
    )
# 5034 "typ/typ/TYPParser.ml"
                 in
                _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv550)) : 'freshtv552)
            | _ ->
                _menhir_fail ()) : 'freshtv554)) : 'freshtv556)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv557 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 5046 "typ/typ/TYPParser.ml"
            ) * Lexing.position) * (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 5050 "typ/typ/TYPParser.ml"
            ) * Lexing.position)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 5054 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv558)) : 'freshtv560)
    | MenhirState151 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv565 * _menhir_state * Lexing.position)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_instruction__)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 5063 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADD ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NEQ ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv561 * _menhir_state * Lexing.position)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_instruction__)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 5097 "typ/typ/TYPParser.ml"
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
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MULT ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NOP ->
                _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | PRINT ->
                _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RETURN ->
                _menhir_run126 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TFUN ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TINT ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TSTRING ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RP ->
                _menhir_reduce72 _menhir_env (Obj.magic _menhir_stack) MenhirState153
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState153) : 'freshtv562)
        | SUB ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv563 * _menhir_state * Lexing.position)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_instruction__)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 5141 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv564)) : 'freshtv566)
    | MenhirState168 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv571 * _menhir_state * (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 5150 "typ/typ/TYPParser.ml"
        ) * Lexing.position) * 'tv_assign_binop) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 5154 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADD ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NEQ ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | COLON | COMMA | RP | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv567 * _menhir_state * (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 5190 "typ/typ/TYPParser.ml"
            ) * Lexing.position) * 'tv_assign_binop) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 5194 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (l : (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 5199 "typ/typ/TYPParser.ml"
            )), _startpos_l_), (op : 'tv_assign_binop)), _, (e : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 5203 "typ/typ/TYPParser.ml"
            ))) = _menhir_stack in
            let _startpos = _startpos_l_ in
            let _v : (
# 75 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 5209 "typ/typ/TYPParser.ml"
            ) = 
# 320 "typ/typ/TYPParser.mly"
    (
      BinopAssign(l, op, e)
    )
# 5215 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv568)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv569 * _menhir_state * (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 5225 "typ/typ/TYPParser.ml"
            ) * Lexing.position) * 'tv_assign_binop) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 5229 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv570)) : 'freshtv572)
    | _ ->
        _menhir_fail ()

and _menhir_reduce62 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 5239 "typ/typ/TYPParser.ml"
) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (t : (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 5245 "typ/typ/TYPParser.ml"
    )), _startpos_t_) = _menhir_stack in
    let _startpos = _startpos_t_ in
    let _v : (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 5251 "typ/typ/TYPParser.ml"
    ) = let _startpos = _startpos_t_ in
    
# 223 "typ/typ/TYPParser.mly"
    ( Id (make_node _startpos t) )
# 5256 "typ/typ/TYPParser.ml"
     in
    _menhir_goto_l_expr _menhir_env _menhir_stack _menhir_s _v _startpos

and _menhir_goto_separated_nonempty_list_COMMA_parameter_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_COMMA_parameter_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState37 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv363) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_COMMA_parameter_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv361) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((x : 'tv_separated_nonempty_list_COMMA_parameter_) : 'tv_separated_nonempty_list_COMMA_parameter_) = _v in
        ((let _v : 'tv_loption_separated_nonempty_list_COMMA_parameter__ = 
# 144 "/home/fusroman/.opam/4.06.0/lib/menhir/standard.mly"
    ( x )
# 5275 "typ/typ/TYPParser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_COMMA_parameter__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv362)) : 'freshtv364)
    | MenhirState44 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv367 * _menhir_state * (
# 70 "typ/typ/TYPParser.mly"
      (TYPTree.parameter)
# 5283 "typ/typ/TYPParser.ml"
        ))) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_COMMA_parameter_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv365 * _menhir_state * (
# 70 "typ/typ/TYPParser.mly"
      (TYPTree.parameter)
# 5291 "typ/typ/TYPParser.ml"
        ))) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((xs : 'tv_separated_nonempty_list_COMMA_parameter_) : 'tv_separated_nonempty_list_COMMA_parameter_) = _v in
        ((let (_menhir_stack, _menhir_s, (x : (
# 70 "typ/typ/TYPParser.mly"
      (TYPTree.parameter)
# 5298 "typ/typ/TYPParser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_separated_nonempty_list_COMMA_parameter_ = 
# 231 "/home/fusroman/.opam/4.06.0/lib/menhir/standard.mly"
    ( x :: xs )
# 5304 "typ/typ/TYPParser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_parameter_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv366)) : 'freshtv368)
    | _ ->
        _menhir_fail ()

and _menhir_reduce61 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (
# 72 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 5315 "typ/typ/TYPParser.ml"
    ) = 
# 218 "typ/typ/TYPParser.mly"
    ( [] )
# 5319 "typ/typ/TYPParser.ml"
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
        let (_menhir_stack : 'freshtv357 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADDRESS ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | BOOL _v ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
        | CPL ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | INT _v ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
        | LABEL _v ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LB ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LP ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | MULT ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NOT ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | SUB ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | TFUN ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TINT ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TSTRING ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState50) : 'freshtv358)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv359 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv360)

and _menhir_run126 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState126
    | BOOL _v ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _v
    | CPL ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState126
    | INT _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _v
    | LABEL _v ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState126
    | MULT ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState126
    | SUB ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState126
    | TFUN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TINT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TSTRING ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState126

and _menhir_run128 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LP ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv353 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADDRESS ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | BOOL _v ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _v
        | CPL ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | INT _v ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _v
        | LABEL _v ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LB ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LP ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | MULT ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NOT ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | SUB ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | TFUN ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TINT ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TSTRING ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState129) : 'freshtv354)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv355 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv356)

and _menhir_run132 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv351) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_startpos__1_ : Lexing.position) = _startpos in
    ((let _1 = () in
    let _startpos = _startpos__1_ in
    let _v : (
# 75 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 5472 "typ/typ/TYPParser.ml"
    ) = 
# 301 "typ/typ/TYPParser.mly"
    ( Nop )
# 5476 "typ/typ/TYPParser.ml"
     in
    _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv352)

and _menhir_run135 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LP ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv347 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADDRESS ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | BOOL _v ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _v
        | CPL ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | INT _v ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _v
        | LABEL _v ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LB ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LP ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | MULT ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NOT ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | SUB ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | TFUN ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TINT ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TSTRING ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState136) : 'freshtv348)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv349 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv350)

and _menhir_run139 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LP ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv343 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
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
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LB ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MULT ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NOP ->
            _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | PRINT ->
            _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | RETURN ->
            _menhir_run126 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TFUN ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TINT ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TSTRING ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | SEMI ->
            _menhir_reduce72 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState140) : 'freshtv344)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv345 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv346)

and _menhir_run141 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv341) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_startpos__1_ : Lexing.position) = _startpos in
    ((let _1 = () in
    let _startpos = _startpos__1_ in
    let _v : (
# 75 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 5592 "typ/typ/TYPParser.ml"
    ) = 
# 303 "typ/typ/TYPParser.mly"
    ( Exit )
# 5596 "typ/typ/TYPParser.ml"
     in
    _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv342)

and _menhir_run142 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
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
# 5612 "typ/typ/TYPParser.ml"
    ) = let _startpos = _startpos__1_ in
    
# 312 "typ/typ/TYPParser.mly"
    ( 
      Continue (make_node _startpos ())
    )
# 5619 "typ/typ/TYPParser.ml"
     in
    _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv340)

and _menhir_run143 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv337) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_startpos__1_ : Lexing.position) = _startpos in
    ((let _1 = () in
    let _startpos = _startpos__1_ in
    let _v : (
# 75 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 5635 "typ/typ/TYPParser.ml"
    ) = let _startpos = _startpos__1_ in
    
# 307 "typ/typ/TYPParser.mly"
    ( 
      Break (make_node _startpos ())
    )
# 5642 "typ/typ/TYPParser.ml"
     in
    _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv338)

and _menhir_run51 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | BOOL _v ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
    | CPL ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | INT _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
    | LABEL _v ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | MULT ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | SUB ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | TFUN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TINT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TSTRING ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
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
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | BOOL _v ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
    | CPL ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | INT _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
    | LABEL _v ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | MULT ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | SUB ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | TFUN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TINT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TSTRING ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
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
        let (_menhir_stack : 'freshtv333 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADDRESS ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | BOOL _v ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
        | CPL ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | INT _v ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
        | LABEL _v ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LB ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LP ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | MULT ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NOT ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | SUB ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | TFUN ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TINT ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TSTRING ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState54) : 'freshtv334)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv335 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv336)

and _menhir_run55 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | BOOL _v ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
    | CPL ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | INT _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
    | LABEL _v ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | MULT ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | SUB ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | TFUN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TINT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TSTRING ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState55

and _menhir_run56 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 5810 "typ/typ/TYPParser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LABEL _ | LB | LS | MULT ->
        _menhir_reduce98 _menhir_env (Obj.magic _menhir_stack)
    | ADD | ADDASSIGN | AND | ASSIGN | COLON | COMMA | DECR | DIV | DIVASSIGN | DOT | EQ | GE | GT | INCR | LE | LP | LT | MULTASSIGN | NEQ | OR | RB | REM | RP | RS | SEMI | SUB | SUBASSIGN ->
        _menhir_reduce62 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv331 * _menhir_state * (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 5828 "typ/typ/TYPParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv332)

and _menhir_run57 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 63 "typ/typ/TYPParser.mly"
       (int)
# 5836 "typ/typ/TYPParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv329) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((i : (
# 63 "typ/typ/TYPParser.mly"
       (int)
# 5846 "typ/typ/TYPParser.ml"
    )) : (
# 63 "typ/typ/TYPParser.mly"
       (int)
# 5850 "typ/typ/TYPParser.ml"
    )) = _v in
    ((let _v : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 5855 "typ/typ/TYPParser.ml"
    ) = 
# 238 "typ/typ/TYPParser.mly"
    ( Int i )
# 5859 "typ/typ/TYPParser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv330)

and _menhir_run58 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | BOOL _v ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
    | CPL ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | INT _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
    | LABEL _v ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | MULT ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | SUB ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | TFUN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TINT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TSTRING ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState58

and _menhir_run59 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 62 "typ/typ/TYPParser.mly"
       (bool)
# 5903 "typ/typ/TYPParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv327) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((b : (
# 62 "typ/typ/TYPParser.mly"
       (bool)
# 5913 "typ/typ/TYPParser.ml"
    )) : (
# 62 "typ/typ/TYPParser.mly"
       (bool)
# 5917 "typ/typ/TYPParser.ml"
    )) = _v in
    ((let _v : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 5922 "typ/typ/TYPParser.ml"
    ) = 
# 240 "typ/typ/TYPParser.mly"
    ( Bool b )
# 5926 "typ/typ/TYPParser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv328)

and _menhir_run60 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LABEL _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv325) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState60 in
        let (_v : (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 5943 "typ/typ/TYPParser.ml"
        )) = _v in
        let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        ((let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        _menhir_reduce62 _menhir_env (Obj.magic _menhir_stack)) : 'freshtv326)
    | MULT ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState60

and _menhir_goto_global_declaration : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_global_declaration -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv323 * _menhir_state * 'tv_global_declaration) = Obj.magic _menhir_stack in
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
        _menhir_reduce68 _menhir_env (Obj.magic _menhir_stack) MenhirState203
    | EOF ->
        _menhir_reduce66 _menhir_env (Obj.magic _menhir_stack) MenhirState203
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState203) : 'freshtv324)

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_run66 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 5993 "typ/typ/TYPParser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ASSIGN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv319 * _menhir_state * (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 6005 "typ/typ/TYPParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADDRESS ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | BOOL _v ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
        | CPL ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | INT _v ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
        | LABEL _v ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LB ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LP ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | MULT ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NOT ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | SUB ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | TFUN ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TINT ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TSTRING ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState67) : 'freshtv320)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv321 * _menhir_state * (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 6047 "typ/typ/TYPParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv322)

and _menhir_goto_parameter : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 70 "typ/typ/TYPParser.mly"
      (TYPTree.parameter)
# 6055 "typ/typ/TYPParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv317 * _menhir_state * (
# 70 "typ/typ/TYPParser.mly"
      (TYPTree.parameter)
# 6063 "typ/typ/TYPParser.ml"
    )) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COMMA ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv311 * _menhir_state * (
# 70 "typ/typ/TYPParser.mly"
      (TYPTree.parameter)
# 6073 "typ/typ/TYPParser.ml"
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
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState44) : 'freshtv312)
    | RP ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv313 * _menhir_state * (
# 70 "typ/typ/TYPParser.mly"
      (TYPTree.parameter)
# 6097 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (x : (
# 70 "typ/typ/TYPParser.mly"
      (TYPTree.parameter)
# 6102 "typ/typ/TYPParser.ml"
        ))) = _menhir_stack in
        let _v : 'tv_separated_nonempty_list_COMMA_parameter_ = 
# 229 "/home/fusroman/.opam/4.06.0/lib/menhir/standard.mly"
    ( [ x ] )
# 6107 "typ/typ/TYPParser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_parameter_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv314)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv315 * _menhir_state * (
# 70 "typ/typ/TYPParser.mly"
      (TYPTree.parameter)
# 6117 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv316)) : 'freshtv318)

and _menhir_goto_loption_separated_nonempty_list_COMMA_parameter__ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_loption_separated_nonempty_list_COMMA_parameter__ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ((('freshtv309 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6129 "typ/typ/TYPParser.ml"
    ) * Lexing.position) * (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 6133 "typ/typ/TYPParser.ml"
    ) * Lexing.position)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_parameter__) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RP ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv305 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6143 "typ/typ/TYPParser.ml"
        ) * Lexing.position) * (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 6147 "typ/typ/TYPParser.ml"
        ) * Lexing.position)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_parameter__) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LB ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv301 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6157 "typ/typ/TYPParser.ml"
            ) * Lexing.position) * (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 6161 "typ/typ/TYPParser.ml"
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
                _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LABEL _v ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MULT ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NOP ->
                _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | PRINT ->
                _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RETURN ->
                _menhir_run126 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TFUN ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TINT ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TSTRING ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | WHILE ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RB ->
                _menhir_reduce61 _menhir_env (Obj.magic _menhir_stack) MenhirState48
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState48) : 'freshtv302)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv303 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6211 "typ/typ/TYPParser.ml"
            ) * Lexing.position) * (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 6215 "typ/typ/TYPParser.ml"
            ) * Lexing.position)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_parameter__)) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv304)) : 'freshtv306)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv307 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6226 "typ/typ/TYPParser.ml"
        ) * Lexing.position) * (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 6230 "typ/typ/TYPParser.ml"
        ) * Lexing.position)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_parameter__) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv308)) : 'freshtv310)

and _menhir_run147 : _menhir_env -> ('ttv_tail * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6238 "typ/typ/TYPParser.ml"
) * Lexing.position) * (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 6242 "typ/typ/TYPParser.ml"
) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState147
    | BOOL _v ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _v
    | CPL ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState147
    | INT _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _v
    | LABEL _v ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState147
    | MULT ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState147
    | SUB ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState147
    | TFUN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TINT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TSTRING ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState147

and _menhir_goto_separated_nonempty_list_COMMA_type_expr_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_COMMA_type_expr_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState25 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv295 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6287 "typ/typ/TYPParser.ml"
        ) * Lexing.position)) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_COMMA_type_expr_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv293 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6295 "typ/typ/TYPParser.ml"
        ) * Lexing.position)) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((xs : 'tv_separated_nonempty_list_COMMA_type_expr_) : 'tv_separated_nonempty_list_COMMA_type_expr_) = _v in
        ((let (_menhir_stack, _menhir_s, (x : (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6302 "typ/typ/TYPParser.ml"
        )), _startpos_x_) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_separated_nonempty_list_COMMA_type_expr_ = 
# 231 "/home/fusroman/.opam/4.06.0/lib/menhir/standard.mly"
    ( x :: xs )
# 6308 "typ/typ/TYPParser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_type_expr_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv294)) : 'freshtv296)
    | MenhirState8 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv299) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_COMMA_type_expr_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv297) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((x : 'tv_separated_nonempty_list_COMMA_type_expr_) : 'tv_separated_nonempty_list_COMMA_type_expr_) = _v in
        ((let _v : 'tv_loption_separated_nonempty_list_COMMA_type_expr__ = 
# 144 "/home/fusroman/.opam/4.06.0/lib/menhir/standard.mly"
    ( x )
# 6323 "typ/typ/TYPParser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_COMMA_type_expr__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv298)) : 'freshtv300)
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_SEMI_field_declaration_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_SEMI_field_declaration_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState133 | MenhirState9 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv287 * _menhir_state * Lexing.position) * _menhir_state * 'tv_separated_nonempty_list_SEMI_field_declaration_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RB ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv275 * _menhir_state * Lexing.position) * _menhir_state * 'tv_separated_nonempty_list_SEMI_field_declaration_) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv273 * _menhir_state * Lexing.position) * _menhir_state * 'tv_separated_nonempty_list_SEMI_field_declaration_) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, _startpos__1_), _, (fields : 'tv_separated_nonempty_list_SEMI_field_declaration_)) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _v : (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6352 "typ/typ/TYPParser.ml"
            ) = 
# 179 "typ/typ/TYPParser.mly"
  (
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
  )
# 6367 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_type_expr _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv274)) : 'freshtv276)
        | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv283 * _menhir_state * Lexing.position) * _menhir_state * 'tv_separated_nonempty_list_SEMI_field_declaration_) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | RB ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv279 * _menhir_state * Lexing.position) * _menhir_state * 'tv_separated_nonempty_list_SEMI_field_declaration_)) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv277 * _menhir_state * Lexing.position) * _menhir_state * 'tv_separated_nonempty_list_SEMI_field_declaration_)) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s, _startpos__1_), _, (fields : 'tv_separated_nonempty_list_SEMI_field_declaration_)) = _menhir_stack in
                let _4 = () in
                let _3 = () in
                let _1 = () in
                let _startpos = _startpos__1_ in
                let _v : (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6390 "typ/typ/TYPParser.ml"
                ) = 
# 179 "typ/typ/TYPParser.mly"
  (
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
  )
# 6405 "typ/typ/TYPParser.ml"
                 in
                _menhir_goto_type_expr _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv278)) : 'freshtv280)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv281 * _menhir_state * Lexing.position) * _menhir_state * 'tv_separated_nonempty_list_SEMI_field_declaration_)) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv282)) : 'freshtv284)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv285 * _menhir_state * Lexing.position) * _menhir_state * 'tv_separated_nonempty_list_SEMI_field_declaration_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv286)) : 'freshtv288)
    | MenhirState22 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv291 * _menhir_state * 'tv_field_declaration)) * _menhir_state * 'tv_separated_nonempty_list_SEMI_field_declaration_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv289 * _menhir_state * 'tv_field_declaration)) * _menhir_state * 'tv_separated_nonempty_list_SEMI_field_declaration_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (x : 'tv_field_declaration)), _, (xs : 'tv_separated_nonempty_list_SEMI_field_declaration_)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_separated_nonempty_list_SEMI_field_declaration_ = 
# 231 "/home/fusroman/.opam/4.06.0/lib/menhir/standard.mly"
    ( x :: xs )
# 6432 "typ/typ/TYPParser.ml"
         in
        _menhir_goto_separated_nonempty_list_SEMI_field_declaration_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv290)) : 'freshtv292)
    | _ ->
        _menhir_fail ()

and _menhir_run14 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6441 "typ/typ/TYPParser.ml"
) * Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv271 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6449 "typ/typ/TYPParser.ml"
    ) * Lexing.position) = Obj.magic _menhir_stack in
    let (_startpos__2_ : Lexing.position) = _startpos in
    ((let (_menhir_stack, _menhir_s, (t : (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6455 "typ/typ/TYPParser.ml"
    )), _startpos_t_) = _menhir_stack in
    let _2 = () in
    let _startpos = _startpos_t_ in
    let _v : (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6462 "typ/typ/TYPParser.ml"
    ) = 
# 174 "typ/typ/TYPParser.mly"
  (
    TPointer t
  )
# 6468 "typ/typ/TYPParser.ml"
     in
    _menhir_goto_type_expr _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv272)

and _menhir_run15 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6475 "typ/typ/TYPParser.ml"
) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RS ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv267 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6486 "typ/typ/TYPParser.ml"
        ) * Lexing.position)) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv265 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6493 "typ/typ/TYPParser.ml"
        ) * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (t : (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6498 "typ/typ/TYPParser.ml"
        )), _startpos_t_) = _menhir_stack in
        let _3 = () in
        let _2 = () in
        let _startpos = _startpos_t_ in
        let _v : (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6506 "typ/typ/TYPParser.ml"
        ) = 
# 170 "typ/typ/TYPParser.mly"
  (
    TArray (t)
  )
# 6512 "typ/typ/TYPParser.ml"
         in
        _menhir_goto_type_expr _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv266)) : 'freshtv268)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv269 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6522 "typ/typ/TYPParser.ml"
        ) * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv270)

and _menhir_run11 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 6530 "typ/typ/TYPParser.ml"
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
# 6554 "typ/typ/TYPParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv263) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : (
# 68 "typ/typ/TYPParser.mly"
      (TYPTree.typ_prog TYPTree.program)
# 6563 "typ/typ/TYPParser.ml"
    )) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv261) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : (
# 68 "typ/typ/TYPParser.mly"
      (TYPTree.typ_prog TYPTree.program)
# 6571 "typ/typ/TYPParser.ml"
    )) : (
# 68 "typ/typ/TYPParser.mly"
      (TYPTree.typ_prog TYPTree.program)
# 6575 "typ/typ/TYPParser.ml"
    )) = _v in
    (Obj.magic _1 : 'freshtv262)) : 'freshtv264)

and _menhir_goto_list_global_declaration_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_list_global_declaration_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv255 * _menhir_state * 'tv_list_global_declaration_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EOF ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv251 * _menhir_state * 'tv_list_global_declaration_) = Obj.magic _menhir_stack in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv249 * _menhir_state * 'tv_list_global_declaration_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (globals : 'tv_list_global_declaration_)) = _menhir_stack in
            let _2 = () in
            let _v : (
# 68 "typ/typ/TYPParser.mly"
      (TYPTree.typ_prog TYPTree.program)
# 6599 "typ/typ/TYPParser.ml"
            ) = 
# 92 "typ/typ/TYPParser.mly"
    (
      let (genv, _type, tree) = make_env globals in
      {genv, _type, tree}
    )
# 6606 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_program _menhir_env _menhir_stack _menhir_s _v) : 'freshtv250)) : 'freshtv252)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv253 * _menhir_state * 'tv_list_global_declaration_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv254)) : 'freshtv256)
    | MenhirState203 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv259 * _menhir_state * 'tv_global_declaration) * _menhir_state * 'tv_list_global_declaration_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv257 * _menhir_state * 'tv_global_declaration) * _menhir_state * 'tv_list_global_declaration_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (x : 'tv_global_declaration)), _, (xs : 'tv_list_global_declaration_)) = _menhir_stack in
        let _v : 'tv_list_global_declaration_ = 
# 201 "/home/fusroman/.opam/4.06.0/lib/menhir/standard.mly"
    ( x :: xs )
# 6625 "typ/typ/TYPParser.ml"
         in
        _menhir_goto_list_global_declaration_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv258)) : 'freshtv260)
    | _ ->
        _menhir_fail ()

and _menhir_goto_list_type_declaration_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_list_type_declaration_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState196 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv239 * _menhir_state * 'tv_type_declaration) * _menhir_state * 'tv_list_type_declaration_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv237 * _menhir_state * 'tv_type_declaration) * _menhir_state * 'tv_list_type_declaration_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (x : 'tv_type_declaration)), _, (xs : 'tv_list_type_declaration_)) = _menhir_stack in
        let _v : 'tv_list_type_declaration_ = 
# 201 "/home/fusroman/.opam/4.06.0/lib/menhir/standard.mly"
    ( x :: xs )
# 6644 "typ/typ/TYPParser.ml"
         in
        _menhir_goto_list_type_declaration_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv238)) : 'freshtv240)
    | MenhirState203 | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv247 * _menhir_state * 'tv_list_type_declaration_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv243 * _menhir_state * 'tv_list_type_declaration_) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv241 * _menhir_state * 'tv_list_type_declaration_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (t : 'tv_list_type_declaration_)) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_global_declaration = 
# 114 "typ/typ/TYPParser.mly"
    ( 
      Type t
    )
# 6666 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_global_declaration _menhir_env _menhir_stack _menhir_s _v) : 'freshtv242)) : 'freshtv244)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv245 * _menhir_state * 'tv_list_type_declaration_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv246)) : 'freshtv248)
    | _ ->
        _menhir_fail ()

and _menhir_goto_type_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6682 "typ/typ/TYPParser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState11 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv167 * _menhir_state * (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 6692 "typ/typ/TYPParser.ml"
        ) * Lexing.position)) * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6696 "typ/typ/TYPParser.ml"
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
# 6710 "typ/typ/TYPParser.ml"
            ) * Lexing.position)) * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6714 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (f : (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 6719 "typ/typ/TYPParser.ml"
            )), _startpos_f_), _, (t : (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6723 "typ/typ/TYPParser.ml"
            )), _startpos_t_) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_field_declaration = let _startpos = _startpos_f_ in
            
# 195 "typ/typ/TYPParser.mly"
  (
    ((make_node _startpos f), t)
  )
# 6732 "typ/typ/TYPParser.ml"
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
# 6763 "typ/typ/TYPParser.ml"
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
# 6780 "typ/typ/TYPParser.ml"
            ) * Lexing.position)) * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6784 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv166)) : 'freshtv168)
    | MenhirState25 | MenhirState8 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv175 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6793 "typ/typ/TYPParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COMMA ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv169 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6803 "typ/typ/TYPParser.ml"
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
# 6831 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (x : (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6836 "typ/typ/TYPParser.ml"
            )), _startpos_x_) = _menhir_stack in
            let _v : 'tv_separated_nonempty_list_COMMA_type_expr_ = 
# 229 "/home/fusroman/.opam/4.06.0/lib/menhir/standard.mly"
    ( [ x ] )
# 6841 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_separated_nonempty_list_COMMA_type_expr_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv172)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv173 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6851 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv174)) : 'freshtv176)
    | MenhirState29 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv183 * _menhir_state * Lexing.position)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_type_expr__)) * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6860 "typ/typ/TYPParser.ml"
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
# 6874 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv177 * _menhir_state * Lexing.position)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_type_expr__)) * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6881 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, _startpos__1_), _, (xs0 : 'tv_loption_separated_nonempty_list_COMMA_type_expr__)), _, (ty : (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6886 "typ/typ/TYPParser.ml"
            )), _startpos_ty_) = _menhir_stack in
            let _6 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _v : (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6896 "typ/typ/TYPParser.ml"
            ) = let ps =
              let xs = xs0 in
              
# 220 "/home/fusroman/.opam/4.06.0/lib/menhir/standard.mly"
    ( xs )
# 6902 "typ/typ/TYPParser.ml"
              
            in
            
# 162 "typ/typ/TYPParser.mly"
  (
    TFun (ps, ty)
  )
# 6910 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_type_expr _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv178)) : 'freshtv180)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv181 * _menhir_state * Lexing.position)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_type_expr__)) * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6920 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv182)) : 'freshtv184)
    | MenhirState4 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv193 * _menhir_state * Lexing.position) * (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 6929 "typ/typ/TYPParser.ml"
        ) * Lexing.position)) * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6933 "typ/typ/TYPParser.ml"
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
# 6947 "typ/typ/TYPParser.ml"
            ) * Lexing.position)) * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6951 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, _startpos__1_), (name_type : (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 6956 "typ/typ/TYPParser.ml"
            )), _startpos_name_type_), _, (_type : (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6960 "typ/typ/TYPParser.ml"
            )), _startpos__type_) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_type_declaration = let _startpos = _startpos__1_ in
            
# 121 "typ/typ/TYPParser.mly"
  (
    (make_node _startpos name_type, _type)
  )
# 6970 "typ/typ/TYPParser.ml"
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
                _menhir_reduce68 _menhir_env (Obj.magic _menhir_stack) MenhirState196
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
# 6997 "typ/typ/TYPParser.ml"
            ) * Lexing.position)) * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 7001 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv192)) : 'freshtv194)
    | MenhirState203 | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv205 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 7010 "typ/typ/TYPParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LABEL _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv201 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 7020 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            let (_v : (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 7025 "typ/typ/TYPParser.ml"
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
# 7039 "typ/typ/TYPParser.ml"
                ) * Lexing.position) * (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 7043 "typ/typ/TYPParser.ml"
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
# 7065 "typ/typ/TYPParser.ml"
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
# 7079 "typ/typ/TYPParser.ml"
                ) * Lexing.position) * (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 7083 "typ/typ/TYPParser.ml"
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
# 7098 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv204)) : 'freshtv206)
    | MenhirState44 | MenhirState37 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv221 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 7107 "typ/typ/TYPParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADDRESS ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv213 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 7117 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LABEL _v ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv209 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 7127 "typ/typ/TYPParser.ml"
                ) * Lexing.position)) = Obj.magic _menhir_stack in
                let (_v : (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 7132 "typ/typ/TYPParser.ml"
                )) = _v in
                let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv207 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 7140 "typ/typ/TYPParser.ml"
                ) * Lexing.position)) = Obj.magic _menhir_stack in
                let ((name : (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 7145 "typ/typ/TYPParser.ml"
                )) : (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 7149 "typ/typ/TYPParser.ml"
                )) = _v in
                let (_startpos_name_ : Lexing.position) = _startpos in
                ((let (_menhir_stack, _menhir_s, (params_type : (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 7155 "typ/typ/TYPParser.ml"
                )), _startpos_params_type_) = _menhir_stack in
                let _2 = () in
                let _v : (
# 70 "typ/typ/TYPParser.mly"
      (TYPTree.parameter)
# 7161 "typ/typ/TYPParser.ml"
                ) = let _startpos = _startpos_params_type_ in
                
# 140 "typ/typ/TYPParser.mly"
    (
      {name = make_node _startpos name; reference = true; params_type}
    )
# 7168 "typ/typ/TYPParser.ml"
                 in
                _menhir_goto_parameter _menhir_env _menhir_stack _menhir_s _v) : 'freshtv208)) : 'freshtv210)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv211 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 7178 "typ/typ/TYPParser.ml"
                ) * Lexing.position)) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv212)) : 'freshtv214)
        | LABEL _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv217 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 7187 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            let (_v : (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 7192 "typ/typ/TYPParser.ml"
            )) = _v in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv215 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 7200 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            let ((name : (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 7205 "typ/typ/TYPParser.ml"
            )) : (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 7209 "typ/typ/TYPParser.ml"
            )) = _v in
            let (_startpos_name_ : Lexing.position) = _startpos in
            ((let (_menhir_stack, _menhir_s, (params_type : (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 7215 "typ/typ/TYPParser.ml"
            )), _startpos_params_type_) = _menhir_stack in
            let _v : (
# 70 "typ/typ/TYPParser.mly"
      (TYPTree.parameter)
# 7220 "typ/typ/TYPParser.ml"
            ) = let _startpos = _startpos_params_type_ in
            
# 135 "typ/typ/TYPParser.mly"
    (
      {name = make_node _startpos name; reference = false; params_type}
    )
# 7227 "typ/typ/TYPParser.ml"
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
# 7241 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv220)) : 'freshtv222)
    | MenhirState168 | MenhirState159 | MenhirState151 | MenhirState147 | MenhirState136 | MenhirState129 | MenhirState126 | MenhirState50 | MenhirState51 | MenhirState52 | MenhirState54 | MenhirState117 | MenhirState55 | MenhirState58 | MenhirState100 | MenhirState98 | MenhirState96 | MenhirState94 | MenhirState92 | MenhirState90 | MenhirState88 | MenhirState86 | MenhirState84 | MenhirState82 | MenhirState80 | MenhirState78 | MenhirState76 | MenhirState74 | MenhirState69 | MenhirState67 | MenhirState63 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv227 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 7250 "typ/typ/TYPParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LB ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv223 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 7260 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_stack = (_menhir_stack, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LABEL _v ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState65) : 'freshtv224)
        | LS ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv225 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 7284 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv226)) : 'freshtv228)
    | MenhirState48 | MenhirState125 | MenhirState191 | MenhirState185 | MenhirState188 | MenhirState186 | MenhirState183 | MenhirState133 | MenhirState178 | MenhirState138 | MenhirState175 | MenhirState155 | MenhirState153 | MenhirState140 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv235 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 7293 "typ/typ/TYPParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LABEL _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv231 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 7303 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            let (_v : (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 7308 "typ/typ/TYPParser.ml"
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
                let (_menhir_stack : ('freshtv229 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 7324 "typ/typ/TYPParser.ml"
                ) * Lexing.position) * (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 7328 "typ/typ/TYPParser.ml"
                ) * Lexing.position) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s, _, _), _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv230)) : 'freshtv232)
        | LS ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv233 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 7343 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv234)) : 'freshtv236)
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
# 7389 "typ/typ/TYPParser.ml"
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
# 7405 "typ/typ/TYPParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv146)

and _menhir_reduce98 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 7413 "typ/typ/TYPParser.ml"
) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (l : (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 7419 "typ/typ/TYPParser.ml"
    )), _startpos_l_) = _menhir_stack in
    let _startpos = _startpos_l_ in
    let _v : (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 7425 "typ/typ/TYPParser.ml"
    ) = let _startpos = _startpos_l_ in
    
# 166 "typ/typ/TYPParser.mly"
  (
    TAlias (make_node _startpos l)
  )
# 7432 "typ/typ/TYPParser.ml"
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
# 7454 "typ/typ/TYPParser.ml"
        ) * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv26)
    | MenhirState188 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv27 * _menhir_state * (
# 77 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 7463 "typ/typ/TYPParser.ml"
        ) * Lexing.position) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv28)
    | MenhirState186 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv29 * _menhir_state * (
# 77 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 7472 "typ/typ/TYPParser.ml"
        ) * Lexing.position) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv30)
    | MenhirState185 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv31 * _menhir_state * (
# 77 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 7481 "typ/typ/TYPParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv32)
    | MenhirState183 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv33 * _menhir_state * (
# 75 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 7490 "typ/typ/TYPParser.ml"
        ) * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv34)
    | MenhirState178 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv35 * _menhir_state * Lexing.position)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 7499 "typ/typ/TYPParser.ml"
        ))) * _menhir_state * (
# 76 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 7503 "typ/typ/TYPParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv36)
    | MenhirState175 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv37 * _menhir_state * (
# 75 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 7512 "typ/typ/TYPParser.ml"
        ) * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv38)
    | MenhirState168 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv39 * _menhir_state * (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 7521 "typ/typ/TYPParser.ml"
        ) * Lexing.position) * 'tv_assign_binop) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv40)
    | MenhirState159 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv41 * _menhir_state * (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 7530 "typ/typ/TYPParser.ml"
        ) * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv42)
    | MenhirState155 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((('freshtv43 * _menhir_state * Lexing.position)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_instruction__)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 7539 "typ/typ/TYPParser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_instruction__)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv44)
    | MenhirState153 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv45 * _menhir_state * Lexing.position)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_instruction__)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 7548 "typ/typ/TYPParser.ml"
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
# 7562 "typ/typ/TYPParser.ml"
        ) * Lexing.position) * (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 7566 "typ/typ/TYPParser.ml"
        ) * Lexing.position)) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _, _), _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv50)
    | MenhirState140 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv51 * _menhir_state * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv52)
    | MenhirState138 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv53 * _menhir_state * Lexing.position)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 7580 "typ/typ/TYPParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv54)
    | MenhirState136 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv55 * _menhir_state * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv56)
    | MenhirState133 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv57 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv58)
    | MenhirState129 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv59 * _menhir_state * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv60)
    | MenhirState126 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv61 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv62)
    | MenhirState125 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv63 * _menhir_state * Lexing.position)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 7609 "typ/typ/TYPParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv64)
    | MenhirState117 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv65 * _menhir_state) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 7618 "typ/typ/TYPParser.ml"
        )))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv66)
    | MenhirState110 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv67 * _menhir_state * 'tv_field_instanciation)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv68)
    | MenhirState100 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv69 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 7632 "typ/typ/TYPParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv70)
    | MenhirState98 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv71 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 7641 "typ/typ/TYPParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv72)
    | MenhirState96 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv73 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 7650 "typ/typ/TYPParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv74)
    | MenhirState94 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv75 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 7659 "typ/typ/TYPParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv76)
    | MenhirState92 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv77 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 7668 "typ/typ/TYPParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv78)
    | MenhirState90 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv79 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 7677 "typ/typ/TYPParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv80)
    | MenhirState88 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv81 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 7686 "typ/typ/TYPParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv82)
    | MenhirState86 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv83 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 7695 "typ/typ/TYPParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv84)
    | MenhirState84 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv85 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 7704 "typ/typ/TYPParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv86)
    | MenhirState82 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv87 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 7713 "typ/typ/TYPParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv88)
    | MenhirState80 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv89 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 7722 "typ/typ/TYPParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv90)
    | MenhirState78 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv91 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 7731 "typ/typ/TYPParser.ml"
        )) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv92)
    | MenhirState76 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv93 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 7740 "typ/typ/TYPParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv94)
    | MenhirState74 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv95 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 7749 "typ/typ/TYPParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv96)
    | MenhirState69 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv97 * _menhir_state * (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 7758 "typ/typ/TYPParser.ml"
        ) * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv98)
    | MenhirState67 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv99 * _menhir_state * (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 7767 "typ/typ/TYPParser.ml"
        ) * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv100)
    | MenhirState65 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv101 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 7776 "typ/typ/TYPParser.ml"
        ) * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv102)
    | MenhirState63 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv103 * _menhir_state * (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 7785 "typ/typ/TYPParser.ml"
        ) * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv104)
    | MenhirState60 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv105 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv106)
    | MenhirState58 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv107 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
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
# 7829 "typ/typ/TYPParser.ml"
        ) * Lexing.position) * (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 7833 "typ/typ/TYPParser.ml"
        ) * Lexing.position)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_parameter__)) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv120)
    | MenhirState44 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv121 * _menhir_state * (
# 70 "typ/typ/TYPParser.mly"
      (TYPTree.parameter)
# 7842 "typ/typ/TYPParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv122)
    | MenhirState37 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv123 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 7851 "typ/typ/TYPParser.ml"
        ) * Lexing.position) * (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 7855 "typ/typ/TYPParser.ml"
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
# 7869 "typ/typ/TYPParser.ml"
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
# 7883 "typ/typ/TYPParser.ml"
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
# 7902 "typ/typ/TYPParser.ml"
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
# 7921 "typ/typ/TYPParser.ml"
        ) = let _startpos = _startpos__1_ in
        
# 98 "typ/typ/TYPParser.mly"
    ( 
      raise_syntax_error _startpos "Syntax error" 
    )
# 7928 "typ/typ/TYPParser.ml"
         in
        _menhir_goto_program _menhir_env _menhir_stack _menhir_s _v) : 'freshtv140)) : 'freshtv142)) : 'freshtv144)

and _menhir_reduce66 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_list_global_declaration_ = 
# 199 "/home/fusroman/.opam/4.06.0/lib/menhir/standard.mly"
    ( [] )
# 7937 "typ/typ/TYPParser.ml"
     in
    _menhir_goto_list_global_declaration_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce68 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_list_type_declaration_ = 
# 199 "/home/fusroman/.opam/4.06.0/lib/menhir/standard.mly"
    ( [] )
# 7946 "typ/typ/TYPParser.ml"
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
# 7962 "typ/typ/TYPParser.ml"
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
# 7974 "typ/typ/TYPParser.ml"
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
# 8000 "typ/typ/TYPParser.ml"
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
# 8024 "typ/typ/TYPParser.ml"
    ) = 
# 158 "typ/typ/TYPParser.mly"
  (
    TArray (TChar)
  )
# 8030 "typ/typ/TYPParser.ml"
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
# 8046 "typ/typ/TYPParser.ml"
    ) = 
# 154 "typ/typ/TYPParser.mly"
  (
    TInt
  )
# 8052 "typ/typ/TYPParser.ml"
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
# 8085 "typ/typ/TYPParser.ml"
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
# 8116 "typ/typ/TYPParser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce98 _menhir_env (Obj.magic _menhir_stack)

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
# 8138 "typ/typ/TYPParser.ml"
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
        _menhir_reduce68 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | EOF ->
        _menhir_reduce66 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0) : 'freshtv2))

# 233 "/home/fusroman/.opam/4.06.0/lib/menhir/standard.mly"
  

# 8181 "typ/typ/TYPParser.ml"
