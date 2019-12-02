
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
  | MenhirState199
  | MenhirState192
  | MenhirState187
  | MenhirState184
  | MenhirState182
  | MenhirState181
  | MenhirState179
  | MenhirState174
  | MenhirState171
  | MenhirState164
  | MenhirState155
  | MenhirState151
  | MenhirState149
  | MenhirState147
  | MenhirState143
  | MenhirState136
  | MenhirState134
  | MenhirState132
  | MenhirState129
  | MenhirState125
  | MenhirState122
  | MenhirState121
  | MenhirState113
  | MenhirState106
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
  | MenhirState72
  | MenhirState67
  | MenhirState65
  | MenhirState63
  | MenhirState61
  | MenhirState58
  | MenhirState56
  | MenhirState53
  | MenhirState52
  | MenhirState50
  | MenhirState49
  | MenhirState48
  | MenhirState46
  | MenhirState42
  | MenhirState35
  | MenhirState27
  | MenhirState23
  | MenhirState20
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
    | MenhirState121 | MenhirState174 | MenhirState134 | MenhirState151 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv773 * _menhir_state * (
# 77 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 212 "typ/typ/TYPParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv771 * _menhir_state * (
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
# 356 "typ/typ/TYPParser.mly"
    ( [c] )
# 232 "typ/typ/TYPParser.ml"
         in
        _menhir_goto_block _menhir_env _menhir_stack _menhir_s _v) : 'freshtv772)) : 'freshtv774)
    | MenhirState46 | MenhirState129 | MenhirState187 | MenhirState181 | MenhirState184 | MenhirState182 | MenhirState179 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv779 * _menhir_state * (
# 77 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 240 "typ/typ/TYPParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BREAK ->
            _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState181 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | COLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv775 * _menhir_state * (
# 77 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 252 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState181 in
            ((let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BREAK ->
                _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState184 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | CONTINUE ->
                _menhir_run138 _menhir_env (Obj.magic _menhir_stack) MenhirState184 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EXIT ->
                _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState184 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOR ->
                _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState184 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IF ->
                _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState184 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LABEL _v ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState184 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState184 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MULT ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState184 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NOP ->
                _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState184 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | PRINT ->
                _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState184 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RETURN ->
                _menhir_run122 _menhir_env (Obj.magic _menhir_stack) MenhirState184 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TFUN ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState184 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TINT ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState184 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TSTRING ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState184 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | WHILE ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState184 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RB ->
                _menhir_reduce60 _menhir_env (Obj.magic _menhir_stack) MenhirState184
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState184) : 'freshtv776)
        | CONTINUE ->
            _menhir_run138 _menhir_env (Obj.magic _menhir_stack) MenhirState181 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | EXIT ->
            _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState181 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | FOR ->
            _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState181 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | IF ->
            _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState181 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LABEL _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState181 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LB ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState181 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MULT ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState181 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NOP ->
            _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState181 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | PRINT ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState181 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | RETURN ->
            _menhir_run122 _menhir_env (Obj.magic _menhir_stack) MenhirState181 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv777 * _menhir_state * (
# 77 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 320 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState181 in
            ((let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BREAK ->
                _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState182 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | CONTINUE ->
                _menhir_run138 _menhir_env (Obj.magic _menhir_stack) MenhirState182 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EXIT ->
                _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState182 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOR ->
                _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState182 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IF ->
                _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState182 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LABEL _v ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState182 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState182 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MULT ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState182 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NOP ->
                _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState182 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | PRINT ->
                _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState182 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RETURN ->
                _menhir_run122 _menhir_env (Obj.magic _menhir_stack) MenhirState182 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TFUN ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState182 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TINT ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState182 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TSTRING ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState182 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | WHILE ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState182 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RB ->
                _menhir_reduce60 _menhir_env (Obj.magic _menhir_stack) MenhirState182
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState182) : 'freshtv778)
        | TFUN ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState181 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TINT ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState181 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TSTRING ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState181 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | WHILE ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState181 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | RB ->
            _menhir_reduce60 _menhir_env (Obj.magic _menhir_stack) MenhirState181
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState181) : 'freshtv780)
    | _ ->
        _menhir_fail ()

and _menhir_goto_loption_separated_nonempty_list_COMMA_expr__ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_loption_separated_nonempty_list_COMMA_expr__ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState67 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv761 * _menhir_state * (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 389 "typ/typ/TYPParser.ml"
        ) * Lexing.position)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RP ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv757 * _menhir_state * (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 399 "typ/typ/TYPParser.ml"
            ) * Lexing.position)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv755 * _menhir_state * (
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
            
# 279 "typ/typ/TYPParser.mly"
    ( Call(f, args) )
# 430 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv756)) : 'freshtv758)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv759 * _menhir_state * (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 440 "typ/typ/TYPParser.ml"
            ) * Lexing.position)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv760)) : 'freshtv762)
    | MenhirState155 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv769 * _menhir_state * (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 449 "typ/typ/TYPParser.ml"
        ) * Lexing.position)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RP ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv765 * _menhir_state * (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 459 "typ/typ/TYPParser.ml"
            ) * Lexing.position)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv763 * _menhir_state * (
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
            
# 328 "typ/typ/TYPParser.mly"
    (
      Call(f, args)
    )
# 493 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv764)) : 'freshtv766)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv767 * _menhir_state * (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 503 "typ/typ/TYPParser.ml"
            ) * Lexing.position)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv768)) : 'freshtv770)
    | _ ->
        _menhir_fail ()

and _menhir_goto_assign_unop : _menhir_env -> 'ttv_tail -> 'tv_assign_unop -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv753 * _menhir_state * (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 516 "typ/typ/TYPParser.ml"
    ) * Lexing.position) = Obj.magic _menhir_stack in
    let (_v : 'tv_assign_unop) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv751 * _menhir_state * (
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
# 323 "typ/typ/TYPParser.mly"
    (
      UnopAssign(l, op)
    )
# 541 "typ/typ/TYPParser.ml"
     in
    _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv752)) : 'freshtv754)

and _menhir_goto_assign_binop : _menhir_env -> 'ttv_tail -> 'tv_assign_binop -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv749 * _menhir_state * (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 552 "typ/typ/TYPParser.ml"
    ) * Lexing.position) * 'tv_assign_binop) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState164
    | BOOL _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState164 _v
    | CPL ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState164
    | INT _v ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState164 _v
    | LABEL _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState164 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState164 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState164
    | MULT ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState164 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState164
    | SUB ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState164
    | TFUN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState164 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TINT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState164 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TSTRING ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState164 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState164) : 'freshtv750)

and _menhir_reduce69 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_loption_separated_nonempty_list_COMMA_expr__ = 
# 142 "/home/fusroman/.opam/4.06.0/lib/menhir/standard.mly"
    ( [] )
# 593 "typ/typ/TYPParser.ml"
     in
    _menhir_goto_loption_separated_nonempty_list_COMMA_expr__ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run61 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 600 "typ/typ/TYPParser.ml"
) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | BOOL _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
    | CPL ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | INT _v ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
    | LABEL _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | MULT ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | SUB ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | TFUN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TINT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TSTRING ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState61

and _menhir_run100 : _menhir_env -> 'ttv_tail * _menhir_state * (
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
        let (_menhir_stack : ('freshtv745 * _menhir_state * (
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
        let (_menhir_stack : ('freshtv743 * _menhir_state * (
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
# 226 "typ/typ/TYPParser.mly"
    (
      RecordAccess(l_expr, l)
    )
# 692 "typ/typ/TYPParser.ml"
         in
        _menhir_goto_l_expr _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv744)) : 'freshtv746)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv747 * _menhir_state * (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 702 "typ/typ/TYPParser.ml"
        ) * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv748)

and _menhir_goto_loption_separated_nonempty_list_COMMA_instruction__ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_loption_separated_nonempty_list_COMMA_instruction__ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState136 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv735 * _menhir_state * Lexing.position)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_instruction__) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv731 * _menhir_state * Lexing.position)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_instruction__) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ADDRESS ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState147
            | BOOL _v ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _v
            | CPL ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState147
            | INT _v ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _v
            | LABEL _v ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LP ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState147
            | MULT ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NOT ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState147
            | SUB ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState147
            | TFUN ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TINT ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TSTRING ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState147) : 'freshtv732)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv733 * _menhir_state * Lexing.position)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_instruction__) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv734)) : 'freshtv736)
    | MenhirState149 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv741 * _menhir_state * Lexing.position)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_instruction__)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 765 "typ/typ/TYPParser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_instruction__) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RP ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv737 * _menhir_state * Lexing.position)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_instruction__)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 775 "typ/typ/TYPParser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_instruction__) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BREAK ->
                _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | CONTINUE ->
                _menhir_run138 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EXIT ->
                _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOR ->
                _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IF ->
                _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LABEL _v ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB ->
                _menhir_run129 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MULT ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NOP ->
                _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | PRINT ->
                _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RETURN ->
                _menhir_run122 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TFUN ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TINT ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TSTRING ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | WHILE ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState151) : 'freshtv738)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv739 * _menhir_state * Lexing.position)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_instruction__)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 821 "typ/typ/TYPParser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_instruction__) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv740)) : 'freshtv742)
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_COMMA_instruction_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_COMMA_instruction_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState149 | MenhirState136 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv725) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_COMMA_instruction_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv723) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((x : 'tv_separated_nonempty_list_COMMA_instruction_) : 'tv_separated_nonempty_list_COMMA_instruction_) = _v in
        ((let _v : 'tv_loption_separated_nonempty_list_COMMA_instruction__ = 
# 144 "/home/fusroman/.opam/4.06.0/lib/menhir/standard.mly"
    ( x )
# 843 "typ/typ/TYPParser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_COMMA_instruction__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv724)) : 'freshtv726)
    | MenhirState171 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv729 * _menhir_state * (
# 75 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 851 "typ/typ/TYPParser.ml"
        ) * Lexing.position)) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_COMMA_instruction_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv727 * _menhir_state * (
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
        _menhir_goto_separated_nonempty_list_COMMA_instruction_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv728)) : 'freshtv730)
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
    | MenhirState151 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((((('freshtv705 * _menhir_state * Lexing.position)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_instruction__)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 891 "typ/typ/TYPParser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_instruction__)) * _menhir_state * (
# 76 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 895 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((((('freshtv703 * _menhir_state * Lexing.position)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_instruction__)) * _menhir_state * (
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
        
# 379 "typ/typ/TYPParser.mly"
    (
      For(init, cond, it, b)
    )
# 947 "typ/typ/TYPParser.ml"
         in
        _menhir_goto_control _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv704)) : 'freshtv706)
    | MenhirState134 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv713 * _menhir_state * Lexing.position)) * _menhir_state * (
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
            let (_menhir_stack : (((('freshtv707 * _menhir_state * Lexing.position)) * _menhir_state * (
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
                _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState174 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | CONTINUE ->
                _menhir_run138 _menhir_env (Obj.magic _menhir_stack) MenhirState174 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EXIT ->
                _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState174 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOR ->
                _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState174 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IF ->
                _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState174 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LABEL _v ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState174 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB ->
                _menhir_run129 _menhir_env (Obj.magic _menhir_stack) MenhirState174 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MULT ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState174 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NOP ->
                _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState174 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | PRINT ->
                _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState174 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RETURN ->
                _menhir_run122 _menhir_env (Obj.magic _menhir_stack) MenhirState174 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TFUN ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState174 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TINT ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState174 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TSTRING ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState174 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | WHILE ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState174 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState174) : 'freshtv708)
        | BREAK | COLON | CONTINUE | EXIT | FOR | IF | LABEL _ | LB | MULT | NOP | PRINT | RB | RETURN | SEMI | TFUN | TINT | TSTRING | WHILE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv709 * _menhir_state * Lexing.position)) * _menhir_state * (
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
# 364 "typ/typ/TYPParser.mly"
    (
      VARTree.If(e, b)
    )
# 1045 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_control _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv710)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv711 * _menhir_state * Lexing.position)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 1055 "typ/typ/TYPParser.ml"
            ))) * _menhir_state * (
# 76 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 1059 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv712)) : 'freshtv714)
    | MenhirState174 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv717 * _menhir_state * Lexing.position)) * _menhir_state * (
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
        let (_menhir_stack : (((((('freshtv715 * _menhir_state * Lexing.position)) * _menhir_state * (
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
# 369 "typ/typ/TYPParser.mly"
    ( 
      IfElse(c, t, e)
    )
# 1119 "typ/typ/TYPParser.ml"
         in
        _menhir_goto_control _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv716)) : 'freshtv718)
    | MenhirState121 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv721 * _menhir_state * Lexing.position)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 1127 "typ/typ/TYPParser.ml"
        ))) * _menhir_state * (
# 76 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 1131 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv719 * _menhir_state * Lexing.position)) * _menhir_state * (
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
# 374 "typ/typ/TYPParser.mly"
    (
      While(e, b)
    )
# 1165 "typ/typ/TYPParser.ml"
         in
        _menhir_goto_control _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv720)) : 'freshtv722)
    | _ ->
        _menhir_fail ()

and _menhir_run129 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BREAK ->
        _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CONTINUE ->
        _menhir_run138 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | EXIT ->
        _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FOR ->
        _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IF ->
        _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LABEL _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv701) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState129 in
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
            _menhir_reduce97 _menhir_env (Obj.magic _menhir_stack)
        | ADDASSIGN | ASSIGN | DECR | DIVASSIGN | DOT | INCR | LP | MULTASSIGN | SUBASSIGN ->
            _menhir_reduce61 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv699 * _menhir_state * (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 1214 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv700)) : 'freshtv702)
    | LB ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MULT ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOP ->
        _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | PRINT ->
        _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RETURN ->
        _menhir_run122 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TFUN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TINT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TSTRING ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | WHILE ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RB ->
        _menhir_reduce60 _menhir_env (Obj.magic _menhir_stack) MenhirState129
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState129

and _menhir_goto_separated_nonempty_list_SEMI_field_instanciation_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_SEMI_field_instanciation_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState63 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv693 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 1252 "typ/typ/TYPParser.ml"
        ) * Lexing.position) * Lexing.position) * _menhir_state * 'tv_separated_nonempty_list_SEMI_field_instanciation_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RB ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv689 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 1262 "typ/typ/TYPParser.ml"
            ) * Lexing.position) * Lexing.position) * _menhir_state * 'tv_separated_nonempty_list_SEMI_field_instanciation_) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv687 * _menhir_state * (
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
# 281 "typ/typ/TYPParser.mly"
    (
      NewRecord (t_e,fields)
    )
# 1287 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv688)) : 'freshtv690)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv691 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 1297 "typ/typ/TYPParser.ml"
            ) * Lexing.position) * Lexing.position) * _menhir_state * 'tv_separated_nonempty_list_SEMI_field_instanciation_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv692)) : 'freshtv694)
    | MenhirState106 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv697 * _menhir_state * 'tv_field_instanciation)) * _menhir_state * 'tv_separated_nonempty_list_SEMI_field_instanciation_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv695 * _menhir_state * 'tv_field_instanciation)) * _menhir_state * 'tv_separated_nonempty_list_SEMI_field_instanciation_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (x : 'tv_field_instanciation)), _, (xs : 'tv_separated_nonempty_list_SEMI_field_instanciation_)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_separated_nonempty_list_SEMI_field_instanciation_ = 
# 231 "/home/fusroman/.opam/4.06.0/lib/menhir/standard.mly"
    ( x :: xs )
# 1311 "typ/typ/TYPParser.ml"
         in
        _menhir_goto_separated_nonempty_list_SEMI_field_instanciation_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv696)) : 'freshtv698)
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_COMMA_expr_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_COMMA_expr_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState155 | MenhirState67 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv681) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_COMMA_expr_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv679) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((x : 'tv_separated_nonempty_list_COMMA_expr_) : 'tv_separated_nonempty_list_COMMA_expr_) = _v in
        ((let _v : 'tv_loption_separated_nonempty_list_COMMA_expr__ = 
# 144 "/home/fusroman/.opam/4.06.0/lib/menhir/standard.mly"
    ( x )
# 1332 "typ/typ/TYPParser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_COMMA_expr__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv680)) : 'freshtv682)
    | MenhirState96 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv685 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 1340 "typ/typ/TYPParser.ml"
        ))) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_COMMA_expr_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv683 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 1348 "typ/typ/TYPParser.ml"
        ))) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((xs : 'tv_separated_nonempty_list_COMMA_expr_) : 'tv_separated_nonempty_list_COMMA_expr_) = _v in
        ((let (_menhir_stack, _menhir_s, (x : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 1355 "typ/typ/TYPParser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_separated_nonempty_list_COMMA_expr_ = 
# 231 "/home/fusroman/.opam/4.06.0/lib/menhir/standard.mly"
    ( x :: xs )
# 1361 "typ/typ/TYPParser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_expr_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv684)) : 'freshtv686)
    | _ ->
        _menhir_fail ()

and _menhir_run72 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 1370 "typ/typ/TYPParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | BOOL _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
    | CPL ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | INT _v ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
    | LABEL _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | MULT ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | SUB ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | TFUN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TINT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TSTRING ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState72

and _menhir_run74 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 1410 "typ/typ/TYPParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | BOOL _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
    | CPL ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | INT _v ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
    | LABEL _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | MULT ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | SUB ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState74
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

and _menhir_run80 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 1450 "typ/typ/TYPParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | BOOL _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
    | CPL ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | INT _v ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
    | LABEL _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | MULT ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | SUB ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState80
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

and _menhir_run82 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 1490 "typ/typ/TYPParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | BOOL _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
    | CPL ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | INT _v ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
    | LABEL _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | MULT ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | SUB ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState82
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

and _menhir_run76 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 1530 "typ/typ/TYPParser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _startpos ->
    let _menhir_stack = (_menhir_stack, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | BOOL _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
    | CPL ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | INT _v ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
    | LABEL _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | MULT ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | SUB ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState76
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

and _menhir_run86 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 1571 "typ/typ/TYPParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | BOOL _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
    | CPL ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | INT _v ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
    | LABEL _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | MULT ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | SUB ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState86
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

and _menhir_run88 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 1611 "typ/typ/TYPParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | BOOL _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | CPL ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | INT _v ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | LABEL _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | MULT ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | SUB ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState88
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
# 1651 "typ/typ/TYPParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | BOOL _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | CPL ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | INT _v ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | LABEL _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | MULT ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | SUB ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState90
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
# 1691 "typ/typ/TYPParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | BOOL _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
    | CPL ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | INT _v ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
    | LABEL _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | MULT ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | SUB ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState92
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
# 1731 "typ/typ/TYPParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | BOOL _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
    | CPL ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | INT _v ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
    | LABEL _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | MULT ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | SUB ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState94
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

and _menhir_run78 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 1771 "typ/typ/TYPParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | BOOL _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
    | CPL ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | INT _v ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
    | LABEL _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | MULT ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | SUB ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState78
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

and _menhir_run98 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 1811 "typ/typ/TYPParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState98
    | BOOL _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _v
    | CPL ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState98
    | INT _v ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _v
    | LABEL _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState98
    | MULT ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState98
    | SUB ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState98
    | TFUN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TINT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TSTRING ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState98

and _menhir_run84 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 1851 "typ/typ/TYPParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | BOOL _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
    | CPL ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | INT _v ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
    | LABEL _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | MULT ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | SUB ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState84
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

and _menhir_goto_l_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 1891 "typ/typ/TYPParser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState58 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv635 * _menhir_state) * _menhir_state * (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 1901 "typ/typ/TYPParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack)
        | LS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | ADD | AND | COLON | COMMA | DIV | EQ | GE | GT | LE | LT | MULT | NEQ | OR | RB | REM | RP | RS | SEMI | SUB ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv631 * _menhir_state) * _menhir_state * (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 1915 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (e : (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 1920 "typ/typ/TYPParser.ml"
            )), _startpos_e_) = _menhir_stack in
            let _1 = () in
            let _v : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 1926 "typ/typ/TYPParser.ml"
            ) = 
# 277 "typ/typ/TYPParser.mly"
    ( e )
# 1930 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv632)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv633 * _menhir_state) * _menhir_state * (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 1940 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv634)) : 'freshtv636)
    | MenhirState164 | MenhirState155 | MenhirState147 | MenhirState143 | MenhirState132 | MenhirState125 | MenhirState122 | MenhirState48 | MenhirState49 | MenhirState50 | MenhirState52 | MenhirState113 | MenhirState53 | MenhirState56 | MenhirState61 | MenhirState98 | MenhirState96 | MenhirState94 | MenhirState92 | MenhirState90 | MenhirState88 | MenhirState86 | MenhirState84 | MenhirState82 | MenhirState80 | MenhirState78 | MenhirState76 | MenhirState74 | MenhirState72 | MenhirState67 | MenhirState65 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv643 * _menhir_state * (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 1949 "typ/typ/TYPParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack)
        | LP ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv637 * _menhir_state * (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 1961 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ADDRESS ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | BOOL _v ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
            | CPL ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | INT _v ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
            | LABEL _v ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LP ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | MULT ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NOT ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | SUB ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | TFUN ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TINT ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TSTRING ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RP ->
                _menhir_reduce69 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState67) : 'freshtv638)
        | LS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | ADD | AND | COLON | COMMA | DIV | EQ | GE | GT | LE | LT | MULT | NEQ | OR | RB | REM | RP | RS | SEMI | SUB ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv639 * _menhir_state * (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 2005 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (l : (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 2010 "typ/typ/TYPParser.ml"
            )), _startpos_l_) = _menhir_stack in
            let _v : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 2015 "typ/typ/TYPParser.ml"
            ) = 
# 241 "typ/typ/TYPParser.mly"
    ( Deref l )
# 2019 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv640)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv641 * _menhir_state * (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 2029 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv642)) : 'freshtv644)
    | MenhirState46 | MenhirState121 | MenhirState187 | MenhirState181 | MenhirState184 | MenhirState182 | MenhirState179 | MenhirState129 | MenhirState174 | MenhirState134 | MenhirState136 | MenhirState171 | MenhirState149 | MenhirState151 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv677 * _menhir_state * (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 2038 "typ/typ/TYPParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADDASSIGN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv647) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv645) = Obj.magic _menhir_stack in
            ((let _1 = () in
            let _v : 'tv_assign_binop = 
# 340 "typ/typ/TYPParser.mly"
              ( AddAssign )
# 2053 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_assign_binop _menhir_env _menhir_stack _v) : 'freshtv646)) : 'freshtv648)
        | ASSIGN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv651) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv649) = Obj.magic _menhir_stack in
            ((let _1 = () in
            let _v : 'tv_assign_binop = 
# 339 "typ/typ/TYPParser.mly"
              ( Standard )
# 2066 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_assign_binop _menhir_env _menhir_stack _v) : 'freshtv650)) : 'freshtv652)
        | DECR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv655) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv653) = Obj.magic _menhir_stack in
            ((let _1 = () in
            let _v : 'tv_assign_unop = 
# 348 "typ/typ/TYPParser.mly"
              ( Decr )
# 2079 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_assign_unop _menhir_env _menhir_stack _v) : 'freshtv654)) : 'freshtv656)
        | DIVASSIGN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv659) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv657) = Obj.magic _menhir_stack in
            ((let _1 = () in
            let _v : 'tv_assign_binop = 
# 343 "typ/typ/TYPParser.mly"
              ( DivAssign )
# 2092 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_assign_binop _menhir_env _menhir_stack _v) : 'freshtv658)) : 'freshtv660)
        | DOT ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv663) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv661) = Obj.magic _menhir_stack in
            ((let _1 = () in
            let _v : 'tv_assign_unop = 
# 347 "typ/typ/TYPParser.mly"
              ( Incr )
# 2107 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_assign_unop _menhir_env _menhir_stack _v) : 'freshtv662)) : 'freshtv664)
        | LP ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv665 * _menhir_state * (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 2115 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ADDRESS ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState155
            | BOOL _v ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _v
            | CPL ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState155
            | INT _v ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _v
            | LABEL _v ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LP ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState155
            | MULT ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NOT ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState155
            | SUB ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState155
            | TFUN ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TINT ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TSTRING ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RP ->
                _menhir_reduce69 _menhir_env (Obj.magic _menhir_stack) MenhirState155
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState155) : 'freshtv666)
        | LS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | MULTASSIGN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv669) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv667) = Obj.magic _menhir_stack in
            ((let _1 = () in
            let _v : 'tv_assign_binop = 
# 342 "typ/typ/TYPParser.mly"
              ( MultAssign )
# 2164 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_assign_binop _menhir_env _menhir_stack _v) : 'freshtv668)) : 'freshtv670)
        | SUBASSIGN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv673) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv671) = Obj.magic _menhir_stack in
            ((let _1 = () in
            let _v : 'tv_assign_binop = 
# 341 "typ/typ/TYPParser.mly"
              ( SubAssign )
# 2177 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_assign_binop _menhir_env _menhir_stack _v) : 'freshtv672)) : 'freshtv674)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv675 * _menhir_state * (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 2187 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv676)) : 'freshtv678)
    | _ ->
        _menhir_fail ()

and _menhir_goto_instructions : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 72 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 2197 "typ/typ/TYPParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState129 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv595 * _menhir_state * Lexing.position) * _menhir_state * (
# 72 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 2207 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RB ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv591 * _menhir_state * Lexing.position) * _menhir_state * (
# 72 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 2217 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv589 * _menhir_state * Lexing.position) * _menhir_state * (
# 72 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 2224 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, _startpos__1_), _, (i : (
# 72 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 2229 "typ/typ/TYPParser.ml"
            ))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (
# 76 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 2236 "typ/typ/TYPParser.ml"
            ) = 
# 359 "typ/typ/TYPParser.mly"
    ( i )
# 2240 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_block _menhir_env _menhir_stack _menhir_s _v) : 'freshtv590)) : 'freshtv592)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv593 * _menhir_state * Lexing.position) * _menhir_state * (
# 72 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 2250 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv594)) : 'freshtv596)
    | MenhirState179 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv599 * _menhir_state * (
# 75 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 2259 "typ/typ/TYPParser.ml"
        ) * Lexing.position)) * _menhir_state * (
# 72 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 2263 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv597 * _menhir_state * (
# 75 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 2269 "typ/typ/TYPParser.ml"
        ) * Lexing.position)) * _menhir_state * (
# 72 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 2273 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (i : (
# 75 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 2278 "typ/typ/TYPParser.ml"
        )), _startpos_i_), _, (s : (
# 72 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 2282 "typ/typ/TYPParser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : (
# 72 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 2288 "typ/typ/TYPParser.ml"
        ) = 
# 201 "typ/typ/TYPParser.mly"
    (
      i::s
    )
# 2294 "typ/typ/TYPParser.ml"
         in
        _menhir_goto_instructions _menhir_env _menhir_stack _menhir_s _v) : 'freshtv598)) : 'freshtv600)
    | MenhirState182 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv603 * _menhir_state * (
# 77 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 2302 "typ/typ/TYPParser.ml"
        ) * Lexing.position) * _menhir_state) * _menhir_state * (
# 72 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 2306 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv601 * _menhir_state * (
# 77 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 2312 "typ/typ/TYPParser.ml"
        ) * Lexing.position) * _menhir_state) * _menhir_state * (
# 72 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 2316 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s, (c : (
# 77 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 2321 "typ/typ/TYPParser.ml"
        )), _startpos_c_), _), _, (s : (
# 72 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 2325 "typ/typ/TYPParser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : (
# 72 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 2331 "typ/typ/TYPParser.ml"
        ) = 
# 207 "typ/typ/TYPParser.mly"
    (
      c::s
    )
# 2337 "typ/typ/TYPParser.ml"
         in
        _menhir_goto_instructions _menhir_env _menhir_stack _menhir_s _v) : 'freshtv602)) : 'freshtv604)
    | MenhirState184 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv607 * _menhir_state * (
# 77 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 2345 "typ/typ/TYPParser.ml"
        ) * Lexing.position) * _menhir_state) * _menhir_state * (
# 72 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 2349 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv605 * _menhir_state * (
# 77 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 2355 "typ/typ/TYPParser.ml"
        ) * Lexing.position) * _menhir_state) * _menhir_state * (
# 72 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 2359 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s, (_1 : (
# 77 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 2364 "typ/typ/TYPParser.ml"
        )), _startpos__1_), _), _, (_3 : (
# 72 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 2368 "typ/typ/TYPParser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : (
# 72 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 2374 "typ/typ/TYPParser.ml"
        ) = let _startpos = _startpos__1_ in
        
# 213 "typ/typ/TYPParser.mly"
    (
      raise_syntax_error _startpos "Expected ';', found ':'."
    )
# 2381 "typ/typ/TYPParser.ml"
         in
        _menhir_goto_instructions _menhir_env _menhir_stack _menhir_s _v) : 'freshtv606)) : 'freshtv608)
    | MenhirState181 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv611 * _menhir_state * (
# 77 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 2389 "typ/typ/TYPParser.ml"
        ) * Lexing.position) * _menhir_state * (
# 72 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 2393 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv609 * _menhir_state * (
# 77 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 2399 "typ/typ/TYPParser.ml"
        ) * Lexing.position) * _menhir_state * (
# 72 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 2403 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (c : (
# 77 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 2408 "typ/typ/TYPParser.ml"
        )), _startpos_c_), _, (s : (
# 72 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 2412 "typ/typ/TYPParser.ml"
        ))) = _menhir_stack in
        let _v : (
# 72 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 2417 "typ/typ/TYPParser.ml"
        ) = 
# 207 "typ/typ/TYPParser.mly"
    (
      c::s
    )
# 2423 "typ/typ/TYPParser.ml"
         in
        _menhir_goto_instructions _menhir_env _menhir_stack _menhir_s _v) : 'freshtv610)) : 'freshtv612)
    | MenhirState187 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv615 * _menhir_state * (
# 75 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 2431 "typ/typ/TYPParser.ml"
        ) * Lexing.position)) * _menhir_state * (
# 72 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 2435 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv613 * _menhir_state * (
# 75 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 2441 "typ/typ/TYPParser.ml"
        ) * Lexing.position)) * _menhir_state * (
# 72 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 2445 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (_1 : (
# 75 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 2450 "typ/typ/TYPParser.ml"
        )), _startpos__1_), _, (_3 : (
# 72 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 2454 "typ/typ/TYPParser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : (
# 72 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 2460 "typ/typ/TYPParser.ml"
        ) = let _startpos = _startpos__1_ in
        
# 213 "typ/typ/TYPParser.mly"
    (
      raise_syntax_error _startpos "Expected ';', found ':'."
    )
# 2467 "typ/typ/TYPParser.ml"
         in
        _menhir_goto_instructions _menhir_env _menhir_stack _menhir_s _v) : 'freshtv614)) : 'freshtv616)
    | MenhirState46 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv629 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 2475 "typ/typ/TYPParser.ml"
        ) * Lexing.position) * (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 2479 "typ/typ/TYPParser.ml"
        ) * Lexing.position)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_parameter__)) * Lexing.position) * _menhir_state * (
# 72 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 2483 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RB ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv625 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 2493 "typ/typ/TYPParser.ml"
            ) * Lexing.position) * (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 2497 "typ/typ/TYPParser.ml"
            ) * Lexing.position)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_parameter__)) * Lexing.position) * _menhir_state * (
# 72 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 2501 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv623 * _menhir_state * (
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
            ((let (((((_menhir_stack, _menhir_s, (return_type : (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 2521 "typ/typ/TYPParser.ml"
            )), _startpos_return_type_), (name : (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 2525 "typ/typ/TYPParser.ml"
            )), _startpos_name_), _, (xs0 : 'tv_loption_separated_nonempty_list_COMMA_parameter__)), _startpos__6_), _, (b : (
# 72 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 2529 "typ/typ/TYPParser.ml"
            ))) = _menhir_stack in
            let _8 = () in
            let _6 = () in
            let _5 = () in
            let _3 = () in
            let _v : (
# 71 "typ/typ/TYPParser.mly"
      (TYPTree.typ_function)
# 2538 "typ/typ/TYPParser.ml"
            ) = let params =
              let xs = xs0 in
              
# 220 "/home/fusroman/.opam/4.06.0/lib/menhir/standard.mly"
    ( xs )
# 2544 "typ/typ/TYPParser.ml"
              
            in
            let _startpos = _startpos_return_type_ in
            
# 147 "typ/typ/TYPParser.mly"
    (
      {name = make_node _startpos name; params; block = b; return_type}
    )
# 2553 "typ/typ/TYPParser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv621) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 71 "typ/typ/TYPParser.mly"
      (TYPTree.typ_function)
# 2561 "typ/typ/TYPParser.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv619) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 71 "typ/typ/TYPParser.mly"
      (TYPTree.typ_function)
# 2569 "typ/typ/TYPParser.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv617) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let ((def_fun : (
# 71 "typ/typ/TYPParser.mly"
      (TYPTree.typ_function)
# 2577 "typ/typ/TYPParser.ml"
            )) : (
# 71 "typ/typ/TYPParser.mly"
      (TYPTree.typ_function)
# 2581 "typ/typ/TYPParser.ml"
            )) = _v in
            ((let _v : 'tv_global_declaration = 
# 105 "typ/typ/TYPParser.mly"
    (
      Fun f
    )
# 2588 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_global_declaration _menhir_env _menhir_stack _menhir_s _v) : 'freshtv618)) : 'freshtv620)) : 'freshtv622)) : 'freshtv624)) : 'freshtv626)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv627 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 2598 "typ/typ/TYPParser.ml"
            ) * Lexing.position) * (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 2602 "typ/typ/TYPParser.ml"
            ) * Lexing.position)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_parameter__)) * Lexing.position) * _menhir_state * (
# 72 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 2606 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv628)) : 'freshtv630)
    | _ ->
        _menhir_fail ()

and _menhir_reduce71 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_loption_separated_nonempty_list_COMMA_instruction__ = 
# 142 "/home/fusroman/.opam/4.06.0/lib/menhir/standard.mly"
    ( [] )
# 2618 "typ/typ/TYPParser.ml"
     in
    _menhir_goto_loption_separated_nonempty_list_COMMA_instruction__ _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_instruction : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 75 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 2625 "typ/typ/TYPParser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState121 | MenhirState174 | MenhirState134 | MenhirState151 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv571 * _menhir_state * (
# 75 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 2635 "typ/typ/TYPParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv567 * _menhir_state * (
# 75 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 2645 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv565 * _menhir_state * (
# 75 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 2652 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (i : (
# 75 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 2657 "typ/typ/TYPParser.ml"
            )), _startpos_i_) = _menhir_stack in
            let _2 = () in
            let _v : (
# 76 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 2663 "typ/typ/TYPParser.ml"
            ) = 
# 353 "typ/typ/TYPParser.mly"
    ( [i] )
# 2667 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_block _menhir_env _menhir_stack _menhir_s _v) : 'freshtv566)) : 'freshtv568)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv569 * _menhir_state * (
# 75 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 2677 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv570)) : 'freshtv572)
    | MenhirState136 | MenhirState171 | MenhirState149 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv579 * _menhir_state * (
# 75 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 2686 "typ/typ/TYPParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COMMA ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv573 * _menhir_state * (
# 75 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 2696 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BREAK ->
                _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState171 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | CONTINUE ->
                _menhir_run138 _menhir_env (Obj.magic _menhir_stack) MenhirState171 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EXIT ->
                _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState171 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LABEL _v ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState171 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState171 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MULT ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState171 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NOP ->
                _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState171 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | PRINT ->
                _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState171 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RETURN ->
                _menhir_run122 _menhir_env (Obj.magic _menhir_stack) MenhirState171 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TFUN ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState171 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TINT ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState171 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TSTRING ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState171 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState171) : 'freshtv574)
        | RP | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv575 * _menhir_state * (
# 75 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 2734 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (x : (
# 75 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 2739 "typ/typ/TYPParser.ml"
            )), _startpos_x_) = _menhir_stack in
            let _v : 'tv_separated_nonempty_list_COMMA_instruction_ = 
# 229 "/home/fusroman/.opam/4.06.0/lib/menhir/standard.mly"
    ( [ x ] )
# 2744 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_separated_nonempty_list_COMMA_instruction_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv576)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv577 * _menhir_state * (
# 75 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 2754 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv578)) : 'freshtv580)
    | MenhirState46 | MenhirState187 | MenhirState181 | MenhirState184 | MenhirState182 | MenhirState179 | MenhirState129 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv587 * _menhir_state * (
# 75 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 2763 "typ/typ/TYPParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv581 * _menhir_state * (
# 75 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 2773 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BREAK ->
                _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | CONTINUE ->
                _menhir_run138 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EXIT ->
                _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOR ->
                _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IF ->
                _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LABEL _v ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MULT ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NOP ->
                _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | PRINT ->
                _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RETURN ->
                _menhir_run122 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TFUN ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TINT ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TSTRING ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | WHILE ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RB ->
                _menhir_reduce60 _menhir_env (Obj.magic _menhir_stack) MenhirState187
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState187) : 'freshtv582)
        | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv583 * _menhir_state * (
# 75 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 2819 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BREAK ->
                _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState179 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | CONTINUE ->
                _menhir_run138 _menhir_env (Obj.magic _menhir_stack) MenhirState179 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EXIT ->
                _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState179 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOR ->
                _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState179 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IF ->
                _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState179 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LABEL _v ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState179 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState179 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MULT ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState179 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NOP ->
                _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState179 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | PRINT ->
                _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState179 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RETURN ->
                _menhir_run122 _menhir_env (Obj.magic _menhir_stack) MenhirState179 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TFUN ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState179 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TINT ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState179 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TSTRING ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState179 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | WHILE ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState179 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RB ->
                _menhir_reduce60 _menhir_env (Obj.magic _menhir_stack) MenhirState179
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState179) : 'freshtv584)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv585 * _menhir_state * (
# 75 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 2867 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv586)) : 'freshtv588)
    | _ ->
        _menhir_fail ()

and _menhir_goto_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 2877 "typ/typ/TYPParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState155 | MenhirState96 | MenhirState67 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv367 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 2887 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADD ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack)
        | COMMA ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv361 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 2901 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ADDRESS ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | BOOL _v ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
            | CPL ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | INT _v ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
            | LABEL _v ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LP ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | MULT ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NOT ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | SUB ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | TFUN ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TINT ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TSTRING ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState96) : 'freshtv362)
        | DIV ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NEQ ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | RP ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv363 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 2963 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (x : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 2968 "typ/typ/TYPParser.ml"
            ))) = _menhir_stack in
            let _v : 'tv_separated_nonempty_list_COMMA_expr_ = 
# 229 "/home/fusroman/.opam/4.06.0/lib/menhir/standard.mly"
    ( [ x ] )
# 2973 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_separated_nonempty_list_COMMA_expr_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv364)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv365 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 2983 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv366)) : 'freshtv368)
    | MenhirState72 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv373 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 2992 "typ/typ/TYPParser.ml"
        ))) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 2996 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIV ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | REM ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | ADD | AND | COLON | COMMA | EQ | GE | GT | LE | LT | NEQ | OR | RB | RP | RS | SEMI | SUB ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv369 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3012 "typ/typ/TYPParser.ml"
            ))) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3016 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (e1 : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3021 "typ/typ/TYPParser.ml"
            ))), _, (e2 : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3025 "typ/typ/TYPParser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3031 "typ/typ/TYPParser.ml"
            ) = 
# 247 "typ/typ/TYPParser.mly"
    ( Binop(e1, Sub, e2) )
# 3035 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv370)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv371 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3045 "typ/typ/TYPParser.ml"
            ))) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3049 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv372)) : 'freshtv374)
    | MenhirState74 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv377 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3058 "typ/typ/TYPParser.ml"
        ))) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3062 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv375 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3068 "typ/typ/TYPParser.ml"
        ))) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3072 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (e1 : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3077 "typ/typ/TYPParser.ml"
        ))), _, (e2 : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3081 "typ/typ/TYPParser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3087 "typ/typ/TYPParser.ml"
        ) = 
# 253 "typ/typ/TYPParser.mly"
    ( Binop(e1, Rem, e2) )
# 3091 "typ/typ/TYPParser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv376)) : 'freshtv378)
    | MenhirState76 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv381 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3099 "typ/typ/TYPParser.ml"
        )) * Lexing.position) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3103 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv379 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3109 "typ/typ/TYPParser.ml"
        )) * Lexing.position) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3113 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s, (e1 : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3118 "typ/typ/TYPParser.ml"
        ))), _startpos__2_), _, (e2 : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3122 "typ/typ/TYPParser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3128 "typ/typ/TYPParser.ml"
        ) = 
# 249 "typ/typ/TYPParser.mly"
    ( Binop(e1, Mult, e2) )
# 3132 "typ/typ/TYPParser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv380)) : 'freshtv382)
    | MenhirState78 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv385 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3140 "typ/typ/TYPParser.ml"
        ))) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3144 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv383 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3150 "typ/typ/TYPParser.ml"
        ))) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3154 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (e1 : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3159 "typ/typ/TYPParser.ml"
        ))), _, (e2 : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3163 "typ/typ/TYPParser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3169 "typ/typ/TYPParser.ml"
        ) = 
# 251 "typ/typ/TYPParser.mly"
    ( Binop(e1, Div, e2) )
# 3173 "typ/typ/TYPParser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv384)) : 'freshtv386)
    | MenhirState80 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv391 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3181 "typ/typ/TYPParser.ml"
        ))) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3185 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADD ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NEQ ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | AND | COLON | COMMA | OR | RB | RP | RS | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv387 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3217 "typ/typ/TYPParser.ml"
            ))) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3221 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (e1 : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3226 "typ/typ/TYPParser.ml"
            ))), _, (e2 : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3230 "typ/typ/TYPParser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3236 "typ/typ/TYPParser.ml"
            ) = 
# 257 "typ/typ/TYPParser.mly"
    ( Binop(e1, Or, e2) )
# 3240 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv388)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv389 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3250 "typ/typ/TYPParser.ml"
            ))) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3254 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv390)) : 'freshtv392)
    | MenhirState82 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv397 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3263 "typ/typ/TYPParser.ml"
        ))) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3267 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADD ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | REM ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | AND | COLON | COMMA | EQ | GE | GT | LE | LT | NEQ | OR | RB | RP | RS | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv393 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3287 "typ/typ/TYPParser.ml"
            ))) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3291 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (e1 : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3296 "typ/typ/TYPParser.ml"
            ))), _, (e2 : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3300 "typ/typ/TYPParser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3306 "typ/typ/TYPParser.ml"
            ) = 
# 269 "typ/typ/TYPParser.mly"
    ( Binop(e1, Neq, e2) )
# 3310 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv394)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv395 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3320 "typ/typ/TYPParser.ml"
            ))) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3324 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv396)) : 'freshtv398)
    | MenhirState84 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv403 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3333 "typ/typ/TYPParser.ml"
        ))) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3337 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIV ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | REM ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | ADD | AND | COLON | COMMA | EQ | GE | GT | LE | LT | NEQ | OR | RB | RP | RS | SEMI | SUB ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv399 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3353 "typ/typ/TYPParser.ml"
            ))) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3357 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (e1 : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3362 "typ/typ/TYPParser.ml"
            ))), _, (e2 : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3366 "typ/typ/TYPParser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3372 "typ/typ/TYPParser.ml"
            ) = 
# 245 "typ/typ/TYPParser.mly"
    ( Binop(e1, Add, e2) )
# 3376 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv400)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv401 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3386 "typ/typ/TYPParser.ml"
            ))) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3390 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv402)) : 'freshtv404)
    | MenhirState86 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv409 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3399 "typ/typ/TYPParser.ml"
        ))) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3403 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADD ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | REM ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | AND | COLON | COMMA | EQ | GE | GT | LE | LT | NEQ | OR | RB | RP | RS | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv405 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3423 "typ/typ/TYPParser.ml"
            ))) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3427 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (e1 : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3432 "typ/typ/TYPParser.ml"
            ))), _, (e2 : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3436 "typ/typ/TYPParser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3442 "typ/typ/TYPParser.ml"
            ) = 
# 259 "typ/typ/TYPParser.mly"
    ( Binop(e1, Lt, e2) )
# 3446 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv406)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv407 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3456 "typ/typ/TYPParser.ml"
            ))) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3460 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv408)) : 'freshtv410)
    | MenhirState88 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv415 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3469 "typ/typ/TYPParser.ml"
        ))) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3473 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADD ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | REM ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | AND | COLON | COMMA | EQ | GE | GT | LE | LT | NEQ | OR | RB | RP | RS | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv411 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3493 "typ/typ/TYPParser.ml"
            ))) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3497 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (e1 : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3502 "typ/typ/TYPParser.ml"
            ))), _, (e2 : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3506 "typ/typ/TYPParser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3512 "typ/typ/TYPParser.ml"
            ) = 
# 261 "typ/typ/TYPParser.mly"
    ( Binop(e1, Le, e2) )
# 3516 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv412)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv413 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3526 "typ/typ/TYPParser.ml"
            ))) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3530 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv414)) : 'freshtv416)
    | MenhirState90 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv421 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3539 "typ/typ/TYPParser.ml"
        ))) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3543 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADD ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | REM ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | AND | COLON | COMMA | EQ | GE | GT | LE | LT | NEQ | OR | RB | RP | RS | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv417 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3563 "typ/typ/TYPParser.ml"
            ))) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3567 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (e1 : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3572 "typ/typ/TYPParser.ml"
            ))), _, (e2 : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3576 "typ/typ/TYPParser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3582 "typ/typ/TYPParser.ml"
            ) = 
# 263 "typ/typ/TYPParser.mly"
    ( Binop(e1, Gt, e2) )
# 3586 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv418)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv419 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3596 "typ/typ/TYPParser.ml"
            ))) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3600 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv420)) : 'freshtv422)
    | MenhirState92 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv427 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3609 "typ/typ/TYPParser.ml"
        ))) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3613 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADD ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | REM ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | AND | COLON | COMMA | EQ | GE | GT | LE | LT | NEQ | OR | RB | RP | RS | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv423 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3633 "typ/typ/TYPParser.ml"
            ))) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3637 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (e1 : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3642 "typ/typ/TYPParser.ml"
            ))), _, (e2 : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3646 "typ/typ/TYPParser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3652 "typ/typ/TYPParser.ml"
            ) = 
# 265 "typ/typ/TYPParser.mly"
    ( Binop(e1, Ge, e2) )
# 3656 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv424)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv425 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3666 "typ/typ/TYPParser.ml"
            ))) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3670 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv426)) : 'freshtv428)
    | MenhirState94 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv433 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3679 "typ/typ/TYPParser.ml"
        ))) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3683 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADD ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | REM ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | AND | COLON | COMMA | EQ | GE | GT | LE | LT | NEQ | OR | RB | RP | RS | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv429 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3703 "typ/typ/TYPParser.ml"
            ))) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3707 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (e1 : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3712 "typ/typ/TYPParser.ml"
            ))), _, (e2 : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3716 "typ/typ/TYPParser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3722 "typ/typ/TYPParser.ml"
            ) = 
# 267 "typ/typ/TYPParser.mly"
    ( Binop(e1, Eq, e2) )
# 3726 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv430)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv431 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3736 "typ/typ/TYPParser.ml"
            ))) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3740 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv432)) : 'freshtv434)
    | MenhirState98 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv439 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3749 "typ/typ/TYPParser.ml"
        ))) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3753 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADD ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NEQ ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | AND | COLON | COMMA | OR | RB | RP | RS | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv435 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3785 "typ/typ/TYPParser.ml"
            ))) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3789 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (e1 : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3794 "typ/typ/TYPParser.ml"
            ))), _, (e2 : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3798 "typ/typ/TYPParser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3804 "typ/typ/TYPParser.ml"
            ) = 
# 255 "typ/typ/TYPParser.mly"
    ( Binop(e1, And, e2) )
# 3808 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv436)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv437 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3818 "typ/typ/TYPParser.ml"
            ))) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3822 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv438)) : 'freshtv440)
    | MenhirState65 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv455 * _menhir_state * (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 3831 "typ/typ/TYPParser.ml"
        ) * Lexing.position)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3835 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADD ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NEQ ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | RB | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv451 * _menhir_state * (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 3871 "typ/typ/TYPParser.ml"
            ) * Lexing.position)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3875 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (f : (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 3880 "typ/typ/TYPParser.ml"
            )), _startpos_f_), _, (e : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3884 "typ/typ/TYPParser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_field_instanciation = let _startpos = _startpos_f_ in
            
# 292 "typ/typ/TYPParser.mly"
    (
      (make_node _startpos f,e)
    )
# 3893 "typ/typ/TYPParser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv449) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_field_instanciation) = _v in
            ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv447 * _menhir_state * 'tv_field_instanciation) = Obj.magic _menhir_stack in
            ((assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | SEMI ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv441 * _menhir_state * 'tv_field_instanciation) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | LABEL _v ->
                    _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState106) : 'freshtv442)
            | RB ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv443 * _menhir_state * 'tv_field_instanciation) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, (x : 'tv_field_instanciation)) = _menhir_stack in
                let _v : 'tv_separated_nonempty_list_SEMI_field_instanciation_ = 
# 229 "/home/fusroman/.opam/4.06.0/lib/menhir/standard.mly"
    ( [ x ] )
# 3924 "typ/typ/TYPParser.ml"
                 in
                _menhir_goto_separated_nonempty_list_SEMI_field_instanciation_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv444)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv445 * _menhir_state * 'tv_field_instanciation) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv446)) : 'freshtv448)) : 'freshtv450)) : 'freshtv452)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv453 * _menhir_state * (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 3941 "typ/typ/TYPParser.ml"
            ) * Lexing.position)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3945 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv454)) : 'freshtv456)
    | MenhirState61 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv463 * _menhir_state * (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3954 "typ/typ/TYPParser.ml"
        ) * Lexing.position)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3958 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADD ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NEQ ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | RS ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv459 * _menhir_state * (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3992 "typ/typ/TYPParser.ml"
            ) * Lexing.position)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 3996 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv457 * _menhir_state * (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4003 "typ/typ/TYPParser.ml"
            ) * Lexing.position)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4007 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (l_expr : (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4012 "typ/typ/TYPParser.ml"
            )), _startpos_l_expr_), _, (e : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4016 "typ/typ/TYPParser.ml"
            ))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _startpos = _startpos_l_expr_ in
            let _v : (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4024 "typ/typ/TYPParser.ml"
            ) = 
# 230 "typ/typ/TYPParser.mly"
    (
      ArrayAccess (l_expr, e)
    )
# 4030 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_l_expr _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv458)) : 'freshtv460)
        | SUB ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv461 * _menhir_state * (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4042 "typ/typ/TYPParser.ml"
            ) * Lexing.position)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4046 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv462)) : 'freshtv464)
    | MenhirState56 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv467 * _menhir_state) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4055 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv465 * _menhir_state) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4061 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _, (e : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4066 "typ/typ/TYPParser.ml"
        ))) = _menhir_stack in
        let _1 = () in
        let _v : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4072 "typ/typ/TYPParser.ml"
        ) = 
# 273 "typ/typ/TYPParser.mly"
    ( Unop(Cpl, e) )
# 4076 "typ/typ/TYPParser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv466)) : 'freshtv468)
    | MenhirState53 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv479 * _menhir_state) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4084 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADD ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NEQ ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | RP ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv475 * _menhir_state) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4118 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LS ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv469 * _menhir_state) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4128 "typ/typ/TYPParser.ml"
                ))) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | ADDRESS ->
                    _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState113
                | BOOL _v ->
                    _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _v
                | CPL ->
                    _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState113
                | INT _v ->
                    _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _v
                | LABEL _v ->
                    _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | LB ->
                    _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | LP ->
                    _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState113
                | MULT ->
                    _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | NOT ->
                    _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState113
                | SUB ->
                    _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState113
                | TFUN ->
                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TINT ->
                    _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TSTRING ->
                    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState113) : 'freshtv470)
            | ADD | AND | COLON | COMMA | DIV | EQ | GE | GT | LE | LT | MULT | NEQ | OR | RB | REM | RP | RS | SEMI | SUB ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv471 * _menhir_state) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4168 "typ/typ/TYPParser.ml"
                ))) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s), _, (e : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4173 "typ/typ/TYPParser.ml"
                ))) = _menhir_stack in
                let _3 = () in
                let _1 = () in
                let _v : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4180 "typ/typ/TYPParser.ml"
                ) = 
# 243 "typ/typ/TYPParser.mly"
    ( e )
# 4184 "typ/typ/TYPParser.ml"
                 in
                _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv472)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv473 * _menhir_state) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4194 "typ/typ/TYPParser.ml"
                ))) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv474)) : 'freshtv476)
        | SUB ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv477 * _menhir_state) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4207 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv478)) : 'freshtv480)
    | MenhirState113 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv487 * _menhir_state) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4216 "typ/typ/TYPParser.ml"
        )))) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4220 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADD ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NEQ ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | RS ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv483 * _menhir_state) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4254 "typ/typ/TYPParser.ml"
            )))) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4258 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv481 * _menhir_state) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4265 "typ/typ/TYPParser.ml"
            )))) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4269 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s), _, (size : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4274 "typ/typ/TYPParser.ml"
            ))), _, (init_elt : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4278 "typ/typ/TYPParser.ml"
            ))) = _menhir_stack in
            let _6 = () in
            let _4 = () in
            let _3 = () in
            let _1 = () in
            let _v : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4287 "typ/typ/TYPParser.ml"
            ) = 
# 285 "typ/typ/TYPParser.mly"
    (
      NewArray (size,init_elt)
    )
# 4293 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv482)) : 'freshtv484)
        | SUB ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv485 * _menhir_state) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4305 "typ/typ/TYPParser.ml"
            )))) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4309 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv486)) : 'freshtv488)
    | MenhirState52 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv495 * _menhir_state * Lexing.position)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4318 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADD ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NEQ ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | RP ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv491 * _menhir_state * Lexing.position)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4352 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv489 * _menhir_state * Lexing.position)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4359 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, _startpos__1_), _, (l : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4364 "typ/typ/TYPParser.ml"
            ))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _v : (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4373 "typ/typ/TYPParser.ml"
            ) = 
# 224 "typ/typ/TYPParser.mly"
    ( l )
# 4377 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_l_expr _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv490)) : 'freshtv492)
        | SUB ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv493 * _menhir_state * Lexing.position)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4389 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv494)) : 'freshtv496)
    | MenhirState50 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv499 * _menhir_state) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4398 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv497 * _menhir_state) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4404 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _, (e : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4409 "typ/typ/TYPParser.ml"
        ))) = _menhir_stack in
        let _1 = () in
        let _v : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4415 "typ/typ/TYPParser.ml"
        ) = 
# 275 "typ/typ/TYPParser.mly"
    ( Unop(Not, e) )
# 4419 "typ/typ/TYPParser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv498)) : 'freshtv500)
    | MenhirState49 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv505 * _menhir_state) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4427 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIV ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | REM ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | ADD | AND | COLON | COMMA | EQ | GE | GT | LE | LT | NEQ | OR | RB | RP | RS | SEMI | SUB ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv501 * _menhir_state) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4443 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (e : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4448 "typ/typ/TYPParser.ml"
            ))) = _menhir_stack in
            let _1 = () in
            let _v : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4454 "typ/typ/TYPParser.ml"
            ) = 
# 271 "typ/typ/TYPParser.mly"
    ( Unop(Minus, e) )
# 4458 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv502)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv503 * _menhir_state) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4468 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv504)) : 'freshtv506)
    | MenhirState48 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv511 * _menhir_state * Lexing.position)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4477 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADD ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NEQ ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | RP ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv507 * _menhir_state * Lexing.position)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4511 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BREAK ->
                _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | CONTINUE ->
                _menhir_run138 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EXIT ->
                _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOR ->
                _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IF ->
                _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LABEL _v ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB ->
                _menhir_run129 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MULT ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NOP ->
                _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | PRINT ->
                _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RETURN ->
                _menhir_run122 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TFUN ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TINT ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TSTRING ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | WHILE ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState121) : 'freshtv508)
        | SUB ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv509 * _menhir_state * Lexing.position)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4559 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv510)) : 'freshtv512)
    | MenhirState122 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv517 * _menhir_state * Lexing.position) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4568 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADD ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NEQ ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | COLON | COMMA | RP | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv513 * _menhir_state * Lexing.position) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4604 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, _startpos__1_), _, (e : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4609 "typ/typ/TYPParser.ml"
            ))) = _menhir_stack in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _v : (
# 75 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 4616 "typ/typ/TYPParser.ml"
            ) = 
# 315 "typ/typ/TYPParser.mly"
    ( Return e )
# 4620 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv514)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv515 * _menhir_state * Lexing.position) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4630 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv516)) : 'freshtv518)
    | MenhirState125 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv525 * _menhir_state * Lexing.position)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4639 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADD ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NEQ ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | RP ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv521 * _menhir_state * Lexing.position)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4673 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv519 * _menhir_state * Lexing.position)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4680 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, _startpos__1_), _, (e : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4685 "typ/typ/TYPParser.ml"
            ))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _v : (
# 75 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 4694 "typ/typ/TYPParser.ml"
            ) = 
# 303 "typ/typ/TYPParser.mly"
    ( Print e )
# 4698 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv520)) : 'freshtv522)
        | SUB ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv523 * _menhir_state * Lexing.position)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4710 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv524)) : 'freshtv526)
    | MenhirState132 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv531 * _menhir_state * Lexing.position)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4719 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADD ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NEQ ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | RP ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv527 * _menhir_state * Lexing.position)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4753 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BREAK ->
                _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | CONTINUE ->
                _menhir_run138 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EXIT ->
                _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOR ->
                _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IF ->
                _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LABEL _v ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB ->
                _menhir_run129 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MULT ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NOP ->
                _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | PRINT ->
                _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RETURN ->
                _menhir_run122 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TFUN ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TINT ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TSTRING ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | WHILE ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState134) : 'freshtv528)
        | SUB ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv529 * _menhir_state * Lexing.position)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4801 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv530)) : 'freshtv532)
    | MenhirState143 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv551 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 4810 "typ/typ/TYPParser.ml"
        ) * Lexing.position) * (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 4814 "typ/typ/TYPParser.ml"
        ) * Lexing.position)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4818 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADD ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NEQ ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | COLON | COMMA | RP | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv547 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 4854 "typ/typ/TYPParser.ml"
            ) * Lexing.position) * (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 4858 "typ/typ/TYPParser.ml"
            ) * Lexing.position)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4862 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (type_variable : (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 4867 "typ/typ/TYPParser.ml"
            )), _startpos_type_variable_), (name : (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 4871 "typ/typ/TYPParser.ml"
            )), _startpos_name_), _, (e : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4875 "typ/typ/TYPParser.ml"
            ))) = _menhir_stack in
            let _3 = () in
            let _startpos = _startpos_type_variable_ in
            let _v : (
# 69 "typ/typ/TYPParser.mly"
      (TYPTree.variable)
# 4882 "typ/typ/TYPParser.ml"
            ) = let _startpos = _startpos_type_variable_ in
            
# 128 "typ/typ/TYPParser.mly"
    (
      (type_variable,make_node _startpos name, e)
    )
# 4889 "typ/typ/TYPParser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv545) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 69 "typ/typ/TYPParser.mly"
      (TYPTree.variable)
# 4897 "typ/typ/TYPParser.ml"
            )) = _v in
            let (_startpos : Lexing.position) = _startpos in
            ((let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
            match _menhir_s with
            | MenhirState199 | MenhirState0 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv539 * _menhir_state * (
# 69 "typ/typ/TYPParser.mly"
      (TYPTree.variable)
# 4907 "typ/typ/TYPParser.ml"
                ) * Lexing.position) = Obj.magic _menhir_stack in
                ((assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | SEMI ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : 'freshtv535 * _menhir_state * (
# 69 "typ/typ/TYPParser.mly"
      (TYPTree.variable)
# 4917 "typ/typ/TYPParser.ml"
                    ) * Lexing.position) = Obj.magic _menhir_stack in
                    ((let _menhir_env = _menhir_discard _menhir_env in
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : 'freshtv533 * _menhir_state * (
# 69 "typ/typ/TYPParser.mly"
      (TYPTree.variable)
# 4924 "typ/typ/TYPParser.ml"
                    ) * Lexing.position) = Obj.magic _menhir_stack in
                    ((let (_menhir_stack, _menhir_s, (var : (
# 69 "typ/typ/TYPParser.mly"
      (TYPTree.variable)
# 4929 "typ/typ/TYPParser.ml"
                    )), _startpos_var_) = _menhir_stack in
                    let _2 = () in
                    let _v : 'tv_global_declaration = 
# 110 "typ/typ/TYPParser.mly"
    (
      Var v
    )
# 4937 "typ/typ/TYPParser.ml"
                     in
                    _menhir_goto_global_declaration _menhir_env _menhir_stack _menhir_s _v) : 'freshtv534)) : 'freshtv536)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : 'freshtv537 * _menhir_state * (
# 69 "typ/typ/TYPParser.mly"
      (TYPTree.variable)
# 4947 "typ/typ/TYPParser.ml"
                    ) * Lexing.position) = Obj.magic _menhir_stack in
                    ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv538)) : 'freshtv540)
            | MenhirState46 | MenhirState121 | MenhirState187 | MenhirState181 | MenhirState184 | MenhirState182 | MenhirState179 | MenhirState129 | MenhirState174 | MenhirState134 | MenhirState171 | MenhirState151 | MenhirState149 | MenhirState136 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv543 * _menhir_state * (
# 69 "typ/typ/TYPParser.mly"
      (TYPTree.variable)
# 4956 "typ/typ/TYPParser.ml"
                ) * Lexing.position) = Obj.magic _menhir_stack in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv541 * _menhir_state * (
# 69 "typ/typ/TYPParser.mly"
      (TYPTree.variable)
# 4962 "typ/typ/TYPParser.ml"
                ) * Lexing.position) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, (v : (
# 69 "typ/typ/TYPParser.mly"
      (TYPTree.variable)
# 4967 "typ/typ/TYPParser.ml"
                )), _startpos_v_) = _menhir_stack in
                let _startpos = _startpos_v_ in
                let _v : (
# 75 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 4973 "typ/typ/TYPParser.ml"
                ) = 
# 333 "typ/typ/TYPParser.mly"
    ( 
      Declaration v
    )
# 4979 "typ/typ/TYPParser.ml"
                 in
                _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv542)) : 'freshtv544)
            | _ ->
                _menhir_fail ()) : 'freshtv546)) : 'freshtv548)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv549 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 4991 "typ/typ/TYPParser.ml"
            ) * Lexing.position) * (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 4995 "typ/typ/TYPParser.ml"
            ) * Lexing.position)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 4999 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv550)) : 'freshtv552)
    | MenhirState147 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv557 * _menhir_state * Lexing.position)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_instruction__)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 5008 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADD ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NEQ ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv553 * _menhir_state * Lexing.position)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_instruction__)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 5042 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BREAK ->
                _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | CONTINUE ->
                _menhir_run138 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EXIT ->
                _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LABEL _v ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MULT ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NOP ->
                _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | PRINT ->
                _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RETURN ->
                _menhir_run122 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TFUN ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TINT ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TSTRING ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RP ->
                _menhir_reduce71 _menhir_env (Obj.magic _menhir_stack) MenhirState149
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState149) : 'freshtv554)
        | SUB ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv555 * _menhir_state * Lexing.position)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_instruction__)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 5086 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv556)) : 'freshtv558)
    | MenhirState164 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv563 * _menhir_state * (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 5095 "typ/typ/TYPParser.ml"
        ) * Lexing.position) * 'tv_assign_binop) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 5099 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADD ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NEQ ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | COLON | COMMA | RP | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv559 * _menhir_state * (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 5135 "typ/typ/TYPParser.ml"
            ) * Lexing.position) * 'tv_assign_binop) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 5139 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (l : (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 5144 "typ/typ/TYPParser.ml"
            )), _startpos_l_), (op : 'tv_assign_binop)), _, (e : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 5148 "typ/typ/TYPParser.ml"
            ))) = _menhir_stack in
            let _startpos = _startpos_l_ in
            let _v : (
# 75 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 5154 "typ/typ/TYPParser.ml"
            ) = 
# 318 "typ/typ/TYPParser.mly"
    (
      BinopAssign(l, op, e)
    )
# 5160 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv560)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv561 * _menhir_state * (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 5170 "typ/typ/TYPParser.ml"
            ) * Lexing.position) * 'tv_assign_binop) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 5174 "typ/typ/TYPParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv562)) : 'freshtv564)
    | _ ->
        _menhir_fail ()

and _menhir_reduce61 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 5184 "typ/typ/TYPParser.ml"
) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (t : (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 5190 "typ/typ/TYPParser.ml"
    )), _startpos_t_) = _menhir_stack in
    let _startpos = _startpos_t_ in
    let _v : (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 5196 "typ/typ/TYPParser.ml"
    ) = let _startpos = _startpos_t_ in
    
# 222 "typ/typ/TYPParser.mly"
    ( Id (make_node _startpos t) )
# 5201 "typ/typ/TYPParser.ml"
     in
    _menhir_goto_l_expr _menhir_env _menhir_stack _menhir_s _v _startpos

and _menhir_goto_separated_nonempty_list_COMMA_parameter_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_COMMA_parameter_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState35 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv355) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_COMMA_parameter_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv353) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((x : 'tv_separated_nonempty_list_COMMA_parameter_) : 'tv_separated_nonempty_list_COMMA_parameter_) = _v in
        ((let _v : 'tv_loption_separated_nonempty_list_COMMA_parameter__ = 
# 144 "/home/fusroman/.opam/4.06.0/lib/menhir/standard.mly"
    ( x )
# 5220 "typ/typ/TYPParser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_COMMA_parameter__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv354)) : 'freshtv356)
    | MenhirState42 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv359 * _menhir_state * (
# 70 "typ/typ/TYPParser.mly"
      (TYPTree.parameter)
# 5228 "typ/typ/TYPParser.ml"
        ))) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_COMMA_parameter_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv357 * _menhir_state * (
# 70 "typ/typ/TYPParser.mly"
      (TYPTree.parameter)
# 5236 "typ/typ/TYPParser.ml"
        ))) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((xs : 'tv_separated_nonempty_list_COMMA_parameter_) : 'tv_separated_nonempty_list_COMMA_parameter_) = _v in
        ((let (_menhir_stack, _menhir_s, (x : (
# 70 "typ/typ/TYPParser.mly"
      (TYPTree.parameter)
# 5243 "typ/typ/TYPParser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_separated_nonempty_list_COMMA_parameter_ = 
# 231 "/home/fusroman/.opam/4.06.0/lib/menhir/standard.mly"
    ( x :: xs )
# 5249 "typ/typ/TYPParser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_parameter_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv358)) : 'freshtv360)
    | _ ->
        _menhir_fail ()

and _menhir_reduce60 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (
# 72 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 5260 "typ/typ/TYPParser.ml"
    ) = 
# 217 "typ/typ/TYPParser.mly"
    ( [] )
# 5264 "typ/typ/TYPParser.ml"
     in
    _menhir_goto_instructions _menhir_env _menhir_stack _menhir_s _v

and _menhir_run47 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LP ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv349 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADDRESS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState48
        | BOOL _v ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
        | CPL ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState48
        | INT _v ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
        | LABEL _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LB ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LP ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState48
        | MULT ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NOT ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState48
        | SUB ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState48
        | TFUN ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TINT ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TSTRING ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState48) : 'freshtv350)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv351 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv352)

and _menhir_run122 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState122
    | BOOL _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _v
    | CPL ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState122
    | INT _v ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _v
    | LABEL _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState122
    | MULT ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState122
    | SUB ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState122
    | TFUN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TINT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TSTRING ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState122

and _menhir_run124 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
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
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | BOOL _v ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState125 _v
        | CPL ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | INT _v ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState125 _v
        | LABEL _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState125 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LB ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState125 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LP ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | MULT ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState125 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NOT ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | SUB ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | TFUN ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState125 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TINT ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState125 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TSTRING ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState125 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState125) : 'freshtv346)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv347 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv348)

and _menhir_run128 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv343) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_startpos__1_ : Lexing.position) = _startpos in
    ((let _1 = () in
    let _startpos = _startpos__1_ in
    let _v : (
# 75 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 5417 "typ/typ/TYPParser.ml"
    ) = 
# 299 "typ/typ/TYPParser.mly"
    ( Nop )
# 5421 "typ/typ/TYPParser.ml"
     in
    _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv344)

and _menhir_run131 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LP ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv339 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADDRESS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState132
        | BOOL _v ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _v
        | CPL ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState132
        | INT _v ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _v
        | LABEL _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LB ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LP ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState132
        | MULT ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NOT ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState132
        | SUB ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState132
        | TFUN ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TINT ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TSTRING ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState132) : 'freshtv340)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv341 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv342)

and _menhir_run135 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LP ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv335 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BREAK ->
            _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | CONTINUE ->
            _menhir_run138 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | EXIT ->
            _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LABEL _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LB ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MULT ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NOP ->
            _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | PRINT ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | RETURN ->
            _menhir_run122 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TFUN ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TINT ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TSTRING ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | SEMI ->
            _menhir_reduce71 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState136) : 'freshtv336)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv337 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv338)

and _menhir_run137 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv333) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_startpos__1_ : Lexing.position) = _startpos in
    ((let _1 = () in
    let _startpos = _startpos__1_ in
    let _v : (
# 75 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 5537 "typ/typ/TYPParser.ml"
    ) = 
# 301 "typ/typ/TYPParser.mly"
    ( Exit )
# 5541 "typ/typ/TYPParser.ml"
     in
    _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv334)

and _menhir_run138 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv331) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_startpos__1_ : Lexing.position) = _startpos in
    ((let _1 = () in
    let _startpos = _startpos__1_ in
    let _v : (
# 75 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 5557 "typ/typ/TYPParser.ml"
    ) = let _startpos = _startpos__1_ in
    
# 310 "typ/typ/TYPParser.mly"
    ( 
      Continue (make_node _startpos ())
    )
# 5564 "typ/typ/TYPParser.ml"
     in
    _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv332)

and _menhir_run139 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv329) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_startpos__1_ : Lexing.position) = _startpos in
    ((let _1 = () in
    let _startpos = _startpos__1_ in
    let _v : (
# 75 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 5580 "typ/typ/TYPParser.ml"
    ) = let _startpos = _startpos__1_ in
    
# 305 "typ/typ/TYPParser.mly"
    ( 
      Break (make_node _startpos ())
    )
# 5587 "typ/typ/TYPParser.ml"
     in
    _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv330)

and _menhir_run49 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | BOOL _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
    | CPL ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | INT _v ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
    | LABEL _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | MULT ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | SUB ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | TFUN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TINT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TSTRING ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState49

and _menhir_run50 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | BOOL _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
    | CPL ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | INT _v ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
    | LABEL _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | MULT ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | SUB ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | TFUN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TINT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TSTRING ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState50

and _menhir_run51 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LP ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv325 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADDRESS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | BOOL _v ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
        | CPL ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | INT _v ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
        | LABEL _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LB ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LP ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | MULT ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NOT ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | SUB ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | TFUN ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TINT ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TSTRING ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState52) : 'freshtv326)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv327 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv328)

and _menhir_run53 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | BOOL _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
    | CPL ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | INT _v ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
    | LABEL _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | MULT ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | SUB ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | TFUN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TINT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TSTRING ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState53

and _menhir_run54 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 5755 "typ/typ/TYPParser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LABEL _ | LB | LS | MULT ->
        _menhir_reduce97 _menhir_env (Obj.magic _menhir_stack)
    | ADD | ADDASSIGN | AND | ASSIGN | COLON | COMMA | DECR | DIV | DIVASSIGN | DOT | EQ | GE | GT | INCR | LE | LP | LT | MULTASSIGN | NEQ | OR | RB | REM | RP | RS | SEMI | SUB | SUBASSIGN ->
        _menhir_reduce61 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv323 * _menhir_state * (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 5773 "typ/typ/TYPParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv324)

and _menhir_run55 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 63 "typ/typ/TYPParser.mly"
       (int)
# 5781 "typ/typ/TYPParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv321) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((i : (
# 63 "typ/typ/TYPParser.mly"
       (int)
# 5791 "typ/typ/TYPParser.ml"
    )) : (
# 63 "typ/typ/TYPParser.mly"
       (int)
# 5795 "typ/typ/TYPParser.ml"
    )) = _v in
    ((let _v : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 5800 "typ/typ/TYPParser.ml"
    ) = 
# 237 "typ/typ/TYPParser.mly"
    ( Int i )
# 5804 "typ/typ/TYPParser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv322)

and _menhir_run56 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | BOOL _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
    | CPL ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | INT _v ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
    | LABEL _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | MULT ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | SUB ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | TFUN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TINT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TSTRING ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState56

and _menhir_run57 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 62 "typ/typ/TYPParser.mly"
       (bool)
# 5848 "typ/typ/TYPParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv319) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((b : (
# 62 "typ/typ/TYPParser.mly"
       (bool)
# 5858 "typ/typ/TYPParser.ml"
    )) : (
# 62 "typ/typ/TYPParser.mly"
       (bool)
# 5862 "typ/typ/TYPParser.ml"
    )) = _v in
    ((let _v : (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 5867 "typ/typ/TYPParser.ml"
    ) = 
# 239 "typ/typ/TYPParser.mly"
    ( Bool b )
# 5871 "typ/typ/TYPParser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv320)

and _menhir_run58 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LABEL _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv317) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState58 in
        let (_v : (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 5888 "typ/typ/TYPParser.ml"
        )) = _v in
        let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        ((let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        _menhir_reduce61 _menhir_env (Obj.magic _menhir_stack)) : 'freshtv318)
    | MULT ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState58

and _menhir_goto_global_declaration : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_global_declaration -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv315 * _menhir_state * 'tv_global_declaration) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LABEL _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState199 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState199 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TFUN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState199 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TINT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState199 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TSTRING ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState199 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TYPE ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState199 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | SEMI ->
        _menhir_reduce67 _menhir_env (Obj.magic _menhir_stack) MenhirState199
    | EOF ->
        _menhir_reduce65 _menhir_env (Obj.magic _menhir_stack) MenhirState199
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState199) : 'freshtv316)

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_run64 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 5938 "typ/typ/TYPParser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ASSIGN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv311 * _menhir_state * (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 5950 "typ/typ/TYPParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADDRESS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | BOOL _v ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
        | CPL ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | INT _v ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
        | LABEL _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LB ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LP ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | MULT ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NOT ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | SUB ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | TFUN ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TINT ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TSTRING ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState65) : 'freshtv312)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv313 * _menhir_state * (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 5992 "typ/typ/TYPParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv314)

and _menhir_goto_parameter : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 70 "typ/typ/TYPParser.mly"
      (TYPTree.parameter)
# 6000 "typ/typ/TYPParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv309 * _menhir_state * (
# 70 "typ/typ/TYPParser.mly"
      (TYPTree.parameter)
# 6008 "typ/typ/TYPParser.ml"
    )) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COMMA ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv303 * _menhir_state * (
# 70 "typ/typ/TYPParser.mly"
      (TYPTree.parameter)
# 6018 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LABEL _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LB ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TFUN ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TINT ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TSTRING ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState42) : 'freshtv304)
    | RP ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv305 * _menhir_state * (
# 70 "typ/typ/TYPParser.mly"
      (TYPTree.parameter)
# 6042 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (x : (
# 70 "typ/typ/TYPParser.mly"
      (TYPTree.parameter)
# 6047 "typ/typ/TYPParser.ml"
        ))) = _menhir_stack in
        let _v : 'tv_separated_nonempty_list_COMMA_parameter_ = 
# 229 "/home/fusroman/.opam/4.06.0/lib/menhir/standard.mly"
    ( [ x ] )
# 6052 "typ/typ/TYPParser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_parameter_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv306)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv307 * _menhir_state * (
# 70 "typ/typ/TYPParser.mly"
      (TYPTree.parameter)
# 6062 "typ/typ/TYPParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv308)) : 'freshtv310)

and _menhir_goto_loption_separated_nonempty_list_COMMA_parameter__ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_loption_separated_nonempty_list_COMMA_parameter__ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ((('freshtv301 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6074 "typ/typ/TYPParser.ml"
    ) * Lexing.position) * (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 6078 "typ/typ/TYPParser.ml"
    ) * Lexing.position)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_parameter__) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RP ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv297 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6088 "typ/typ/TYPParser.ml"
        ) * Lexing.position) * (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 6092 "typ/typ/TYPParser.ml"
        ) * Lexing.position)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_parameter__) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LB ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv293 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6102 "typ/typ/TYPParser.ml"
            ) * Lexing.position) * (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 6106 "typ/typ/TYPParser.ml"
            ) * Lexing.position)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_parameter__)) = Obj.magic _menhir_stack in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_stack = (_menhir_stack, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BREAK ->
                _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | CONTINUE ->
                _menhir_run138 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EXIT ->
                _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOR ->
                _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IF ->
                _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LABEL _v ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MULT ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NOP ->
                _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | PRINT ->
                _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RETURN ->
                _menhir_run122 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TFUN ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TINT ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TSTRING ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | WHILE ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RB ->
                _menhir_reduce60 _menhir_env (Obj.magic _menhir_stack) MenhirState46
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState46) : 'freshtv294)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv295 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6156 "typ/typ/TYPParser.ml"
            ) * Lexing.position) * (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 6160 "typ/typ/TYPParser.ml"
            ) * Lexing.position)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_parameter__)) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv296)) : 'freshtv298)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv299 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6171 "typ/typ/TYPParser.ml"
        ) * Lexing.position) * (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 6175 "typ/typ/TYPParser.ml"
        ) * Lexing.position)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_parameter__) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv300)) : 'freshtv302)

and _menhir_run143 : _menhir_env -> ('ttv_tail * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6183 "typ/typ/TYPParser.ml"
) * Lexing.position) * (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 6187 "typ/typ/TYPParser.ml"
) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState143
    | BOOL _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _v
    | CPL ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState143
    | INT _v ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _v
    | LABEL _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState143
    | MULT ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState143
    | SUB ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState143
    | TFUN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TINT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TSTRING ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState143

and _menhir_goto_separated_nonempty_list_COMMA_type_expr_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_COMMA_type_expr_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState23 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv287 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6232 "typ/typ/TYPParser.ml"
        ) * Lexing.position)) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_COMMA_type_expr_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv285 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6240 "typ/typ/TYPParser.ml"
        ) * Lexing.position)) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((xs : 'tv_separated_nonempty_list_COMMA_type_expr_) : 'tv_separated_nonempty_list_COMMA_type_expr_) = _v in
        ((let (_menhir_stack, _menhir_s, (x : (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6247 "typ/typ/TYPParser.ml"
        )), _startpos_x_) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_separated_nonempty_list_COMMA_type_expr_ = 
# 231 "/home/fusroman/.opam/4.06.0/lib/menhir/standard.mly"
    ( x :: xs )
# 6253 "typ/typ/TYPParser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_type_expr_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv286)) : 'freshtv288)
    | MenhirState8 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv291) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_COMMA_type_expr_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv289) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((x : 'tv_separated_nonempty_list_COMMA_type_expr_) : 'tv_separated_nonempty_list_COMMA_type_expr_) = _v in
        ((let _v : 'tv_loption_separated_nonempty_list_COMMA_type_expr__ = 
# 144 "/home/fusroman/.opam/4.06.0/lib/menhir/standard.mly"
    ( x )
# 6268 "typ/typ/TYPParser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_COMMA_type_expr__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv290)) : 'freshtv292)
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_SEMI_field_declaration_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_SEMI_field_declaration_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState129 | MenhirState9 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv279 * _menhir_state * Lexing.position) * _menhir_state * 'tv_separated_nonempty_list_SEMI_field_declaration_) = Obj.magic _menhir_stack in
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
# 6297 "typ/typ/TYPParser.ml"
            ) = 
# 178 "typ/typ/TYPParser.mly"
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
# 6312 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_type_expr _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv274)) : 'freshtv276)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv277 * _menhir_state * Lexing.position) * _menhir_state * 'tv_separated_nonempty_list_SEMI_field_declaration_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv278)) : 'freshtv280)
    | MenhirState20 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv283 * _menhir_state * 'tv_field_declaration)) * _menhir_state * 'tv_separated_nonempty_list_SEMI_field_declaration_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv281 * _menhir_state * 'tv_field_declaration)) * _menhir_state * 'tv_separated_nonempty_list_SEMI_field_declaration_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (x : 'tv_field_declaration)), _, (xs : 'tv_separated_nonempty_list_SEMI_field_declaration_)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_separated_nonempty_list_SEMI_field_declaration_ = 
# 231 "/home/fusroman/.opam/4.06.0/lib/menhir/standard.mly"
    ( x :: xs )
# 6332 "typ/typ/TYPParser.ml"
         in
        _menhir_goto_separated_nonempty_list_SEMI_field_declaration_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv282)) : 'freshtv284)
    | _ ->
        _menhir_fail ()

and _menhir_run14 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6341 "typ/typ/TYPParser.ml"
) * Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv271 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6349 "typ/typ/TYPParser.ml"
    ) * Lexing.position) = Obj.magic _menhir_stack in
    let (_startpos__2_ : Lexing.position) = _startpos in
    ((let (_menhir_stack, _menhir_s, (t : (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6355 "typ/typ/TYPParser.ml"
    )), _startpos_t_) = _menhir_stack in
    let _2 = () in
    let _startpos = _startpos_t_ in
    let _v : (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6362 "typ/typ/TYPParser.ml"
    ) = 
# 174 "typ/typ/TYPParser.mly"
  (
    TPointer t
  )
# 6368 "typ/typ/TYPParser.ml"
     in
    _menhir_goto_type_expr _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv272)

and _menhir_run15 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6375 "typ/typ/TYPParser.ml"
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
# 6386 "typ/typ/TYPParser.ml"
        ) * Lexing.position)) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv265 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6393 "typ/typ/TYPParser.ml"
        ) * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (t : (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6398 "typ/typ/TYPParser.ml"
        )), _startpos_t_) = _menhir_stack in
        let _3 = () in
        let _2 = () in
        let _startpos = _startpos_t_ in
        let _v : (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6406 "typ/typ/TYPParser.ml"
        ) = 
# 170 "typ/typ/TYPParser.mly"
  (
    TArray (t)
  )
# 6412 "typ/typ/TYPParser.ml"
         in
        _menhir_goto_type_expr _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv266)) : 'freshtv268)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv269 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6422 "typ/typ/TYPParser.ml"
        ) * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv270)

and _menhir_run11 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 6430 "typ/typ/TYPParser.ml"
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
# 6454 "typ/typ/TYPParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv263) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : (
# 68 "typ/typ/TYPParser.mly"
      (TYPTree.typ_prog TYPTree.program)
# 6463 "typ/typ/TYPParser.ml"
    )) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv261) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : (
# 68 "typ/typ/TYPParser.mly"
      (TYPTree.typ_prog TYPTree.program)
# 6471 "typ/typ/TYPParser.ml"
    )) : (
# 68 "typ/typ/TYPParser.mly"
      (TYPTree.typ_prog TYPTree.program)
# 6475 "typ/typ/TYPParser.ml"
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
# 6499 "typ/typ/TYPParser.ml"
            ) = 
# 92 "typ/typ/TYPParser.mly"
    (
      let (genv, _type, tree) = make_env globals in
      {genv, _type, tree}
    )
# 6506 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_program _menhir_env _menhir_stack _menhir_s _v) : 'freshtv250)) : 'freshtv252)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv253 * _menhir_state * 'tv_list_global_declaration_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv254)) : 'freshtv256)
    | MenhirState199 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv259 * _menhir_state * 'tv_global_declaration) * _menhir_state * 'tv_list_global_declaration_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv257 * _menhir_state * 'tv_global_declaration) * _menhir_state * 'tv_list_global_declaration_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (x : 'tv_global_declaration)), _, (xs : 'tv_list_global_declaration_)) = _menhir_stack in
        let _v : 'tv_list_global_declaration_ = 
# 201 "/home/fusroman/.opam/4.06.0/lib/menhir/standard.mly"
    ( x :: xs )
# 6525 "typ/typ/TYPParser.ml"
         in
        _menhir_goto_list_global_declaration_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv258)) : 'freshtv260)
    | _ ->
        _menhir_fail ()

and _menhir_goto_list_type_declaration_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_list_type_declaration_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState192 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv239 * _menhir_state * 'tv_type_declaration) * _menhir_state * 'tv_list_type_declaration_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv237 * _menhir_state * 'tv_type_declaration) * _menhir_state * 'tv_list_type_declaration_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (x : 'tv_type_declaration)), _, (xs : 'tv_list_type_declaration_)) = _menhir_stack in
        let _v : 'tv_list_type_declaration_ = 
# 201 "/home/fusroman/.opam/4.06.0/lib/menhir/standard.mly"
    ( x :: xs )
# 6544 "typ/typ/TYPParser.ml"
         in
        _menhir_goto_list_type_declaration_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv238)) : 'freshtv240)
    | MenhirState199 | MenhirState0 ->
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
# 6566 "typ/typ/TYPParser.ml"
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
# 6582 "typ/typ/TYPParser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState11 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv167 * _menhir_state * (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 6592 "typ/typ/TYPParser.ml"
        ) * Lexing.position)) * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6596 "typ/typ/TYPParser.ml"
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
# 6610 "typ/typ/TYPParser.ml"
            ) * Lexing.position)) * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6614 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (f : (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 6619 "typ/typ/TYPParser.ml"
            )), _startpos_f_), _, (t : (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6623 "typ/typ/TYPParser.ml"
            )), _startpos_t_) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_field_declaration = let _startpos = _startpos_f_ in
            
# 194 "typ/typ/TYPParser.mly"
  (
    ((make_node _startpos f), t)
  )
# 6632 "typ/typ/TYPParser.ml"
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
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState20) : 'freshtv154)
            | RB ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv155 * _menhir_state * 'tv_field_declaration) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, (x : 'tv_field_declaration)) = _menhir_stack in
                let _v : 'tv_separated_nonempty_list_SEMI_field_declaration_ = 
# 229 "/home/fusroman/.opam/4.06.0/lib/menhir/standard.mly"
    ( [ x ] )
# 6663 "typ/typ/TYPParser.ml"
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
# 6680 "typ/typ/TYPParser.ml"
            ) * Lexing.position)) * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6684 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv166)) : 'freshtv168)
    | MenhirState23 | MenhirState8 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv175 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6693 "typ/typ/TYPParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COMMA ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv169 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6703 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LABEL _v ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TFUN ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TINT ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TSTRING ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState23) : 'freshtv170)
        | LS ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | ARROW ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv171 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6731 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (x : (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6736 "typ/typ/TYPParser.ml"
            )), _startpos_x_) = _menhir_stack in
            let _v : 'tv_separated_nonempty_list_COMMA_type_expr_ = 
# 229 "/home/fusroman/.opam/4.06.0/lib/menhir/standard.mly"
    ( [ x ] )
# 6741 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_separated_nonempty_list_COMMA_type_expr_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv172)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv173 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6751 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv174)) : 'freshtv176)
    | MenhirState27 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv183 * _menhir_state * Lexing.position)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_type_expr__)) * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6760 "typ/typ/TYPParser.ml"
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
# 6774 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv177 * _menhir_state * Lexing.position)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_type_expr__)) * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6781 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, _startpos__1_), _, (xs0 : 'tv_loption_separated_nonempty_list_COMMA_type_expr__)), _, (ty : (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6786 "typ/typ/TYPParser.ml"
            )), _startpos_ty_) = _menhir_stack in
            let _6 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _v : (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6796 "typ/typ/TYPParser.ml"
            ) = let ps =
              let xs = xs0 in
              
# 220 "/home/fusroman/.opam/4.06.0/lib/menhir/standard.mly"
    ( xs )
# 6802 "typ/typ/TYPParser.ml"
              
            in
            
# 162 "typ/typ/TYPParser.mly"
  (
    TFun (ps, ty)
  )
# 6810 "typ/typ/TYPParser.ml"
             in
            _menhir_goto_type_expr _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv178)) : 'freshtv180)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv181 * _menhir_state * Lexing.position)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_type_expr__)) * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6820 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv182)) : 'freshtv184)
    | MenhirState4 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv193 * _menhir_state * Lexing.position) * (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 6829 "typ/typ/TYPParser.ml"
        ) * Lexing.position)) * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6833 "typ/typ/TYPParser.ml"
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
# 6847 "typ/typ/TYPParser.ml"
            ) * Lexing.position)) * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6851 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, _startpos__1_), (name_type : (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 6856 "typ/typ/TYPParser.ml"
            )), _startpos_name_type_), _, (_type : (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6860 "typ/typ/TYPParser.ml"
            )), _startpos__type_) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_type_declaration = let _startpos = _startpos__1_ in
            
# 121 "typ/typ/TYPParser.mly"
  (
    (make_node _startpos name_type, _type)
  )
# 6870 "typ/typ/TYPParser.ml"
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
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState192 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | SEMI ->
                _menhir_reduce67 _menhir_env (Obj.magic _menhir_stack) MenhirState192
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState192) : 'freshtv186)) : 'freshtv188)) : 'freshtv190)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv191 * _menhir_state * Lexing.position) * (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 6897 "typ/typ/TYPParser.ml"
            ) * Lexing.position)) * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6901 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv192)) : 'freshtv194)
    | MenhirState199 | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv205 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6910 "typ/typ/TYPParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LABEL _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv201 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6920 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            let (_v : (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 6925 "typ/typ/TYPParser.ml"
            )) = _v in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_stack = (_menhir_stack, _v, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ASSIGN ->
                _menhir_run143 _menhir_env (Obj.magic _menhir_stack)
            | LP ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv197 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6939 "typ/typ/TYPParser.ml"
                ) * Lexing.position) * (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 6943 "typ/typ/TYPParser.ml"
                ) * Lexing.position) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | LABEL _v ->
                    _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | LB ->
                    _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TFUN ->
                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TINT ->
                    _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TSTRING ->
                    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | RP ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : 'freshtv195) = Obj.magic _menhir_stack in
                    let (_menhir_s : _menhir_state) = MenhirState35 in
                    ((let _v : 'tv_loption_separated_nonempty_list_COMMA_parameter__ = 
# 142 "/home/fusroman/.opam/4.06.0/lib/menhir/standard.mly"
    ( [] )
# 6965 "typ/typ/TYPParser.ml"
                     in
                    _menhir_goto_loption_separated_nonempty_list_COMMA_parameter__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv196)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState35) : 'freshtv198)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv199 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 6979 "typ/typ/TYPParser.ml"
                ) * Lexing.position) * (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 6983 "typ/typ/TYPParser.ml"
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
# 6998 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv204)) : 'freshtv206)
    | MenhirState42 | MenhirState35 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv221 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 7007 "typ/typ/TYPParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADDRESS ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv213 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 7017 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LABEL _v ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv209 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 7027 "typ/typ/TYPParser.ml"
                ) * Lexing.position)) = Obj.magic _menhir_stack in
                let (_v : (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 7032 "typ/typ/TYPParser.ml"
                )) = _v in
                let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv207 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 7040 "typ/typ/TYPParser.ml"
                ) * Lexing.position)) = Obj.magic _menhir_stack in
                let ((name : (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 7045 "typ/typ/TYPParser.ml"
                )) : (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 7049 "typ/typ/TYPParser.ml"
                )) = _v in
                let (_startpos_name_ : Lexing.position) = _startpos in
                ((let (_menhir_stack, _menhir_s, (params_type : (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 7055 "typ/typ/TYPParser.ml"
                )), _startpos_params_type_) = _menhir_stack in
                let _2 = () in
                let _v : (
# 70 "typ/typ/TYPParser.mly"
      (TYPTree.parameter)
# 7061 "typ/typ/TYPParser.ml"
                ) = let _startpos = _startpos_params_type_ in
                
# 140 "typ/typ/TYPParser.mly"
    (
      {name = make_node _startpos name; reference = true; params_type}
    )
# 7068 "typ/typ/TYPParser.ml"
                 in
                _menhir_goto_parameter _menhir_env _menhir_stack _menhir_s _v) : 'freshtv208)) : 'freshtv210)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv211 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 7078 "typ/typ/TYPParser.ml"
                ) * Lexing.position)) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv212)) : 'freshtv214)
        | LABEL _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv217 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 7087 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            let (_v : (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 7092 "typ/typ/TYPParser.ml"
            )) = _v in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv215 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 7100 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            let ((name : (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 7105 "typ/typ/TYPParser.ml"
            )) : (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 7109 "typ/typ/TYPParser.ml"
            )) = _v in
            let (_startpos_name_ : Lexing.position) = _startpos in
            ((let (_menhir_stack, _menhir_s, (params_type : (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 7115 "typ/typ/TYPParser.ml"
            )), _startpos_params_type_) = _menhir_stack in
            let _v : (
# 70 "typ/typ/TYPParser.mly"
      (TYPTree.parameter)
# 7120 "typ/typ/TYPParser.ml"
            ) = let _startpos = _startpos_params_type_ in
            
# 135 "typ/typ/TYPParser.mly"
    (
      {name = make_node _startpos name; reference = false; params_type}
    )
# 7127 "typ/typ/TYPParser.ml"
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
# 7141 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv220)) : 'freshtv222)
    | MenhirState164 | MenhirState155 | MenhirState147 | MenhirState143 | MenhirState132 | MenhirState125 | MenhirState122 | MenhirState48 | MenhirState49 | MenhirState50 | MenhirState52 | MenhirState113 | MenhirState53 | MenhirState56 | MenhirState98 | MenhirState96 | MenhirState94 | MenhirState92 | MenhirState90 | MenhirState88 | MenhirState86 | MenhirState84 | MenhirState82 | MenhirState80 | MenhirState78 | MenhirState76 | MenhirState74 | MenhirState72 | MenhirState67 | MenhirState65 | MenhirState61 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv227 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 7150 "typ/typ/TYPParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LB ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv223 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 7160 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_stack = (_menhir_stack, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LABEL _v ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState63) : 'freshtv224)
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
# 7184 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv226)) : 'freshtv228)
    | MenhirState46 | MenhirState121 | MenhirState187 | MenhirState181 | MenhirState184 | MenhirState182 | MenhirState179 | MenhirState129 | MenhirState174 | MenhirState134 | MenhirState171 | MenhirState151 | MenhirState149 | MenhirState136 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv235 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 7193 "typ/typ/TYPParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LABEL _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv231 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 7203 "typ/typ/TYPParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            let (_v : (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 7208 "typ/typ/TYPParser.ml"
            )) = _v in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_stack = (_menhir_stack, _v, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ASSIGN ->
                _menhir_run143 _menhir_env (Obj.magic _menhir_stack)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv229 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 7224 "typ/typ/TYPParser.ml"
                ) * Lexing.position) * (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 7228 "typ/typ/TYPParser.ml"
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
# 7243 "typ/typ/TYPParser.ml"
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
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LB ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TFUN ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TINT ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TSTRING ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState27) : 'freshtv148)
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
# 7289 "typ/typ/TYPParser.ml"
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
# 7305 "typ/typ/TYPParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv146)

and _menhir_reduce97 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 7313 "typ/typ/TYPParser.ml"
) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (l : (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 7319 "typ/typ/TYPParser.ml"
    )), _startpos_l_) = _menhir_stack in
    let _startpos = _startpos_l_ in
    let _v : (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 7325 "typ/typ/TYPParser.ml"
    ) = let _startpos = _startpos_l_ in
    
# 166 "typ/typ/TYPParser.mly"
  (
    TAlias (make_node _startpos l)
  )
# 7332 "typ/typ/TYPParser.ml"
     in
    _menhir_goto_type_expr _menhir_env _menhir_stack _menhir_s _v _startpos

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState199 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv21 * _menhir_state * 'tv_global_declaration) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv22)
    | MenhirState192 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv23 * _menhir_state * 'tv_type_declaration) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv24)
    | MenhirState187 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv25 * _menhir_state * (
# 75 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 7354 "typ/typ/TYPParser.ml"
        ) * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv26)
    | MenhirState184 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv27 * _menhir_state * (
# 77 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 7363 "typ/typ/TYPParser.ml"
        ) * Lexing.position) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv28)
    | MenhirState182 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv29 * _menhir_state * (
# 77 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 7372 "typ/typ/TYPParser.ml"
        ) * Lexing.position) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv30)
    | MenhirState181 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv31 * _menhir_state * (
# 77 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 7381 "typ/typ/TYPParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv32)
    | MenhirState179 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv33 * _menhir_state * (
# 75 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 7390 "typ/typ/TYPParser.ml"
        ) * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv34)
    | MenhirState174 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv35 * _menhir_state * Lexing.position)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 7399 "typ/typ/TYPParser.ml"
        ))) * _menhir_state * (
# 76 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instrs)
# 7403 "typ/typ/TYPParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv36)
    | MenhirState171 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv37 * _menhir_state * (
# 75 "typ/typ/TYPParser.mly"
      (TYPTree.typ_instr)
# 7412 "typ/typ/TYPParser.ml"
        ) * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv38)
    | MenhirState164 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv39 * _menhir_state * (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 7421 "typ/typ/TYPParser.ml"
        ) * Lexing.position) * 'tv_assign_binop) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv40)
    | MenhirState155 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv41 * _menhir_state * (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 7430 "typ/typ/TYPParser.ml"
        ) * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv42)
    | MenhirState151 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((('freshtv43 * _menhir_state * Lexing.position)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_instruction__)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 7439 "typ/typ/TYPParser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_instruction__)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv44)
    | MenhirState149 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv45 * _menhir_state * Lexing.position)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_instruction__)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 7448 "typ/typ/TYPParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv46)
    | MenhirState147 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv47 * _menhir_state * Lexing.position)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_instruction__)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv48)
    | MenhirState143 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv49 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 7462 "typ/typ/TYPParser.ml"
        ) * Lexing.position) * (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 7466 "typ/typ/TYPParser.ml"
        ) * Lexing.position)) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _, _), _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv50)
    | MenhirState136 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv51 * _menhir_state * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv52)
    | MenhirState134 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv53 * _menhir_state * Lexing.position)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 7480 "typ/typ/TYPParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv54)
    | MenhirState132 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv55 * _menhir_state * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv56)
    | MenhirState129 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv57 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv58)
    | MenhirState125 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv59 * _menhir_state * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv60)
    | MenhirState122 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv61 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv62)
    | MenhirState121 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv63 * _menhir_state * Lexing.position)) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 7509 "typ/typ/TYPParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv64)
    | MenhirState113 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv65 * _menhir_state) * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 7518 "typ/typ/TYPParser.ml"
        )))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv66)
    | MenhirState106 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv67 * _menhir_state * 'tv_field_instanciation)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv68)
    | MenhirState98 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv69 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 7532 "typ/typ/TYPParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv70)
    | MenhirState96 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv71 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 7541 "typ/typ/TYPParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv72)
    | MenhirState94 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv73 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 7550 "typ/typ/TYPParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv74)
    | MenhirState92 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv75 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 7559 "typ/typ/TYPParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv76)
    | MenhirState90 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv77 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 7568 "typ/typ/TYPParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv78)
    | MenhirState88 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv79 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 7577 "typ/typ/TYPParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv80)
    | MenhirState86 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv81 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 7586 "typ/typ/TYPParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv82)
    | MenhirState84 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv83 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 7595 "typ/typ/TYPParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv84)
    | MenhirState82 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv85 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 7604 "typ/typ/TYPParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv86)
    | MenhirState80 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv87 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 7613 "typ/typ/TYPParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv88)
    | MenhirState78 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv89 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 7622 "typ/typ/TYPParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv90)
    | MenhirState76 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv91 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 7631 "typ/typ/TYPParser.ml"
        )) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv92)
    | MenhirState74 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv93 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 7640 "typ/typ/TYPParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv94)
    | MenhirState72 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv95 * _menhir_state * (
# 73 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 7649 "typ/typ/TYPParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv96)
    | MenhirState67 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv97 * _menhir_state * (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 7658 "typ/typ/TYPParser.ml"
        ) * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv98)
    | MenhirState65 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv99 * _menhir_state * (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 7667 "typ/typ/TYPParser.ml"
        ) * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv100)
    | MenhirState63 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv101 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 7676 "typ/typ/TYPParser.ml"
        ) * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv102)
    | MenhirState61 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv103 * _menhir_state * (
# 74 "typ/typ/TYPParser.mly"
      (TYPTree.typ_expression)
# 7685 "typ/typ/TYPParser.ml"
        ) * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv104)
    | MenhirState58 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv105 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv106)
    | MenhirState56 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv107 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv108)
    | MenhirState53 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv109 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv110)
    | MenhirState52 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv111 * _menhir_state * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv112)
    | MenhirState50 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv113 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv114)
    | MenhirState49 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv115 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv116)
    | MenhirState48 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv117 * _menhir_state * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv118)
    | MenhirState46 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv119 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 7729 "typ/typ/TYPParser.ml"
        ) * Lexing.position) * (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 7733 "typ/typ/TYPParser.ml"
        ) * Lexing.position)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_parameter__)) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv120)
    | MenhirState42 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv121 * _menhir_state * (
# 70 "typ/typ/TYPParser.mly"
      (TYPTree.parameter)
# 7742 "typ/typ/TYPParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv122)
    | MenhirState35 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv123 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 7751 "typ/typ/TYPParser.ml"
        ) * Lexing.position) * (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 7755 "typ/typ/TYPParser.ml"
        ) * Lexing.position)) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _, _), _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv124)
    | MenhirState27 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv125 * _menhir_state * Lexing.position)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_type_expr__)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv126)
    | MenhirState23 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv127 * _menhir_state * (
# 78 "typ/typ/TYPParser.mly"
      (TYPTree._type)
# 7769 "typ/typ/TYPParser.ml"
        ) * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv128)
    | MenhirState20 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv129 * _menhir_state * 'tv_field_declaration)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv130)
    | MenhirState11 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv131 * _menhir_state * (
# 64 "typ/typ/TYPParser.mly"
       (string)
# 7783 "typ/typ/TYPParser.ml"
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
# 7802 "typ/typ/TYPParser.ml"
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
# 7821 "typ/typ/TYPParser.ml"
        ) = let _startpos = _startpos__1_ in
        
# 98 "typ/typ/TYPParser.mly"
    ( 
      raise_syntax_error _startpos "Syntax error" 
    )
# 7828 "typ/typ/TYPParser.ml"
         in
        _menhir_goto_program _menhir_env _menhir_stack _menhir_s _v) : 'freshtv140)) : 'freshtv142)) : 'freshtv144)

and _menhir_reduce65 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_list_global_declaration_ = 
# 199 "/home/fusroman/.opam/4.06.0/lib/menhir/standard.mly"
    ( [] )
# 7837 "typ/typ/TYPParser.ml"
     in
    _menhir_goto_list_global_declaration_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce67 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_list_type_declaration_ = 
# 199 "/home/fusroman/.opam/4.06.0/lib/menhir/standard.mly"
    ( [] )
# 7846 "typ/typ/TYPParser.ml"
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
# 7862 "typ/typ/TYPParser.ml"
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
# 7874 "typ/typ/TYPParser.ml"
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
# 7900 "typ/typ/TYPParser.ml"
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
# 7924 "typ/typ/TYPParser.ml"
    ) = 
# 158 "typ/typ/TYPParser.mly"
  (
    TArray (TChar)
  )
# 7930 "typ/typ/TYPParser.ml"
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
# 7946 "typ/typ/TYPParser.ml"
    ) = 
# 154 "typ/typ/TYPParser.mly"
  (
    TInt
  )
# 7952 "typ/typ/TYPParser.ml"
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
# 7985 "typ/typ/TYPParser.ml"
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
# 8016 "typ/typ/TYPParser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce97 _menhir_env (Obj.magic _menhir_stack)

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
# 8038 "typ/typ/TYPParser.ml"
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
        _menhir_reduce67 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | EOF ->
        _menhir_reduce65 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0) : 'freshtv2))

# 233 "/home/fusroman/.opam/4.06.0/lib/menhir/standard.mly"
  

# 8081 "typ/typ/TYPParser.ml"
