
module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | WHILE
    | SUBASSIGN
    | SUB
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
# 46 "var/cll/CLLParser.mly"
       (string)
# 31 "var/cll/CLLParser.ml"
  )
    | INT of (
# 45 "var/cll/CLLParser.mly"
       (int)
# 36 "var/cll/CLLParser.ml"
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
# 44 "var/cll/CLLParser.mly"
       (bool)
# 59 "var/cll/CLLParser.ml"
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
  | MenhirState141
  | MenhirState139
  | MenhirState136
  | MenhirState131
  | MenhirState126
  | MenhirState123
  | MenhirState121
  | MenhirState120
  | MenhirState118
  | MenhirState113
  | MenhirState110
  | MenhirState108
  | MenhirState105
  | MenhirState103
  | MenhirState101
  | MenhirState99
  | MenhirState96
  | MenhirState94
  | MenhirState81
  | MenhirState78
  | MenhirState75
  | MenhirState65
  | MenhirState63
  | MenhirState61
  | MenhirState59
  | MenhirState58
  | MenhirState56
  | MenhirState48
  | MenhirState46
  | MenhirState44
  | MenhirState42
  | MenhirState40
  | MenhirState38
  | MenhirState36
  | MenhirState34
  | MenhirState32
  | MenhirState29
  | MenhirState27
  | MenhirState25
  | MenhirState23
  | MenhirState16
  | MenhirState13
  | MenhirState12
  | MenhirState11
  | MenhirState10
  | MenhirState9
  | MenhirState6
  | MenhirState5
  | MenhirState0

# 1 "var/cll/CLLParser.mly"
  
  open Lexing
  open Tagset
  open ARTTree
  open CLLTree
  open IMPTree

  let get_line pos =
    pos.pos_lnum

  let get_column pos =
    pos.pos_cnum - pos.pos_bol

  let raise_syntax_error pos msg =
    raise (SyntaxError(msg, get_line pos, get_column pos))

  let raise_duplicate_element pos t =
    raise_syntax_error pos (Printf.sprintf "Tag '%s' is declared at least twice" t)

  let raise_reserved_variable pos var=
    raise_syntax_error pos (Printf.sprintf "'%s' is a reserved variable." var)

  let make_node pos contents =
    {line = get_line pos; column = get_column pos; contents}

# 158 "var/cll/CLLParser.ml"

let rec _menhir_goto_assigns : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 163 "var/cll/CLLParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState78 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv499 * _menhir_state * Lexing.position)) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 173 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 177 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RP ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv495 * _menhir_state * Lexing.position)) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 187 "var/cll/CLLParser.ml"
            ))) * _menhir_state * (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 191 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BREAK ->
                _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | CONTINUE ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EXIT ->
                _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOR ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IF ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LABEL _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | MULT ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NOP ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | PRINT ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RETURN ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | WHILE ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState81) : 'freshtv496)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv497 * _menhir_state * Lexing.position)) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 231 "var/cll/CLLParser.ml"
            ))) * _menhir_state * (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 235 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv498)) : 'freshtv500)
    | MenhirState94 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv503 * _menhir_state * (
# 55 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 244 "var/cll/CLLParser.ml"
        ) * Lexing.position)) * _menhir_state * (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 248 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv501 * _menhir_state * (
# 55 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 254 "var/cll/CLLParser.ml"
        ) * Lexing.position)) * _menhir_state * (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 258 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (a : (
# 55 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 263 "var/cll/CLLParser.ml"
        )), _startpos_a_), _, (s : (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 267 "var/cll/CLLParser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 273 "var/cll/CLLParser.ml"
        ) = 
# 306 "var/cll/CLLParser.mly"
                           ( Cycle.prepend s a )
# 277 "var/cll/CLLParser.ml"
         in
        _menhir_goto_assigns _menhir_env _menhir_stack _menhir_s _v) : 'freshtv502)) : 'freshtv504)
    | MenhirState65 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv511 * _menhir_state * Lexing.position)) * _menhir_state * (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 285 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RP ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv505 * _menhir_state * Lexing.position)) * _menhir_state * (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 295 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BREAK ->
                _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | CONTINUE ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EXIT ->
                _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOR ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IF ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LABEL _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState110
            | MULT ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NOP ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | PRINT ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RETURN ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | WHILE ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState110) : 'freshtv506)
        | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv507 * _menhir_state * Lexing.position)) * _menhir_state * (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 333 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ADDRESS ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BOOL _v ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v
            | CPL ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState99
            | INT _v ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v
            | LABEL _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LP ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState99
            | MULT ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NOT ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState99
            | SUB ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState99
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState99) : 'freshtv508)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv509 * _menhir_state * Lexing.position)) * _menhir_state * (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 367 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv510)) : 'freshtv512)
    | MenhirState101 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv517 * _menhir_state * Lexing.position)) * _menhir_state * (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 376 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 380 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 384 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RP ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv513 * _menhir_state * Lexing.position)) * _menhir_state * (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 394 "var/cll/CLLParser.ml"
            ))) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 398 "var/cll/CLLParser.ml"
            ))) * _menhir_state * (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 402 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BREAK ->
                _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | CONTINUE ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EXIT ->
                _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOR ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IF ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LABEL _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState103
            | MULT ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NOP ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | PRINT ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RETURN ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | WHILE ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState103) : 'freshtv514)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv515 * _menhir_state * Lexing.position)) * _menhir_state * (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 442 "var/cll/CLLParser.ml"
            ))) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 446 "var/cll/CLLParser.ml"
            ))) * _menhir_state * (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 450 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv516)) : 'freshtv518)
    | MenhirState99 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv523 * _menhir_state * Lexing.position)) * _menhir_state * (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 459 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 463 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RP ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv519 * _menhir_state * Lexing.position)) * _menhir_state * (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 473 "var/cll/CLLParser.ml"
            ))) * _menhir_state * (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 477 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BREAK ->
                _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | CONTINUE ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EXIT ->
                _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOR ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IF ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LABEL _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | MULT ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NOP ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | PRINT ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RETURN ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | WHILE ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState108) : 'freshtv520)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv521 * _menhir_state * Lexing.position)) * _menhir_state * (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 517 "var/cll/CLLParser.ml"
            ))) * _menhir_state * (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 521 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv522)) : 'freshtv524)
    | _ ->
        _menhir_fail ()

and _menhir_goto_assign_unop : _menhir_env -> 'ttv_tail -> 'tv_assign_unop -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv493 * _menhir_state * (
# 53 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 534 "var/cll/CLLParser.ml"
    ) * Lexing.position) = Obj.magic _menhir_stack in
    let (_v : 'tv_assign_unop) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv491 * _menhir_state * (
# 53 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 541 "var/cll/CLLParser.ml"
    ) * Lexing.position) = Obj.magic _menhir_stack in
    let ((op : 'tv_assign_unop) : 'tv_assign_unop) = _v in
    ((let (_menhir_stack, _menhir_s, (l : (
# 53 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 547 "var/cll/CLLParser.ml"
    )), _startpos_l_) = _menhir_stack in
    let _startpos = _startpos_l_ in
    let _v : (
# 55 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 553 "var/cll/CLLParser.ml"
    ) = 
# 233 "var/cll/CLLParser.mly"
    (
      UnopAssign(l, op)
    )
# 559 "var/cll/CLLParser.ml"
     in
    _menhir_goto_assign _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv492)) : 'freshtv494)

and _menhir_goto_assign_binop : _menhir_env -> 'ttv_tail -> 'tv_assign_binop -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv489 * _menhir_state * (
# 53 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 570 "var/cll/CLLParser.ml"
    ) * Lexing.position) * 'tv_assign_binop) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BOOL _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
    | CPL ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | INT _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
    | LABEL _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | MULT ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | SUB ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState75) : 'freshtv490)

and _menhir_goto_assign : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 55 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 601 "var/cll/CLLParser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState5 | MenhirState6 | MenhirState58 | MenhirState59 | MenhirState126 | MenhirState118 | MenhirState120 | MenhirState123 | MenhirState121 | MenhirState63 | MenhirState113 | MenhirState110 | MenhirState108 | MenhirState105 | MenhirState103 | MenhirState96 | MenhirState81 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv479 * _menhir_state * (
# 55 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 611 "var/cll/CLLParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv477 * _menhir_state * (
# 55 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 617 "var/cll/CLLParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (a : (
# 55 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 622 "var/cll/CLLParser.ml"
        )), _startpos_a_) = _menhir_stack in
        let _startpos = _startpos_a_ in
        let _v : (
# 54 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 628 "var/cll/CLLParser.ml"
        ) = 
# 223 "var/cll/CLLParser.mly"
    ( a )
# 632 "var/cll/CLLParser.ml"
         in
        _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv478)) : 'freshtv480)
    | MenhirState65 | MenhirState99 | MenhirState101 | MenhirState94 | MenhirState78 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv487 * _menhir_state * (
# 55 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 640 "var/cll/CLLParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COMMA ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv481 * _menhir_state * (
# 55 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 650 "var/cll/CLLParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LABEL _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MULT ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState94) : 'freshtv482)
        | RP | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv483 * _menhir_state * (
# 55 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 668 "var/cll/CLLParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (a : (
# 55 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 673 "var/cll/CLLParser.ml"
            )), _startpos_a_) = _menhir_stack in
            let _v : (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 678 "var/cll/CLLParser.ml"
            ) = 
# 305 "var/cll/CLLParser.mly"
            ( Cycle.from_elt a )
# 682 "var/cll/CLLParser.ml"
             in
            _menhir_goto_assigns _menhir_env _menhir_stack _menhir_s _v) : 'freshtv484)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv485 * _menhir_state * (
# 55 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 692 "var/cll/CLLParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv486)) : 'freshtv488)
    | _ ->
        _menhir_fail ()

and _menhir_run23 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 702 "var/cll/CLLParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BOOL _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
    | CPL ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | INT _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
    | LABEL _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | MULT ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | SUB ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState23

and _menhir_run25 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 734 "var/cll/CLLParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BOOL _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
    | CPL ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | INT _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
    | LABEL _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | MULT ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | SUB ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState25

and _menhir_run32 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 766 "var/cll/CLLParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BOOL _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
    | CPL ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | INT _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
    | LABEL _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | MULT ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | SUB ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState32

and _menhir_run34 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 798 "var/cll/CLLParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BOOL _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
    | CPL ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | INT _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
    | LABEL _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | MULT ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | SUB ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState34

and _menhir_run27 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 830 "var/cll/CLLParser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _startpos ->
    let _menhir_stack = (_menhir_stack, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BOOL _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
    | CPL ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | INT _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
    | LABEL _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | MULT ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | SUB ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState27

and _menhir_run38 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 863 "var/cll/CLLParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BOOL _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | CPL ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | INT _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | LABEL _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | MULT ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | SUB ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState38

and _menhir_run40 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 895 "var/cll/CLLParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BOOL _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | CPL ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | INT _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | LABEL _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | MULT ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | SUB ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState40

and _menhir_run42 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 927 "var/cll/CLLParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BOOL _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
    | CPL ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | INT _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
    | LABEL _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | MULT ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | SUB ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState42

and _menhir_run44 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 959 "var/cll/CLLParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BOOL _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | CPL ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | INT _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | LABEL _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | MULT ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | SUB ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState44

and _menhir_run46 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 991 "var/cll/CLLParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BOOL _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | CPL ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | INT _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | LABEL _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | MULT ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | SUB ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState46

and _menhir_run29 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1023 "var/cll/CLLParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BOOL _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | CPL ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | INT _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | LABEL _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | MULT ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | SUB ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState29

and _menhir_run48 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1055 "var/cll/CLLParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BOOL _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | CPL ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | INT _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | LABEL _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | MULT ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | SUB ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState48

and _menhir_run36 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1087 "var/cll/CLLParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BOOL _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
    | CPL ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | INT _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
    | LABEL _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | MULT ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | SUB ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState36

and _menhir_goto_control : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 57 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 1119 "var/cll/CLLParser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState6 | MenhirState58 | MenhirState113 | MenhirState63 | MenhirState110 | MenhirState108 | MenhirState105 | MenhirState103 | MenhirState96 | MenhirState81 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv469 * _menhir_state * (
# 57 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 1129 "var/cll/CLLParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv467 * _menhir_state * (
# 57 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 1135 "var/cll/CLLParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (c : (
# 57 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 1140 "var/cll/CLLParser.ml"
        )), _startpos_c_) = _menhir_stack in
        let _v : (
# 56 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 1145 "var/cll/CLLParser.ml"
        ) = 
# 258 "var/cll/CLLParser.mly"
    ( c )
# 1149 "var/cll/CLLParser.ml"
         in
        _menhir_goto_block _menhir_env _menhir_stack _menhir_s _v) : 'freshtv468)) : 'freshtv470)
    | MenhirState5 | MenhirState59 | MenhirState126 | MenhirState120 | MenhirState123 | MenhirState121 | MenhirState118 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv475 * _menhir_state * (
# 57 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 1157 "var/cll/CLLParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BREAK ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | COLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv471 * _menhir_state * (
# 57 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 1169 "var/cll/CLLParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState120 in
            ((let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BREAK ->
                _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | CONTINUE ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EXIT ->
                _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOR ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IF ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LABEL _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MULT ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NOP ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | PRINT ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RETURN ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | WHILE ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RB ->
                _menhir_reduce62 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState123) : 'freshtv472)
        | CONTINUE ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | EXIT ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | FOR ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | IF ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LABEL _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MULT ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NOP ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | PRINT ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | RETURN ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv473 * _menhir_state * (
# 57 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 1227 "var/cll/CLLParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState120 in
            ((let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BREAK ->
                _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | CONTINUE ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EXIT ->
                _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOR ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IF ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LABEL _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MULT ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NOP ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | PRINT ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RETURN ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | WHILE ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RB ->
                _menhir_reduce62 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState121) : 'freshtv474)
        | WHILE ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | RB ->
            _menhir_reduce62 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState120) : 'freshtv476)
    | _ ->
        _menhir_fail ()

and _menhir_goto_data_declarations : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 59 "var/cll/CLLParser.mly"
      (ARTTree.datas ARTTree.compiler_type)
# 1276 "var/cll/CLLParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState139 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv453 * _menhir_state * (
# 46 "var/cll/CLLParser.mly"
       (string)
# 1286 "var/cll/CLLParser.ml"
        ) * Lexing.position)) * (
# 45 "var/cll/CLLParser.mly"
       (int)
# 1290 "var/cll/CLLParser.ml"
        )) * _menhir_state * (
# 59 "var/cll/CLLParser.mly"
      (ARTTree.datas ARTTree.compiler_type)
# 1294 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv451 * _menhir_state * (
# 46 "var/cll/CLLParser.mly"
       (string)
# 1300 "var/cll/CLLParser.ml"
        ) * Lexing.position)) * (
# 45 "var/cll/CLLParser.mly"
       (int)
# 1304 "var/cll/CLLParser.ml"
        )) * _menhir_state * (
# 59 "var/cll/CLLParser.mly"
      (ARTTree.datas ARTTree.compiler_type)
# 1308 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s, (t : (
# 46 "var/cll/CLLParser.mly"
       (string)
# 1313 "var/cll/CLLParser.ml"
        )), _startpos_t_), (v : (
# 45 "var/cll/CLLParser.mly"
       (int)
# 1317 "var/cll/CLLParser.ml"
        ))), _, (s : (
# 59 "var/cll/CLLParser.mly"
      (ARTTree.datas ARTTree.compiler_type)
# 1321 "var/cll/CLLParser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : (
# 59 "var/cll/CLLParser.mly"
      (ARTTree.datas ARTTree.compiler_type)
# 1327 "var/cll/CLLParser.ml"
        ) = let _startpos = _startpos_t_ in
        
# 311 "var/cll/CLLParser.mly"
    (
      if Tagset.mem t cll_variables then
        raise_reserved_variable _startpos t;
      try
        let tag_set = add t s.tag_set in
        let tag = make_node _startpos (t, v) in
        let syntax_tree = Cycle.prepend s.syntax_tree tag in
        {syntax_tree; tag_set}
      with
      | DuplicateElement t ->
        raise_duplicate_element _startpos t 
    )
# 1343 "var/cll/CLLParser.ml"
         in
        _menhir_goto_data_declarations _menhir_env _menhir_stack _menhir_s _v) : 'freshtv452)) : 'freshtv454)
    | MenhirState141 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv457 * _menhir_state * (
# 46 "var/cll/CLLParser.mly"
       (string)
# 1351 "var/cll/CLLParser.ml"
        ) * Lexing.position)) * (
# 44 "var/cll/CLLParser.mly"
       (bool)
# 1355 "var/cll/CLLParser.ml"
        )) * _menhir_state * (
# 59 "var/cll/CLLParser.mly"
      (ARTTree.datas ARTTree.compiler_type)
# 1359 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv455 * _menhir_state * (
# 46 "var/cll/CLLParser.mly"
       (string)
# 1365 "var/cll/CLLParser.ml"
        ) * Lexing.position)) * (
# 44 "var/cll/CLLParser.mly"
       (bool)
# 1369 "var/cll/CLLParser.ml"
        )) * _menhir_state * (
# 59 "var/cll/CLLParser.mly"
      (ARTTree.datas ARTTree.compiler_type)
# 1373 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s, (t : (
# 46 "var/cll/CLLParser.mly"
       (string)
# 1378 "var/cll/CLLParser.ml"
        )), _startpos_t_), (b : (
# 44 "var/cll/CLLParser.mly"
       (bool)
# 1382 "var/cll/CLLParser.ml"
        ))), _, (s : (
# 59 "var/cll/CLLParser.mly"
      (ARTTree.datas ARTTree.compiler_type)
# 1386 "var/cll/CLLParser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : (
# 59 "var/cll/CLLParser.mly"
      (ARTTree.datas ARTTree.compiler_type)
# 1392 "var/cll/CLLParser.ml"
        ) = let _startpos = _startpos_t_ in
        
# 325 "var/cll/CLLParser.mly"
    (
      if Tagset.mem t cll_variables then
        raise_reserved_variable _startpos t;
      try
        let tag_set = add t s.tag_set in
        let tag = make_node _startpos (t, Arith.int_of_bool b) in
        let syntax_tree = Cycle.prepend s.syntax_tree tag in
        {syntax_tree; tag_set}
      with
      | DuplicateElement t ->
        raise_duplicate_element _startpos t 
    )
# 1408 "var/cll/CLLParser.ml"
         in
        _menhir_goto_data_declarations _menhir_env _menhir_stack _menhir_s _v) : 'freshtv456)) : 'freshtv458)
    | MenhirState136 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv465 * _menhir_state * (
# 60 "var/cll/CLLParser.mly"
      ((CLLTree.procedure_definition Cycle.cycle) ARTTree.compiler_type)
# 1416 "var/cll/CLLParser.ml"
        ) * Lexing.position)) * _menhir_state * (
# 59 "var/cll/CLLParser.mly"
      (ARTTree.datas ARTTree.compiler_type)
# 1420 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EOF ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv461 * _menhir_state * (
# 60 "var/cll/CLLParser.mly"
      ((CLLTree.procedure_definition Cycle.cycle) ARTTree.compiler_type)
# 1430 "var/cll/CLLParser.ml"
            ) * Lexing.position)) * _menhir_state * (
# 59 "var/cll/CLLParser.mly"
      (ARTTree.datas ARTTree.compiler_type)
# 1434 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv459 * _menhir_state * (
# 60 "var/cll/CLLParser.mly"
      ((CLLTree.procedure_definition Cycle.cycle) ARTTree.compiler_type)
# 1440 "var/cll/CLLParser.ml"
            ) * Lexing.position)) * _menhir_state * (
# 59 "var/cll/CLLParser.mly"
      (ARTTree.datas ARTTree.compiler_type)
# 1444 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (globals : (
# 60 "var/cll/CLLParser.mly"
      ((CLLTree.procedure_definition Cycle.cycle) ARTTree.compiler_type)
# 1449 "var/cll/CLLParser.ml"
            )), _startpos_globals_), _, (data : (
# 59 "var/cll/CLLParser.mly"
      (ARTTree.datas ARTTree.compiler_type)
# 1453 "var/cll/CLLParser.ml"
            ))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : (
# 50 "var/cll/CLLParser.mly"
      (CLLTree.cll_prog ARTTree.compiler_type)
# 1460 "var/cll/CLLParser.ml"
            ) = let _startpos = _startpos_globals_ in
            
# 74 "var/cll/CLLParser.mly"
    (
      try
        let tag_set = union cll_variables (union globals.tag_set data.tag_set) in
        let syntax_tree = ProcedureDefinitionData(globals.syntax_tree, data.syntax_tree) in
        {tag_set; syntax_tree}
      with
      | DuplicateElement t ->
        raise_duplicate_element _startpos t
    )
# 1473 "var/cll/CLLParser.ml"
             in
            _menhir_goto_program _menhir_env _menhir_stack _menhir_s _v) : 'freshtv460)) : 'freshtv462)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv463 * _menhir_state * (
# 60 "var/cll/CLLParser.mly"
      ((CLLTree.procedure_definition Cycle.cycle) ARTTree.compiler_type)
# 1483 "var/cll/CLLParser.ml"
            ) * Lexing.position)) * _menhir_state * (
# 59 "var/cll/CLLParser.mly"
      (ARTTree.datas ARTTree.compiler_type)
# 1487 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv464)) : 'freshtv466)
    | _ ->
        _menhir_fail ()

and _menhir_run67 : _menhir_env -> 'ttv_tail -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv449) = Obj.magic _menhir_stack in
    ((let _1 = () in
    let _v : 'tv_assign_binop = 
# 241 "var/cll/CLLParser.mly"
              ( SubAssign )
# 1503 "var/cll/CLLParser.ml"
     in
    _menhir_goto_assign_binop _menhir_env _menhir_stack _v) : 'freshtv450)

and _menhir_run68 : _menhir_env -> 'ttv_tail -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv447) = Obj.magic _menhir_stack in
    ((let _1 = () in
    let _v : 'tv_assign_binop = 
# 242 "var/cll/CLLParser.mly"
              ( MultAssign )
# 1516 "var/cll/CLLParser.ml"
     in
    _menhir_goto_assign_binop _menhir_env _menhir_stack _v) : 'freshtv448)

and _menhir_run69 : _menhir_env -> 'ttv_tail -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv445) = Obj.magic _menhir_stack in
    ((let _1 = () in
    let _v : 'tv_assign_unop = 
# 247 "var/cll/CLLParser.mly"
              ( Incr )
# 1529 "var/cll/CLLParser.ml"
     in
    _menhir_goto_assign_unop _menhir_env _menhir_stack _v) : 'freshtv446)

and _menhir_run70 : _menhir_env -> 'ttv_tail -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv443) = Obj.magic _menhir_stack in
    ((let _1 = () in
    let _v : 'tv_assign_binop = 
# 243 "var/cll/CLLParser.mly"
              ( DivAssign )
# 1542 "var/cll/CLLParser.ml"
     in
    _menhir_goto_assign_binop _menhir_env _menhir_stack _v) : 'freshtv444)

and _menhir_run71 : _menhir_env -> 'ttv_tail -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv441) = Obj.magic _menhir_stack in
    ((let _1 = () in
    let _v : 'tv_assign_unop = 
# 248 "var/cll/CLLParser.mly"
              ( Decr )
# 1555 "var/cll/CLLParser.ml"
     in
    _menhir_goto_assign_unop _menhir_env _menhir_stack _v) : 'freshtv442)

and _menhir_run72 : _menhir_env -> 'ttv_tail -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv439) = Obj.magic _menhir_stack in
    ((let _1 = () in
    let _v : 'tv_assign_binop = 
# 239 "var/cll/CLLParser.mly"
              ( Standard )
# 1568 "var/cll/CLLParser.ml"
     in
    _menhir_goto_assign_binop _menhir_env _menhir_stack _v) : 'freshtv440)

and _menhir_run73 : _menhir_env -> 'ttv_tail -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv437) = Obj.magic _menhir_stack in
    ((let _1 = () in
    let _v : 'tv_assign_binop = 
# 240 "var/cll/CLLParser.mly"
              ( AddAssign )
# 1581 "var/cll/CLLParser.ml"
     in
    _menhir_goto_assign_binop _menhir_env _menhir_stack _v) : 'freshtv438)

and _menhir_reduce30 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 53 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1588 "var/cll/CLLParser.ml"
) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (l : (
# 53 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1594 "var/cll/CLLParser.ml"
    )), _startpos_l_) = _menhir_stack in
    let _v : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1599 "var/cll/CLLParser.ml"
    ) = 
# 149 "var/cll/CLLParser.mly"
    ( l )
# 1603 "var/cll/CLLParser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1610 "var/cll/CLLParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState16 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv299 * _menhir_state) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1620 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv297 * _menhir_state) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1626 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _, (e : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1631 "var/cll/CLLParser.ml"
        ))) = _menhir_stack in
        let _1 = () in
        let _v : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1637 "var/cll/CLLParser.ml"
        ) = 
# 181 "var/cll/CLLParser.mly"
    ( Unop(Cpl, e) )
# 1641 "var/cll/CLLParser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv298)) : 'freshtv300)
    | MenhirState13 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv307 * _menhir_state) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1649 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADD ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NEQ ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | RP ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv303 * _menhir_state) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1683 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv301 * _menhir_state) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1690 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (e : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1695 "var/cll/CLLParser.ml"
            ))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1702 "var/cll/CLLParser.ml"
            ) = 
# 151 "var/cll/CLLParser.mly"
    ( e )
# 1706 "var/cll/CLLParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv302)) : 'freshtv304)
        | SUB ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv305 * _menhir_state) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1718 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv306)) : 'freshtv308)
    | MenhirState23 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv313 * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1727 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1731 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIV ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | REM ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | ADD | ADDASSIGN | AND | ASSIGN | COLON | COMMA | DECR | DIVASSIGN | EQ | GE | GT | INCR | LE | LP | LT | MULTASSIGN | NEQ | OR | RP | SEMI | SUB | SUBASSIGN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv309 * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1747 "var/cll/CLLParser.ml"
            ))) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1751 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (e1 : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1756 "var/cll/CLLParser.ml"
            ))), _, (e2 : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1760 "var/cll/CLLParser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1766 "var/cll/CLLParser.ml"
            ) = 
# 155 "var/cll/CLLParser.mly"
    ( Binop(e1, Sub, e2) )
# 1770 "var/cll/CLLParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv310)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv311 * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1780 "var/cll/CLLParser.ml"
            ))) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1784 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv312)) : 'freshtv314)
    | MenhirState25 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv317 * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1793 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1797 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv315 * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1803 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1807 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (e1 : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1812 "var/cll/CLLParser.ml"
        ))), _, (e2 : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1816 "var/cll/CLLParser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1822 "var/cll/CLLParser.ml"
        ) = 
# 161 "var/cll/CLLParser.mly"
    ( Binop(e1, Rem, e2) )
# 1826 "var/cll/CLLParser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv316)) : 'freshtv318)
    | MenhirState27 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv321 * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1834 "var/cll/CLLParser.ml"
        )) * Lexing.position) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1838 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv319 * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1844 "var/cll/CLLParser.ml"
        )) * Lexing.position) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1848 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s, (e1 : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1853 "var/cll/CLLParser.ml"
        ))), _startpos__2_), _, (e2 : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1857 "var/cll/CLLParser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1863 "var/cll/CLLParser.ml"
        ) = 
# 157 "var/cll/CLLParser.mly"
    ( Binop(e1, Mult, e2) )
# 1867 "var/cll/CLLParser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv320)) : 'freshtv322)
    | MenhirState29 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv325 * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1875 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1879 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv323 * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1885 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1889 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (e1 : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1894 "var/cll/CLLParser.ml"
        ))), _, (e2 : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1898 "var/cll/CLLParser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1904 "var/cll/CLLParser.ml"
        ) = 
# 159 "var/cll/CLLParser.mly"
    ( Binop(e1, Div, e2) )
# 1908 "var/cll/CLLParser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv324)) : 'freshtv326)
    | MenhirState32 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv331 * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1916 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1920 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADD ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NEQ ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | ADDASSIGN | AND | ASSIGN | COLON | COMMA | DECR | DIVASSIGN | INCR | LP | MULTASSIGN | OR | RP | SEMI | SUBASSIGN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv327 * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1952 "var/cll/CLLParser.ml"
            ))) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1956 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (e1 : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1961 "var/cll/CLLParser.ml"
            ))), _, (e2 : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1965 "var/cll/CLLParser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1971 "var/cll/CLLParser.ml"
            ) = 
# 165 "var/cll/CLLParser.mly"
    ( Binop(e1, Or, e2) )
# 1975 "var/cll/CLLParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv328)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv329 * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1985 "var/cll/CLLParser.ml"
            ))) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1989 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv330)) : 'freshtv332)
    | MenhirState34 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv337 * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1998 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2002 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADD ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | REM ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | ADDASSIGN | AND | ASSIGN | COLON | COMMA | DECR | DIVASSIGN | EQ | GE | GT | INCR | LE | LP | LT | MULTASSIGN | NEQ | OR | RP | SEMI | SUBASSIGN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv333 * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2022 "var/cll/CLLParser.ml"
            ))) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2026 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (e1 : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2031 "var/cll/CLLParser.ml"
            ))), _, (e2 : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2035 "var/cll/CLLParser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2041 "var/cll/CLLParser.ml"
            ) = 
# 177 "var/cll/CLLParser.mly"
    ( Binop(e1, Neq, e2) )
# 2045 "var/cll/CLLParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv334)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv335 * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2055 "var/cll/CLLParser.ml"
            ))) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2059 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv336)) : 'freshtv338)
    | MenhirState36 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv343 * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2068 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2072 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIV ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | REM ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | ADD | ADDASSIGN | AND | ASSIGN | COLON | COMMA | DECR | DIVASSIGN | EQ | GE | GT | INCR | LE | LP | LT | MULTASSIGN | NEQ | OR | RP | SEMI | SUB | SUBASSIGN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv339 * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2088 "var/cll/CLLParser.ml"
            ))) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2092 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (e1 : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2097 "var/cll/CLLParser.ml"
            ))), _, (e2 : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2101 "var/cll/CLLParser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2107 "var/cll/CLLParser.ml"
            ) = 
# 153 "var/cll/CLLParser.mly"
    ( Binop(e1, Add, e2) )
# 2111 "var/cll/CLLParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv340)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv341 * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2121 "var/cll/CLLParser.ml"
            ))) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2125 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv342)) : 'freshtv344)
    | MenhirState38 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv349 * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2134 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2138 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADD ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | REM ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | ADDASSIGN | AND | ASSIGN | COLON | COMMA | DECR | DIVASSIGN | EQ | GE | GT | INCR | LE | LP | LT | MULTASSIGN | NEQ | OR | RP | SEMI | SUBASSIGN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv345 * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2158 "var/cll/CLLParser.ml"
            ))) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2162 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (e1 : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2167 "var/cll/CLLParser.ml"
            ))), _, (e2 : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2171 "var/cll/CLLParser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2177 "var/cll/CLLParser.ml"
            ) = 
# 167 "var/cll/CLLParser.mly"
    ( Binop(e1, Lt, e2) )
# 2181 "var/cll/CLLParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv346)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv347 * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2191 "var/cll/CLLParser.ml"
            ))) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2195 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv348)) : 'freshtv350)
    | MenhirState40 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv355 * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2204 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2208 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADD ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | REM ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | ADDASSIGN | AND | ASSIGN | COLON | COMMA | DECR | DIVASSIGN | EQ | GE | GT | INCR | LE | LP | LT | MULTASSIGN | NEQ | OR | RP | SEMI | SUBASSIGN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv351 * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2228 "var/cll/CLLParser.ml"
            ))) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2232 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (e1 : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2237 "var/cll/CLLParser.ml"
            ))), _, (e2 : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2241 "var/cll/CLLParser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2247 "var/cll/CLLParser.ml"
            ) = 
# 169 "var/cll/CLLParser.mly"
    ( Binop(e1, Le, e2) )
# 2251 "var/cll/CLLParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv352)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv353 * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2261 "var/cll/CLLParser.ml"
            ))) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2265 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv354)) : 'freshtv356)
    | MenhirState42 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv361 * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2274 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2278 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADD ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | REM ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | ADDASSIGN | AND | ASSIGN | COLON | COMMA | DECR | DIVASSIGN | EQ | GE | GT | INCR | LE | LP | LT | MULTASSIGN | NEQ | OR | RP | SEMI | SUBASSIGN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv357 * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2298 "var/cll/CLLParser.ml"
            ))) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2302 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (e1 : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2307 "var/cll/CLLParser.ml"
            ))), _, (e2 : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2311 "var/cll/CLLParser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2317 "var/cll/CLLParser.ml"
            ) = 
# 171 "var/cll/CLLParser.mly"
    ( Binop(e1, Gt, e2) )
# 2321 "var/cll/CLLParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv358)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv359 * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2331 "var/cll/CLLParser.ml"
            ))) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2335 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv360)) : 'freshtv362)
    | MenhirState44 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv367 * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2344 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2348 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADD ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | REM ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | ADDASSIGN | AND | ASSIGN | COLON | COMMA | DECR | DIVASSIGN | EQ | GE | GT | INCR | LE | LP | LT | MULTASSIGN | NEQ | OR | RP | SEMI | SUBASSIGN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv363 * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2368 "var/cll/CLLParser.ml"
            ))) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2372 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (e1 : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2377 "var/cll/CLLParser.ml"
            ))), _, (e2 : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2381 "var/cll/CLLParser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2387 "var/cll/CLLParser.ml"
            ) = 
# 173 "var/cll/CLLParser.mly"
    ( Binop(e1, Ge, e2) )
# 2391 "var/cll/CLLParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv364)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv365 * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2401 "var/cll/CLLParser.ml"
            ))) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2405 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv366)) : 'freshtv368)
    | MenhirState46 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv373 * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2414 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2418 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADD ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | REM ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | ADDASSIGN | AND | ASSIGN | COLON | COMMA | DECR | DIVASSIGN | EQ | GE | GT | INCR | LE | LP | LT | MULTASSIGN | NEQ | OR | RP | SEMI | SUBASSIGN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv369 * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2438 "var/cll/CLLParser.ml"
            ))) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2442 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (e1 : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2447 "var/cll/CLLParser.ml"
            ))), _, (e2 : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2451 "var/cll/CLLParser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2457 "var/cll/CLLParser.ml"
            ) = 
# 175 "var/cll/CLLParser.mly"
    ( Binop(e1, Eq, e2) )
# 2461 "var/cll/CLLParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv370)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv371 * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2471 "var/cll/CLLParser.ml"
            ))) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2475 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv372)) : 'freshtv374)
    | MenhirState48 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv379 * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2484 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2488 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADD ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NEQ ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | ADDASSIGN | AND | ASSIGN | COLON | COMMA | DECR | DIVASSIGN | INCR | LP | MULTASSIGN | OR | RP | SEMI | SUBASSIGN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv375 * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2520 "var/cll/CLLParser.ml"
            ))) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2524 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (e1 : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2529 "var/cll/CLLParser.ml"
            ))), _, (e2 : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2533 "var/cll/CLLParser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2539 "var/cll/CLLParser.ml"
            ) = 
# 163 "var/cll/CLLParser.mly"
    ( Binop(e1, And, e2) )
# 2543 "var/cll/CLLParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv376)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv377 * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2553 "var/cll/CLLParser.ml"
            ))) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2557 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv378)) : 'freshtv380)
    | MenhirState12 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv383 * _menhir_state * Lexing.position) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2566 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv381 * _menhir_state * Lexing.position) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2572 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _startpos__1_), _, (l : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2577 "var/cll/CLLParser.ml"
        ))) = _menhir_stack in
        let _1 = () in
        let _startpos = _startpos__1_ in
        let _v : (
# 53 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2584 "var/cll/CLLParser.ml"
        ) = 
# 192 "var/cll/CLLParser.mly"
    ( LStar l )
# 2588 "var/cll/CLLParser.ml"
         in
        _menhir_goto_l_expr _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv382)) : 'freshtv384)
    | MenhirState11 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv387 * _menhir_state) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2596 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv385 * _menhir_state) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2602 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _, (e : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2607 "var/cll/CLLParser.ml"
        ))) = _menhir_stack in
        let _1 = () in
        let _v : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2613 "var/cll/CLLParser.ml"
        ) = 
# 183 "var/cll/CLLParser.mly"
    ( Unop(Not, e) )
# 2617 "var/cll/CLLParser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv386)) : 'freshtv388)
    | MenhirState10 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv393 * _menhir_state) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2625 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIV ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | REM ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | ADD | ADDASSIGN | AND | ASSIGN | COLON | COMMA | DECR | DIVASSIGN | EQ | GE | GT | INCR | LE | LP | LT | MULTASSIGN | NEQ | OR | RP | SEMI | SUB | SUBASSIGN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv389 * _menhir_state) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2641 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (e : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2646 "var/cll/CLLParser.ml"
            ))) = _menhir_stack in
            let _1 = () in
            let _v : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2652 "var/cll/CLLParser.ml"
            ) = 
# 179 "var/cll/CLLParser.mly"
    ( Unop(Minus, e) )
# 2656 "var/cll/CLLParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv390)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv391 * _menhir_state) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2666 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv392)) : 'freshtv394)
    | MenhirState9 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv401 * _menhir_state * Lexing.position)) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2675 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADD ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NEQ ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | RP ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv397 * _menhir_state * Lexing.position)) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2709 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv395 * _menhir_state * Lexing.position)) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2716 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, _startpos__1_), _, (e : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2721 "var/cll/CLLParser.ml"
            ))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _v : (
# 54 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 2730 "var/cll/CLLParser.ml"
            ) = 
# 201 "var/cll/CLLParser.mly"
    ( Print e )
# 2734 "var/cll/CLLParser.ml"
             in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv396)) : 'freshtv398)
        | SUB ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv399 * _menhir_state * Lexing.position)) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2746 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv400)) : 'freshtv402)
    | MenhirState56 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv407 * _menhir_state * Lexing.position) * _menhir_state) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2755 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADD ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NEQ ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | RP ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv403 * _menhir_state * Lexing.position) * _menhir_state) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2789 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BREAK ->
                _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | CONTINUE ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EXIT ->
                _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOR ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IF ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LABEL _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState58
            | MULT ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NOP ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | PRINT ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RETURN ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | WHILE ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState58) : 'freshtv404)
        | SUB ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv405 * _menhir_state * Lexing.position) * _menhir_state) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2831 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv406)) : 'freshtv408)
    | MenhirState61 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv413 * _menhir_state * Lexing.position)) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2840 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADD ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NEQ ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | RP ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv409 * _menhir_state * Lexing.position)) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2874 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BREAK ->
                _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | CONTINUE ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EXIT ->
                _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOR ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IF ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LABEL _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState63
            | MULT ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NOP ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | PRINT ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RETURN ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | WHILE ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState63) : 'freshtv410)
        | SUB ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv411 * _menhir_state * Lexing.position)) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2916 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv412)) : 'freshtv414)
    | MenhirState75 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv419 * _menhir_state * (
# 53 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2925 "var/cll/CLLParser.ml"
        ) * Lexing.position) * 'tv_assign_binop) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2929 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADD ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NEQ ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | COLON | COMMA | RP | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv415 * _menhir_state * (
# 53 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2965 "var/cll/CLLParser.ml"
            ) * Lexing.position) * 'tv_assign_binop) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2969 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (l : (
# 53 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2974 "var/cll/CLLParser.ml"
            )), _startpos_l_), (op : 'tv_assign_binop)), _, (e : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2978 "var/cll/CLLParser.ml"
            ))) = _menhir_stack in
            let _startpos = _startpos_l_ in
            let _v : (
# 55 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 2984 "var/cll/CLLParser.ml"
            ) = 
# 228 "var/cll/CLLParser.mly"
    (
      BinopAssign(l, op, e)
    )
# 2990 "var/cll/CLLParser.ml"
             in
            _menhir_goto_assign _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv416)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv417 * _menhir_state * (
# 53 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3000 "var/cll/CLLParser.ml"
            ) * Lexing.position) * 'tv_assign_binop) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3004 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv418)) : 'freshtv420)
    | MenhirState65 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv427 * _menhir_state * Lexing.position)) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3013 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADD ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NEQ ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | RP ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv421 * _menhir_state * Lexing.position)) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3047 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BREAK ->
                _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | CONTINUE ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EXIT ->
                _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOR ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IF ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LABEL _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | MULT ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NOP ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | PRINT ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RETURN ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | WHILE ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState96) : 'freshtv422)
        | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv423 * _menhir_state * Lexing.position)) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3085 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LABEL _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MULT ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState78) : 'freshtv424)
        | SUB ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv425 * _menhir_state * Lexing.position)) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3107 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv426)) : 'freshtv428)
    | MenhirState99 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv435 * _menhir_state * Lexing.position)) * _menhir_state * (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3116 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3120 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADD ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NEQ ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | RP ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv429 * _menhir_state * Lexing.position)) * _menhir_state * (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3154 "var/cll/CLLParser.ml"
            ))) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3158 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BREAK ->
                _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | CONTINUE ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EXIT ->
                _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOR ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IF ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LABEL _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState105
            | MULT ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NOP ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | PRINT ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RETURN ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | WHILE ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState105) : 'freshtv430)
        | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv431 * _menhir_state * Lexing.position)) * _menhir_state * (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3196 "var/cll/CLLParser.ml"
            ))) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3200 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LABEL _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MULT ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState101) : 'freshtv432)
        | SUB ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv433 * _menhir_state * Lexing.position)) * _menhir_state * (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3222 "var/cll/CLLParser.ml"
            ))) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3226 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv434)) : 'freshtv436)
    | _ ->
        _menhir_fail ()

and _menhir_goto_block : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 56 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3236 "var/cll/CLLParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState81 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv255 * _menhir_state * Lexing.position)) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3246 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3250 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 56 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3254 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv253 * _menhir_state * Lexing.position)) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3260 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3264 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 56 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3268 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((((_menhir_stack, _menhir_s, _startpos__1_), _, (_3 : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3273 "var/cll/CLLParser.ml"
        ))), _, (_5 : (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3277 "var/cll/CLLParser.ml"
        ))), _, (_7 : (
# 56 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3281 "var/cll/CLLParser.ml"
        ))) = _menhir_stack in
        let _6 = () in
        let _4 = () in
        let _2 = () in
        let _1 = () in
        let _startpos = _startpos__1_ in
        let _v : (
# 57 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3291 "var/cll/CLLParser.ml"
        ) = let _startpos = _startpos__1_ in
        
# 294 "var/cll/CLLParser.mly"
    (
      raise_syntax_error _startpos "Ill-formed 'for' loop"
    )
# 3298 "var/cll/CLLParser.ml"
         in
        _menhir_goto_control _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv254)) : 'freshtv256)
    | MenhirState96 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv259 * _menhir_state * Lexing.position)) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3306 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 56 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3310 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv257 * _menhir_state * Lexing.position)) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3316 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 56 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3320 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s, _startpos__1_), _, (_3 : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3325 "var/cll/CLLParser.ml"
        ))), _, (_5 : (
# 56 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3329 "var/cll/CLLParser.ml"
        ))) = _menhir_stack in
        let _4 = () in
        let _2 = () in
        let _1 = () in
        let _startpos = _startpos__1_ in
        let _v : (
# 57 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3338 "var/cll/CLLParser.ml"
        ) = let _startpos = _startpos__1_ in
        
# 299 "var/cll/CLLParser.mly"
    (
      raise_syntax_error _startpos "Ill-formed 'for' loop; You may want to use a 'while' loop instead."
    )
# 3345 "var/cll/CLLParser.ml"
         in
        _menhir_goto_control _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv258)) : 'freshtv260)
    | MenhirState103 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((((('freshtv263 * _menhir_state * Lexing.position)) * _menhir_state * (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3353 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3357 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3361 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 56 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3365 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((((('freshtv261 * _menhir_state * Lexing.position)) * _menhir_state * (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3371 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3375 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3379 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 56 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3383 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (((((_menhir_stack, _menhir_s, _startpos__1_), _, (init : (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3388 "var/cll/CLLParser.ml"
        ))), _, (cond : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3392 "var/cll/CLLParser.ml"
        ))), _, (it : (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3396 "var/cll/CLLParser.ml"
        ))), _, (b : (
# 56 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3400 "var/cll/CLLParser.ml"
        ))) = _menhir_stack in
        let _8 = () in
        let _6 = () in
        let _4 = () in
        let _2 = () in
        let _1 = () in
        let _startpos = _startpos__1_ in
        let _v : (
# 57 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3411 "var/cll/CLLParser.ml"
        ) = 
# 281 "var/cll/CLLParser.mly"
    (
      Cycle.from_elt (For(init, cond, it, b))
    )
# 3417 "var/cll/CLLParser.ml"
         in
        _menhir_goto_control _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv262)) : 'freshtv264)
    | MenhirState105 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv267 * _menhir_state * Lexing.position)) * _menhir_state * (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3425 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3429 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 56 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3433 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv265 * _menhir_state * Lexing.position)) * _menhir_state * (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3439 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3443 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 56 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3447 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((((_menhir_stack, _menhir_s, _startpos__1_), _, (_3 : (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3452 "var/cll/CLLParser.ml"
        ))), _, (_5 : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3456 "var/cll/CLLParser.ml"
        ))), _, (_7 : (
# 56 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3460 "var/cll/CLLParser.ml"
        ))) = _menhir_stack in
        let _6 = () in
        let _4 = () in
        let _2 = () in
        let _1 = () in
        let _startpos = _startpos__1_ in
        let _v : (
# 57 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3470 "var/cll/CLLParser.ml"
        ) = let _startpos = _startpos__1_ in
        
# 294 "var/cll/CLLParser.mly"
    (
      raise_syntax_error _startpos "Ill-formed 'for' loop"
    )
# 3477 "var/cll/CLLParser.ml"
         in
        _menhir_goto_control _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv266)) : 'freshtv268)
    | MenhirState108 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv271 * _menhir_state * Lexing.position)) * _menhir_state * (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3485 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3489 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 56 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3493 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv269 * _menhir_state * Lexing.position)) * _menhir_state * (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3499 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3503 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 56 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3507 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((((_menhir_stack, _menhir_s, _startpos__1_), _, (_3 : (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3512 "var/cll/CLLParser.ml"
        ))), _, (_5 : (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3516 "var/cll/CLLParser.ml"
        ))), _, (_7 : (
# 56 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3520 "var/cll/CLLParser.ml"
        ))) = _menhir_stack in
        let _6 = () in
        let _4 = () in
        let _2 = () in
        let _1 = () in
        let _startpos = _startpos__1_ in
        let _v : (
# 57 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3530 "var/cll/CLLParser.ml"
        ) = let _startpos = _startpos__1_ in
        
# 294 "var/cll/CLLParser.mly"
    (
      raise_syntax_error _startpos "Ill-formed 'for' loop"
    )
# 3537 "var/cll/CLLParser.ml"
         in
        _menhir_goto_control _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv270)) : 'freshtv272)
    | MenhirState110 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv275 * _menhir_state * Lexing.position)) * _menhir_state * (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3545 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 56 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3549 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv273 * _menhir_state * Lexing.position)) * _menhir_state * (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3555 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 56 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3559 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s, _startpos__1_), _, (_3 : (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3564 "var/cll/CLLParser.ml"
        ))), _, (_5 : (
# 56 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3568 "var/cll/CLLParser.ml"
        ))) = _menhir_stack in
        let _4 = () in
        let _2 = () in
        let _1 = () in
        let _startpos = _startpos__1_ in
        let _v : (
# 57 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3577 "var/cll/CLLParser.ml"
        ) = let _startpos = _startpos__1_ in
        
# 294 "var/cll/CLLParser.mly"
    (
      raise_syntax_error _startpos "Ill-formed 'for' loop"
    )
# 3584 "var/cll/CLLParser.ml"
         in
        _menhir_goto_control _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv274)) : 'freshtv276)
    | MenhirState63 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv283 * _menhir_state * Lexing.position)) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3592 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 56 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3596 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ELSE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv277 * _menhir_state * Lexing.position)) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3606 "var/cll/CLLParser.ml"
            ))) * _menhir_state * (
# 56 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3610 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BREAK ->
                _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | CONTINUE ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EXIT ->
                _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOR ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IF ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LABEL _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState113
            | MULT ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NOP ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | PRINT ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RETURN ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | WHILE ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState113) : 'freshtv278)
        | BREAK | COLON | CONTINUE | EXIT | FOR | IF | LABEL _ | MULT | NOP | PRINT | RB | RETURN | SEMI | WHILE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv279 * _menhir_state * Lexing.position)) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3648 "var/cll/CLLParser.ml"
            ))) * _menhir_state * (
# 56 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3652 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, _startpos__1_), _, (e : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3657 "var/cll/CLLParser.ml"
            ))), _, (b : (
# 56 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3661 "var/cll/CLLParser.ml"
            ))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _v : (
# 57 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3670 "var/cll/CLLParser.ml"
            ) = 
# 266 "var/cll/CLLParser.mly"
    (
      Cycle.from_elt (CLLTree.If(e, b))
    )
# 3676 "var/cll/CLLParser.ml"
             in
            _menhir_goto_control _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv280)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv281 * _menhir_state * Lexing.position)) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3686 "var/cll/CLLParser.ml"
            ))) * _menhir_state * (
# 56 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3690 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv282)) : 'freshtv284)
    | MenhirState113 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv287 * _menhir_state * Lexing.position)) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3699 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 56 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3703 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 56 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3707 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv285 * _menhir_state * Lexing.position)) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3713 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 56 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3717 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 56 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3721 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((((_menhir_stack, _menhir_s, _startpos__1_), _, (c : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3726 "var/cll/CLLParser.ml"
        ))), _, (t : (
# 56 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3730 "var/cll/CLLParser.ml"
        ))), _, (e : (
# 56 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3734 "var/cll/CLLParser.ml"
        ))) = _menhir_stack in
        let _6 = () in
        let _4 = () in
        let _2 = () in
        let _1 = () in
        let _startpos = _startpos__1_ in
        let _v : (
# 57 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3744 "var/cll/CLLParser.ml"
        ) = 
# 271 "var/cll/CLLParser.mly"
    ( 
      Cycle.from_elt (CLLTree.IfElse(c, t, e))
    )
# 3750 "var/cll/CLLParser.ml"
         in
        _menhir_goto_control _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv286)) : 'freshtv288)
    | MenhirState58 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv291 * _menhir_state * Lexing.position) * _menhir_state) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3758 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 56 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3762 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv289 * _menhir_state * Lexing.position) * _menhir_state) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3768 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 56 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3772 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((((_menhir_stack, _menhir_s, _startpos__1_), _), _, (e : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3777 "var/cll/CLLParser.ml"
        ))), _, (b : (
# 56 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3781 "var/cll/CLLParser.ml"
        ))) = _menhir_stack in
        let _4 = () in
        let _2 = () in
        let _1 = () in
        let _startpos = _startpos__1_ in
        let _v : (
# 57 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3790 "var/cll/CLLParser.ml"
        ) = 
# 276 "var/cll/CLLParser.mly"
    (
      Cycle.from_elt (CLLTree.While(e, b))
    )
# 3796 "var/cll/CLLParser.ml"
         in
        _menhir_goto_control _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv290)) : 'freshtv292)
    | MenhirState6 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv295 * _menhir_state * Lexing.position) * _menhir_state * (
# 56 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3804 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv293 * _menhir_state * Lexing.position) * _menhir_state * (
# 56 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3810 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _startpos__1_), _, (_2 : (
# 56 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3815 "var/cll/CLLParser.ml"
        ))) = _menhir_stack in
        let _1 = () in
        let _startpos = _startpos__1_ in
        let _v : (
# 57 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3822 "var/cll/CLLParser.ml"
        ) = let _startpos = _startpos__1_ in
        
# 286 "var/cll/CLLParser.mly"
    (
      raise_syntax_error _startpos "No condition found for 'while'"
    )
# 3829 "var/cll/CLLParser.ml"
         in
        _menhir_goto_control _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv294)) : 'freshtv296)
    | _ ->
        _menhir_fail ()

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_reduce27 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (
# 59 "var/cll/CLLParser.mly"
      (ARTTree.datas ARTTree.compiler_type)
# 3845 "var/cll/CLLParser.ml"
    ) = 
# 338 "var/cll/CLLParser.mly"
    ( {syntax_tree = Cycle.empty_cycle; tag_set = empty} )
# 3849 "var/cll/CLLParser.ml"
     in
    _menhir_goto_data_declarations _menhir_env _menhir_stack _menhir_s _v

and _menhir_run137 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 46 "var/cll/CLLParser.mly"
       (string)
# 3856 "var/cll/CLLParser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COLON ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv249 * _menhir_state * (
# 46 "var/cll/CLLParser.mly"
       (string)
# 3868 "var/cll/CLLParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BOOL _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv243 * _menhir_state * (
# 46 "var/cll/CLLParser.mly"
       (string)
# 3878 "var/cll/CLLParser.ml"
            ) * Lexing.position)) = Obj.magic _menhir_stack in
            let (_v : (
# 44 "var/cll/CLLParser.mly"
       (bool)
# 3883 "var/cll/CLLParser.ml"
            )) = _v in
            ((let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LABEL _v ->
                _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState141 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EOF ->
                _menhir_reduce27 _menhir_env (Obj.magic _menhir_stack) MenhirState141
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState141) : 'freshtv244)
        | INT _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv245 * _menhir_state * (
# 46 "var/cll/CLLParser.mly"
       (string)
# 3902 "var/cll/CLLParser.ml"
            ) * Lexing.position)) = Obj.magic _menhir_stack in
            let (_v : (
# 45 "var/cll/CLLParser.mly"
       (int)
# 3907 "var/cll/CLLParser.ml"
            )) = _v in
            ((let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LABEL _v ->
                _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState139 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EOF ->
                _menhir_reduce27 _menhir_env (Obj.magic _menhir_stack) MenhirState139
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState139) : 'freshtv246)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv247 * _menhir_state * (
# 46 "var/cll/CLLParser.mly"
       (string)
# 3928 "var/cll/CLLParser.ml"
            ) * Lexing.position)) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv248)) : 'freshtv250)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv251 * _menhir_state * (
# 46 "var/cll/CLLParser.mly"
       (string)
# 3939 "var/cll/CLLParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv252)

and _menhir_goto_instructions : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 51 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3947 "var/cll/CLLParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState59 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv215 * _menhir_state) * _menhir_state * (
# 51 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3957 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RB ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv211 * _menhir_state) * _menhir_state * (
# 51 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3967 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv209 * _menhir_state) * _menhir_state * (
# 51 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3975 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            let (_endpos__3_ : Lexing.position) = _endpos in
            ((let ((_menhir_stack, _menhir_s), _, (i : (
# 51 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3981 "var/cll/CLLParser.ml"
            ))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (
# 56 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3988 "var/cll/CLLParser.ml"
            ) = 
# 261 "var/cll/CLLParser.mly"
    ( i )
# 3992 "var/cll/CLLParser.ml"
             in
            _menhir_goto_block _menhir_env _menhir_stack _menhir_s _v) : 'freshtv210)) : 'freshtv212)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv213 * _menhir_state) * _menhir_state * (
# 51 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 4002 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv214)) : 'freshtv216)
    | MenhirState118 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv219 * _menhir_state * (
# 54 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 4011 "var/cll/CLLParser.ml"
        ) * Lexing.position)) * _menhir_state * (
# 51 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 4015 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv217 * _menhir_state * (
# 54 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 4021 "var/cll/CLLParser.ml"
        ) * Lexing.position)) * _menhir_state * (
# 51 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 4025 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (i : (
# 54 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 4030 "var/cll/CLLParser.ml"
        )), _startpos_i_), _, (s : (
# 51 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 4034 "var/cll/CLLParser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : (
# 51 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 4040 "var/cll/CLLParser.ml"
        ) = 
# 124 "var/cll/CLLParser.mly"
    (
      Cycle.prepend s i
    )
# 4046 "var/cll/CLLParser.ml"
         in
        _menhir_goto_instructions _menhir_env _menhir_stack _menhir_s _v) : 'freshtv218)) : 'freshtv220)
    | MenhirState121 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv223 * _menhir_state * (
# 57 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 4054 "var/cll/CLLParser.ml"
        ) * Lexing.position) * _menhir_state) * _menhir_state * (
# 51 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 4058 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv221 * _menhir_state * (
# 57 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 4064 "var/cll/CLLParser.ml"
        ) * Lexing.position) * _menhir_state) * _menhir_state * (
# 51 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 4068 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s, (c : (
# 57 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 4073 "var/cll/CLLParser.ml"
        )), _startpos_c_), _), _, (s : (
# 51 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 4077 "var/cll/CLLParser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : (
# 51 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 4083 "var/cll/CLLParser.ml"
        ) = 
# 130 "var/cll/CLLParser.mly"
    (
      Cycle.extend c s
    )
# 4089 "var/cll/CLLParser.ml"
         in
        _menhir_goto_instructions _menhir_env _menhir_stack _menhir_s _v) : 'freshtv222)) : 'freshtv224)
    | MenhirState123 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv227 * _menhir_state * (
# 57 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 4097 "var/cll/CLLParser.ml"
        ) * Lexing.position) * _menhir_state) * _menhir_state * (
# 51 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 4101 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv225 * _menhir_state * (
# 57 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 4107 "var/cll/CLLParser.ml"
        ) * Lexing.position) * _menhir_state) * _menhir_state * (
# 51 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 4111 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s, (_1 : (
# 57 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 4116 "var/cll/CLLParser.ml"
        )), _startpos__1_), _), _, (_3 : (
# 51 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 4120 "var/cll/CLLParser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : (
# 51 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 4126 "var/cll/CLLParser.ml"
        ) = let _startpos = _startpos__1_ in
        
# 136 "var/cll/CLLParser.mly"
    (
      raise_syntax_error _startpos "Expected ';', found ':'."
    )
# 4133 "var/cll/CLLParser.ml"
         in
        _menhir_goto_instructions _menhir_env _menhir_stack _menhir_s _v) : 'freshtv226)) : 'freshtv228)
    | MenhirState120 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv231 * _menhir_state * (
# 57 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 4141 "var/cll/CLLParser.ml"
        ) * Lexing.position) * _menhir_state * (
# 51 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 4145 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv229 * _menhir_state * (
# 57 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 4151 "var/cll/CLLParser.ml"
        ) * Lexing.position) * _menhir_state * (
# 51 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 4155 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (c : (
# 57 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 4160 "var/cll/CLLParser.ml"
        )), _startpos_c_), _, (s : (
# 51 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 4164 "var/cll/CLLParser.ml"
        ))) = _menhir_stack in
        let _v : (
# 51 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 4169 "var/cll/CLLParser.ml"
        ) = 
# 130 "var/cll/CLLParser.mly"
    (
      Cycle.extend c s
    )
# 4175 "var/cll/CLLParser.ml"
         in
        _menhir_goto_instructions _menhir_env _menhir_stack _menhir_s _v) : 'freshtv230)) : 'freshtv232)
    | MenhirState126 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv235 * _menhir_state * (
# 54 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 4183 "var/cll/CLLParser.ml"
        ) * Lexing.position)) * _menhir_state * (
# 51 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 4187 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv233 * _menhir_state * (
# 54 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 4193 "var/cll/CLLParser.ml"
        ) * Lexing.position)) * _menhir_state * (
# 51 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 4197 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (_1 : (
# 54 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 4202 "var/cll/CLLParser.ml"
        )), _startpos__1_), _, (_3 : (
# 51 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 4206 "var/cll/CLLParser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : (
# 51 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 4212 "var/cll/CLLParser.ml"
        ) = let _startpos = _startpos__1_ in
        
# 136 "var/cll/CLLParser.mly"
    (
      raise_syntax_error _startpos "Expected ';', found ':'."
    )
# 4219 "var/cll/CLLParser.ml"
         in
        _menhir_goto_instructions _menhir_env _menhir_stack _menhir_s _v) : 'freshtv234)) : 'freshtv236)
    | MenhirState5 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv241 * _menhir_state * (
# 46 "var/cll/CLLParser.mly"
       (string)
# 4227 "var/cll/CLLParser.ml"
        ) * Lexing.position)))) * _menhir_state * (
# 51 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 4231 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RB ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv237 * _menhir_state * (
# 46 "var/cll/CLLParser.mly"
       (string)
# 4241 "var/cll/CLLParser.ml"
            ) * Lexing.position)))) * _menhir_state * (
# 51 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 4245 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_stack = (_menhir_stack, _endpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LABEL _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState131 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | DATA | EOF ->
                _menhir_reduce66 _menhir_env (Obj.magic _menhir_stack) MenhirState131
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState131) : 'freshtv238)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv239 * _menhir_state * (
# 46 "var/cll/CLLParser.mly"
       (string)
# 4267 "var/cll/CLLParser.ml"
            ) * Lexing.position)))) * _menhir_state * (
# 51 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 4271 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv240)) : 'freshtv242)
    | _ ->
        _menhir_fail ()

and _menhir_run59 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BREAK ->
        _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CONTINUE ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | EXIT ->
        _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FOR ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IF ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LABEL _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MULT ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOP ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | PRINT ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RETURN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | WHILE ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RB ->
        _menhir_reduce62 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState59

and _menhir_goto_l_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 53 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 4316 "var/cll/CLLParser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState75 | MenhirState61 | MenhirState56 | MenhirState9 | MenhirState10 | MenhirState11 | MenhirState12 | MenhirState48 | MenhirState46 | MenhirState44 | MenhirState42 | MenhirState40 | MenhirState38 | MenhirState36 | MenhirState34 | MenhirState32 | MenhirState29 | MenhirState27 | MenhirState25 | MenhirState23 | MenhirState13 | MenhirState16 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv187 * _menhir_state * (
# 53 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 4326 "var/cll/CLLParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        (_menhir_reduce30 _menhir_env (Obj.magic _menhir_stack) : 'freshtv188)
    | MenhirState99 | MenhirState65 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv191 * _menhir_state * (
# 53 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 4334 "var/cll/CLLParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADDASSIGN ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | DECR ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | DIVASSIGN ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | MULTASSIGN ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | SUBASSIGN ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | ADD | AND | DIV | EQ | GE | GT | LE | LT | MULT | NEQ | OR | REM | RP | SEMI | SUB ->
            _menhir_reduce30 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv189 * _menhir_state * (
# 53 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 4362 "var/cll/CLLParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv190)) : 'freshtv192)
    | MenhirState101 | MenhirState94 | MenhirState78 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv195 * _menhir_state * (
# 53 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 4371 "var/cll/CLLParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADDASSIGN ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | DECR ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | DIVASSIGN ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | MULTASSIGN ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | SUBASSIGN ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv193 * _menhir_state * (
# 53 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 4397 "var/cll/CLLParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv194)) : 'freshtv196)
    | MenhirState5 | MenhirState6 | MenhirState58 | MenhirState126 | MenhirState120 | MenhirState123 | MenhirState121 | MenhirState118 | MenhirState59 | MenhirState113 | MenhirState63 | MenhirState110 | MenhirState108 | MenhirState105 | MenhirState103 | MenhirState96 | MenhirState81 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv207 * _menhir_state * (
# 53 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 4406 "var/cll/CLLParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADDASSIGN ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | DECR ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | DIVASSIGN ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | LP ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv203 * _menhir_state * (
# 53 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 4426 "var/cll/CLLParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | RP ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv199 * _menhir_state * (
# 53 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 4436 "var/cll/CLLParser.ml"
                ) * Lexing.position)) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv197 * _menhir_state * (
# 53 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 4443 "var/cll/CLLParser.ml"
                ) * Lexing.position)) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, (l_e : (
# 53 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 4448 "var/cll/CLLParser.ml"
                )), _startpos_l_e_) = _menhir_stack in
                let _3 = () in
                let _2 = () in
                let _startpos = _startpos_l_e_ in
                let _v : (
# 54 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 4456 "var/cll/CLLParser.ml"
                ) = 
# 218 "var/cll/CLLParser.mly"
    (
      Call l_e
    )
# 4462 "var/cll/CLLParser.ml"
                 in
                _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv198)) : 'freshtv200)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv201 * _menhir_state * (
# 53 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 4472 "var/cll/CLLParser.ml"
                ) * Lexing.position)) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv202)) : 'freshtv204)
        | MULTASSIGN ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | SUBASSIGN ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv205 * _menhir_state * (
# 53 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 4487 "var/cll/CLLParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv206)) : 'freshtv208)
    | _ ->
        _menhir_fail ()

and _menhir_run10 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BOOL _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _v
    | CPL ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | INT _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _v
    | LABEL _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | MULT ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | SUB ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState10

and _menhir_run11 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BOOL _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v
    | CPL ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | INT _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v
    | LABEL _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | MULT ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | SUB ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState11

and _menhir_run13 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BOOL _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v
    | CPL ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | INT _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v
    | LABEL _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | MULT ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | SUB ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState13

and _menhir_run15 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 45 "var/cll/CLLParser.mly"
       (int)
# 4584 "var/cll/CLLParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv185) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((i : (
# 45 "var/cll/CLLParser.mly"
       (int)
# 4594 "var/cll/CLLParser.ml"
    )) : (
# 45 "var/cll/CLLParser.mly"
       (int)
# 4598 "var/cll/CLLParser.ml"
    )) = _v in
    ((let _v : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 4603 "var/cll/CLLParser.ml"
    ) = 
# 145 "var/cll/CLLParser.mly"
    ( Int i )
# 4607 "var/cll/CLLParser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv186)

and _menhir_run16 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BOOL _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
    | CPL ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | INT _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
    | LABEL _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | MULT ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | SUB ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState16

and _menhir_run17 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 44 "var/cll/CLLParser.mly"
       (bool)
# 4643 "var/cll/CLLParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv183) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((b : (
# 44 "var/cll/CLLParser.mly"
       (bool)
# 4653 "var/cll/CLLParser.ml"
    )) : (
# 44 "var/cll/CLLParser.mly"
       (bool)
# 4657 "var/cll/CLLParser.ml"
    )) = _v in
    ((let _v : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 4662 "var/cll/CLLParser.ml"
    ) = 
# 147 "var/cll/CLLParser.mly"
    ( Bool b )
# 4666 "var/cll/CLLParser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv184)

and _menhir_run18 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LABEL _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv179 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        let (_v : (
# 46 "var/cll/CLLParser.mly"
       (string)
# 4682 "var/cll/CLLParser.ml"
        )) = _v in
        let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv177 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        let ((t : (
# 46 "var/cll/CLLParser.mly"
       (string)
# 4691 "var/cll/CLLParser.ml"
        )) : (
# 46 "var/cll/CLLParser.mly"
       (string)
# 4695 "var/cll/CLLParser.ml"
        )) = _v in
        let (_startpos_t_ : Lexing.position) = _startpos in
        ((let (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
        let _1 = () in
        let _v : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 4703 "var/cll/CLLParser.ml"
        ) = let _startpos = _startpos__1_ in
        
# 185 "var/cll/CLLParser.mly"
    ( Address (make_node _startpos t) )
# 4708 "var/cll/CLLParser.ml"
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
# 54 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 4722 "var/cll/CLLParser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState6 | MenhirState58 | MenhirState113 | MenhirState63 | MenhirState110 | MenhirState108 | MenhirState105 | MenhirState103 | MenhirState96 | MenhirState81 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv167 * _menhir_state * (
# 54 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 4732 "var/cll/CLLParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv163 * _menhir_state * (
# 54 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 4742 "var/cll/CLLParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv161 * _menhir_state * (
# 54 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 4749 "var/cll/CLLParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (i : (
# 54 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 4754 "var/cll/CLLParser.ml"
            )), _startpos_i_) = _menhir_stack in
            let _2 = () in
            let _v : (
# 56 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 4760 "var/cll/CLLParser.ml"
            ) = 
# 253 "var/cll/CLLParser.mly"
    (
      Cycle.from_elt i
    )
# 4766 "var/cll/CLLParser.ml"
             in
            _menhir_goto_block _menhir_env _menhir_stack _menhir_s _v) : 'freshtv162)) : 'freshtv164)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv165 * _menhir_state * (
# 54 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 4776 "var/cll/CLLParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv166)) : 'freshtv168)
    | MenhirState5 | MenhirState126 | MenhirState120 | MenhirState123 | MenhirState121 | MenhirState118 | MenhirState59 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv175 * _menhir_state * (
# 54 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 4785 "var/cll/CLLParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv169 * _menhir_state * (
# 54 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 4795 "var/cll/CLLParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BREAK ->
                _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | CONTINUE ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EXIT ->
                _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOR ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IF ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LABEL _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MULT ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NOP ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | PRINT ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RETURN ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | WHILE ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RB ->
                _menhir_reduce62 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState126) : 'freshtv170)
        | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv171 * _menhir_state * (
# 54 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 4833 "var/cll/CLLParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BREAK ->
                _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | CONTINUE ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EXIT ->
                _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOR ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IF ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LABEL _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MULT ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NOP ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | PRINT ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RETURN ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | WHILE ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RB ->
                _menhir_reduce62 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState118) : 'freshtv172)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv173 * _menhir_state * (
# 54 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 4873 "var/cll/CLLParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv174)) : 'freshtv176)
    | _ ->
        _menhir_fail ()

and _menhir_goto_program : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 50 "var/cll/CLLParser.mly"
      (CLLTree.cll_prog ARTTree.compiler_type)
# 4883 "var/cll/CLLParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv159) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : (
# 50 "var/cll/CLLParser.mly"
      (CLLTree.cll_prog ARTTree.compiler_type)
# 4892 "var/cll/CLLParser.ml"
    )) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv157) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : (
# 50 "var/cll/CLLParser.mly"
      (CLLTree.cll_prog ARTTree.compiler_type)
# 4900 "var/cll/CLLParser.ml"
    )) : (
# 50 "var/cll/CLLParser.mly"
      (CLLTree.cll_prog ARTTree.compiler_type)
# 4904 "var/cll/CLLParser.ml"
    )) = _v in
    (Obj.magic _1 : 'freshtv158)) : 'freshtv160)

and _menhir_goto_procedure_definitions : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 60 "var/cll/CLLParser.mly"
      ((CLLTree.procedure_definition Cycle.cycle) ARTTree.compiler_type)
# 4911 "var/cll/CLLParser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState131 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv145 * _menhir_state * (
# 46 "var/cll/CLLParser.mly"
       (string)
# 4921 "var/cll/CLLParser.ml"
        ) * Lexing.position)))) * _menhir_state * (
# 51 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 4925 "var/cll/CLLParser.ml"
        )) * Lexing.position) * _menhir_state * (
# 60 "var/cll/CLLParser.mly"
      ((CLLTree.procedure_definition Cycle.cycle) ARTTree.compiler_type)
# 4929 "var/cll/CLLParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv143 * _menhir_state * (
# 46 "var/cll/CLLParser.mly"
       (string)
# 4935 "var/cll/CLLParser.ml"
        ) * Lexing.position)))) * _menhir_state * (
# 51 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 4939 "var/cll/CLLParser.ml"
        )) * Lexing.position) * _menhir_state * (
# 60 "var/cll/CLLParser.mly"
      ((CLLTree.procedure_definition Cycle.cycle) ARTTree.compiler_type)
# 4943 "var/cll/CLLParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((((_menhir_stack, _menhir_s, (name : (
# 46 "var/cll/CLLParser.mly"
       (string)
# 4948 "var/cll/CLLParser.ml"
        )), _startpos_name_), _, (b : (
# 51 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 4952 "var/cll/CLLParser.ml"
        ))), _endpos__6_), _, (s : (
# 60 "var/cll/CLLParser.mly"
      ((CLLTree.procedure_definition Cycle.cycle) ARTTree.compiler_type)
# 4956 "var/cll/CLLParser.ml"
        )), _startpos_s_) = _menhir_stack in
        let _6 = () in
        let _4 = () in
        let _3 = () in
        let _2 = () in
        let _startpos = _startpos_name_ in
        let _v : (
# 60 "var/cll/CLLParser.mly"
      ((CLLTree.procedure_definition Cycle.cycle) ARTTree.compiler_type)
# 4966 "var/cll/CLLParser.ml"
        ) = let _startpos = _startpos_name_ in
        
# 104 "var/cll/CLLParser.mly"
  (
    if Tagset.mem name cll_variables then
      raise_reserved_variable _startpos name;
    let name_node = make_node _startpos name in
    let decl = {name = name_node; block = b} in
    let syntax_tree = Cycle.prepend s.syntax_tree decl in
    try
      let tag_set = Tagset.add name s.tag_set in
      {syntax_tree; tag_set}
    with DuplicateElement t ->
      raise_duplicate_element _startpos t
  )
# 4982 "var/cll/CLLParser.ml"
         in
        _menhir_goto_procedure_definitions _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv144)) : 'freshtv146)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv155 * _menhir_state * (
# 60 "var/cll/CLLParser.mly"
      ((CLLTree.procedure_definition Cycle.cycle) ARTTree.compiler_type)
# 4990 "var/cll/CLLParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DATA ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv147 * _menhir_state * (
# 60 "var/cll/CLLParser.mly"
      ((CLLTree.procedure_definition Cycle.cycle) ARTTree.compiler_type)
# 5000 "var/cll/CLLParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LABEL _v ->
                _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EOF ->
                _menhir_reduce27 _menhir_env (Obj.magic _menhir_stack) MenhirState136
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState136) : 'freshtv148)
        | EOF ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv151 * _menhir_state * (
# 60 "var/cll/CLLParser.mly"
      ((CLLTree.procedure_definition Cycle.cycle) ARTTree.compiler_type)
# 5018 "var/cll/CLLParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv149 * _menhir_state * (
# 60 "var/cll/CLLParser.mly"
      ((CLLTree.procedure_definition Cycle.cycle) ARTTree.compiler_type)
# 5024 "var/cll/CLLParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (globals : (
# 60 "var/cll/CLLParser.mly"
      ((CLLTree.procedure_definition Cycle.cycle) ARTTree.compiler_type)
# 5029 "var/cll/CLLParser.ml"
            )), _startpos_globals_) = _menhir_stack in
            let _2 = () in
            let _v : (
# 50 "var/cll/CLLParser.mly"
      (CLLTree.cll_prog ARTTree.compiler_type)
# 5035 "var/cll/CLLParser.ml"
            ) = let _startpos = _startpos_globals_ in
            
# 85 "var/cll/CLLParser.mly"
    (
      try
        let tag_set = union cll_variables globals.tag_set in
        let syntax_tree = ProcedureDefinition globals.syntax_tree in
        {tag_set; syntax_tree}
      with
      | DuplicateElement t ->
        raise_duplicate_element _startpos t
    )
# 5048 "var/cll/CLLParser.ml"
             in
            _menhir_goto_program _menhir_env _menhir_stack _menhir_s _v) : 'freshtv150)) : 'freshtv152)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv153 * _menhir_state * (
# 60 "var/cll/CLLParser.mly"
      ((CLLTree.procedure_definition Cycle.cycle) ARTTree.compiler_type)
# 5058 "var/cll/CLLParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv154)) : 'freshtv156)
    | _ ->
        _menhir_fail ()

and _menhir_reduce62 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (
# 51 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 5070 "var/cll/CLLParser.ml"
    ) = 
# 140 "var/cll/CLLParser.mly"
    ( Cycle.empty_cycle )
# 5074 "var/cll/CLLParser.ml"
     in
    _menhir_goto_instructions _menhir_env _menhir_stack _menhir_s _v

and _menhir_run6 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BREAK ->
        _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CONTINUE ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | EXIT ->
        _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FOR ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IF ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LABEL _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | LP ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv141 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState6 in
        ((let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADDRESS ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | BOOL _v ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
        | CPL ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | INT _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
        | LABEL _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LP ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | MULT ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NOT ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | SUB ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState56) : 'freshtv142)
    | MULT ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOP ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | PRINT ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RETURN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | WHILE ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState6

and _menhir_run7 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv139) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_startpos__1_ : Lexing.position) = _startpos in
    ((let _1 = () in
    let _startpos = _startpos__1_ in
    let _v : (
# 54 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 5155 "var/cll/CLLParser.ml"
    ) = 
# 213 "var/cll/CLLParser.mly"
    (
      Return
    )
# 5161 "var/cll/CLLParser.ml"
     in
    _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv140)

and _menhir_run8 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
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
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | BOOL _v ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
        | CPL ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState9
        | INT _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
        | LABEL _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LP ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState9
        | MULT ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NOT ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState9
        | SUB ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState9
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState9) : 'freshtv136)
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
# 54 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 5219 "var/cll/CLLParser.ml"
    ) = 
# 197 "var/cll/CLLParser.mly"
    ( Nop )
# 5223 "var/cll/CLLParser.ml"
     in
    _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv134)

and _menhir_run12 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BOOL _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
    | CPL ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | INT _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
    | LABEL _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | MULT ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | SUB ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState12

and _menhir_run14 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 46 "var/cll/CLLParser.mly"
       (string)
# 5259 "var/cll/CLLParser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv131) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((t : (
# 46 "var/cll/CLLParser.mly"
       (string)
# 5269 "var/cll/CLLParser.ml"
    )) : (
# 46 "var/cll/CLLParser.mly"
       (string)
# 5273 "var/cll/CLLParser.ml"
    )) = _v in
    let (_startpos_t_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos_t_ in
    let _v : (
# 53 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 5280 "var/cll/CLLParser.ml"
    ) = let _startpos = _startpos_t_ in
    
# 190 "var/cll/CLLParser.mly"
    ( Id (make_node _startpos t) )
# 5285 "var/cll/CLLParser.ml"
     in
    _menhir_goto_l_expr _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv132)

and _menhir_run60 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LP ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv127 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADDRESS ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | BOOL _v ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
        | CPL ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | INT _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
        | LABEL _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LP ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | MULT ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NOT ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | SUB ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState61) : 'freshtv128)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv129 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv130)

and _menhir_run64 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LP ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv123 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADDRESS ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | BOOL _v ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
        | CPL ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | INT _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
        | LABEL _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LP ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | MULT ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NOT ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | SUB ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState65) : 'freshtv124)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv125 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv126)

and _menhir_run82 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv121) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_startpos__1_ : Lexing.position) = _startpos in
    ((let _1 = () in
    let _startpos = _startpos__1_ in
    let _v : (
# 54 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 5385 "var/cll/CLLParser.ml"
    ) = 
# 199 "var/cll/CLLParser.mly"
    ( Exit )
# 5389 "var/cll/CLLParser.ml"
     in
    _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv122)

and _menhir_run83 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv119) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_startpos__1_ : Lexing.position) = _startpos in
    ((let _1 = () in
    let _startpos = _startpos__1_ in
    let _v : (
# 54 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 5405 "var/cll/CLLParser.ml"
    ) = let _startpos = _startpos__1_ in
    
# 208 "var/cll/CLLParser.mly"
    ( 
      Continue (make_node _startpos ())
    )
# 5412 "var/cll/CLLParser.ml"
     in
    _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv120)

and _menhir_run84 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv117) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_startpos__1_ : Lexing.position) = _startpos in
    ((let _1 = () in
    let _startpos = _startpos__1_ in
    let _v : (
# 54 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 5428 "var/cll/CLLParser.ml"
    ) = let _startpos = _startpos__1_ in
    
# 203 "var/cll/CLLParser.mly"
    ( 
      Break (make_node _startpos ())
    )
# 5435 "var/cll/CLLParser.ml"
     in
    _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv118)

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState141 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv15 * _menhir_state * (
# 46 "var/cll/CLLParser.mly"
       (string)
# 5447 "var/cll/CLLParser.ml"
        ) * Lexing.position)) * (
# 44 "var/cll/CLLParser.mly"
       (bool)
# 5451 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv16)
    | MenhirState139 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv17 * _menhir_state * (
# 46 "var/cll/CLLParser.mly"
       (string)
# 5460 "var/cll/CLLParser.ml"
        ) * Lexing.position)) * (
# 45 "var/cll/CLLParser.mly"
       (int)
# 5464 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv18)
    | MenhirState136 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv19 * _menhir_state * (
# 60 "var/cll/CLLParser.mly"
      ((CLLTree.procedure_definition Cycle.cycle) ARTTree.compiler_type)
# 5473 "var/cll/CLLParser.ml"
        ) * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv20)
    | MenhirState131 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv21 * _menhir_state * (
# 46 "var/cll/CLLParser.mly"
       (string)
# 5482 "var/cll/CLLParser.ml"
        ) * Lexing.position)))) * _menhir_state * (
# 51 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 5486 "var/cll/CLLParser.ml"
        )) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv22)
    | MenhirState126 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv23 * _menhir_state * (
# 54 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 5495 "var/cll/CLLParser.ml"
        ) * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv24)
    | MenhirState123 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv25 * _menhir_state * (
# 57 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 5504 "var/cll/CLLParser.ml"
        ) * Lexing.position) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv26)
    | MenhirState121 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv27 * _menhir_state * (
# 57 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 5513 "var/cll/CLLParser.ml"
        ) * Lexing.position) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv28)
    | MenhirState120 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv29 * _menhir_state * (
# 57 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 5522 "var/cll/CLLParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv30)
    | MenhirState118 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv31 * _menhir_state * (
# 54 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 5531 "var/cll/CLLParser.ml"
        ) * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv32)
    | MenhirState113 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv33 * _menhir_state * Lexing.position)) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 5540 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 56 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 5544 "var/cll/CLLParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv34)
    | MenhirState110 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv35 * _menhir_state * Lexing.position)) * _menhir_state * (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 5553 "var/cll/CLLParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv36)
    | MenhirState108 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv37 * _menhir_state * Lexing.position)) * _menhir_state * (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 5562 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 5566 "var/cll/CLLParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv38)
    | MenhirState105 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv39 * _menhir_state * Lexing.position)) * _menhir_state * (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 5575 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 5579 "var/cll/CLLParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv40)
    | MenhirState103 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((('freshtv41 * _menhir_state * Lexing.position)) * _menhir_state * (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 5588 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 5592 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 5596 "var/cll/CLLParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv42)
    | MenhirState101 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv43 * _menhir_state * Lexing.position)) * _menhir_state * (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 5605 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 5609 "var/cll/CLLParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv44)
    | MenhirState99 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv45 * _menhir_state * Lexing.position)) * _menhir_state * (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 5618 "var/cll/CLLParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv46)
    | MenhirState96 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv47 * _menhir_state * Lexing.position)) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 5627 "var/cll/CLLParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv48)
    | MenhirState94 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv49 * _menhir_state * (
# 55 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 5636 "var/cll/CLLParser.ml"
        ) * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv50)
    | MenhirState81 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv51 * _menhir_state * Lexing.position)) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 5645 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 5649 "var/cll/CLLParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv52)
    | MenhirState78 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv53 * _menhir_state * Lexing.position)) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 5658 "var/cll/CLLParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv54)
    | MenhirState75 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv55 * _menhir_state * (
# 53 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 5667 "var/cll/CLLParser.ml"
        ) * Lexing.position) * 'tv_assign_binop) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv56)
    | MenhirState65 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv57 * _menhir_state * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv58)
    | MenhirState63 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv59 * _menhir_state * Lexing.position)) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 5681 "var/cll/CLLParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv60)
    | MenhirState61 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv61 * _menhir_state * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv62)
    | MenhirState59 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv63 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv64)
    | MenhirState58 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv65 * _menhir_state * Lexing.position) * _menhir_state) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 5700 "var/cll/CLLParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv66)
    | MenhirState56 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv67 * _menhir_state * Lexing.position) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv68)
    | MenhirState48 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv69 * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 5714 "var/cll/CLLParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv70)
    | MenhirState46 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv71 * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 5723 "var/cll/CLLParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv72)
    | MenhirState44 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv73 * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 5732 "var/cll/CLLParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv74)
    | MenhirState42 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv75 * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 5741 "var/cll/CLLParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv76)
    | MenhirState40 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv77 * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 5750 "var/cll/CLLParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv78)
    | MenhirState38 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv79 * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 5759 "var/cll/CLLParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv80)
    | MenhirState36 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv81 * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 5768 "var/cll/CLLParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv82)
    | MenhirState34 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv83 * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 5777 "var/cll/CLLParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv84)
    | MenhirState32 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv85 * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 5786 "var/cll/CLLParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv86)
    | MenhirState29 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv87 * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 5795 "var/cll/CLLParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv88)
    | MenhirState27 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv89 * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 5804 "var/cll/CLLParser.ml"
        )) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv90)
    | MenhirState25 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv91 * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 5813 "var/cll/CLLParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv92)
    | MenhirState23 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv93 * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 5822 "var/cll/CLLParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv94)
    | MenhirState16 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv95 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv96)
    | MenhirState13 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv97 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv98)
    | MenhirState12 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv99 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv100)
    | MenhirState11 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv101 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv102)
    | MenhirState10 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv103 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv104)
    | MenhirState9 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv105 * _menhir_state * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv106)
    | MenhirState6 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv107 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv108)
    | MenhirState5 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv109 * _menhir_state * (
# 46 "var/cll/CLLParser.mly"
       (string)
# 5866 "var/cll/CLLParser.ml"
        ) * Lexing.position)))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv110)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv115) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv113) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState0 in
        let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv111) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_startpos__1_ : Lexing.position) = _startpos in
        ((let _1 = () in
        let _v : (
# 50 "var/cll/CLLParser.mly"
      (CLLTree.cll_prog ARTTree.compiler_type)
# 5885 "var/cll/CLLParser.ml"
        ) = let _startpos = _startpos__1_ in
        
# 96 "var/cll/CLLParser.mly"
    ( 
      raise_syntax_error _startpos "CLL program structure: list procedure declaration .data <declarations>" 
    )
# 5892 "var/cll/CLLParser.ml"
         in
        _menhir_goto_program _menhir_env _menhir_stack _menhir_s _v) : 'freshtv112)) : 'freshtv114)) : 'freshtv116)

and _menhir_reduce66 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let (_, _startpos) = Obj.magic _menhir_stack in
    let _v : (
# 60 "var/cll/CLLParser.mly"
      ((CLLTree.procedure_definition Cycle.cycle) ARTTree.compiler_type)
# 5902 "var/cll/CLLParser.ml"
    ) = 
# 117 "var/cll/CLLParser.mly"
  (
    {syntax_tree = Cycle.empty_cycle; tag_set = Tagset.empty}
  )
# 5908 "var/cll/CLLParser.ml"
     in
    _menhir_goto_procedure_definitions _menhir_env _menhir_stack _menhir_s _v _startpos

and _menhir_run2 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 46 "var/cll/CLLParser.mly"
       (string)
# 5915 "var/cll/CLLParser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LP ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv11 * _menhir_state * (
# 46 "var/cll/CLLParser.mly"
       (string)
# 5927 "var/cll/CLLParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RP ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv7 * _menhir_state * (
# 46 "var/cll/CLLParser.mly"
       (string)
# 5937 "var/cll/CLLParser.ml"
            ) * Lexing.position)) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LB ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv3 * _menhir_state * (
# 46 "var/cll/CLLParser.mly"
       (string)
# 5947 "var/cll/CLLParser.ml"
                ) * Lexing.position))) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | BREAK ->
                    _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | CONTINUE ->
                    _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | EXIT ->
                    _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | FOR ->
                    _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | IF ->
                    _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | LABEL _v ->
                    _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | MULT ->
                    _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | NOP ->
                    _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | PRINT ->
                    _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | RETURN ->
                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | WHILE ->
                    _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | RB ->
                    _menhir_reduce62 _menhir_env (Obj.magic _menhir_stack) MenhirState5
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState5) : 'freshtv4)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv5 * _menhir_state * (
# 46 "var/cll/CLLParser.mly"
       (string)
# 5987 "var/cll/CLLParser.ml"
                ) * Lexing.position))) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv6)) : 'freshtv8)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv9 * _menhir_state * (
# 46 "var/cll/CLLParser.mly"
       (string)
# 5998 "var/cll/CLLParser.ml"
            ) * Lexing.position)) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv10)) : 'freshtv12)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv13 * _menhir_state * (
# 46 "var/cll/CLLParser.mly"
       (string)
# 6009 "var/cll/CLLParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv14)

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
# 50 "var/cll/CLLParser.mly"
      (CLLTree.cll_prog ARTTree.compiler_type)
# 6029 "var/cll/CLLParser.ml"
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
        _menhir_reduce66 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0) : 'freshtv2))

# 219 "/usr/share/menhir/standard.mly"
  


# 6061 "var/cll/CLLParser.ml"
