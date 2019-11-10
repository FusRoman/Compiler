
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

# 157 "var/cll/CLLParser.ml"

let rec _menhir_goto_assigns : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 162 "var/cll/CLLParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState78 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv491 * _menhir_state * Lexing.position)) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 172 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 176 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RP ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv487 * _menhir_state * Lexing.position)) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 186 "var/cll/CLLParser.ml"
            ))) * _menhir_state * (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 190 "var/cll/CLLParser.ml"
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
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState81) : 'freshtv488)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv489 * _menhir_state * Lexing.position)) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 230 "var/cll/CLLParser.ml"
            ))) * _menhir_state * (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 234 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv490)) : 'freshtv492)
    | MenhirState94 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv495 * _menhir_state * (
# 55 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 243 "var/cll/CLLParser.ml"
        ) * Lexing.position)) * _menhir_state * (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 247 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv493 * _menhir_state * (
# 55 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 253 "var/cll/CLLParser.ml"
        ) * Lexing.position)) * _menhir_state * (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 257 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (a : (
# 55 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 262 "var/cll/CLLParser.ml"
        )), _startpos_a_), _, (s : (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 266 "var/cll/CLLParser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 272 "var/cll/CLLParser.ml"
        ) = 
# 306 "var/cll/CLLParser.mly"
                           ( Cycle.prepend s a )
# 276 "var/cll/CLLParser.ml"
         in
        _menhir_goto_assigns _menhir_env _menhir_stack _menhir_s _v) : 'freshtv494)) : 'freshtv496)
    | MenhirState65 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv503 * _menhir_state * Lexing.position)) * _menhir_state * (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 284 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RP ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv497 * _menhir_state * Lexing.position)) * _menhir_state * (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 294 "var/cll/CLLParser.ml"
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
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState110) : 'freshtv498)
        | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv499 * _menhir_state * Lexing.position)) * _menhir_state * (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 332 "var/cll/CLLParser.ml"
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
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState99) : 'freshtv500)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv501 * _menhir_state * Lexing.position)) * _menhir_state * (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 366 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv502)) : 'freshtv504)
    | MenhirState101 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv509 * _menhir_state * Lexing.position)) * _menhir_state * (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 375 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 379 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 383 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RP ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv505 * _menhir_state * Lexing.position)) * _menhir_state * (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 393 "var/cll/CLLParser.ml"
            ))) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 397 "var/cll/CLLParser.ml"
            ))) * _menhir_state * (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 401 "var/cll/CLLParser.ml"
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
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState103) : 'freshtv506)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv507 * _menhir_state * Lexing.position)) * _menhir_state * (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 441 "var/cll/CLLParser.ml"
            ))) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 445 "var/cll/CLLParser.ml"
            ))) * _menhir_state * (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 449 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv508)) : 'freshtv510)
    | MenhirState99 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv515 * _menhir_state * Lexing.position)) * _menhir_state * (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 458 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 462 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RP ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv511 * _menhir_state * Lexing.position)) * _menhir_state * (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 472 "var/cll/CLLParser.ml"
            ))) * _menhir_state * (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 476 "var/cll/CLLParser.ml"
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
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState108) : 'freshtv512)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv513 * _menhir_state * Lexing.position)) * _menhir_state * (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 516 "var/cll/CLLParser.ml"
            ))) * _menhir_state * (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 520 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv514)) : 'freshtv516)
    | _ ->
        _menhir_fail ()

and _menhir_goto_assign_unop : _menhir_env -> 'ttv_tail -> 'tv_assign_unop -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv485 * _menhir_state * (
# 53 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 533 "var/cll/CLLParser.ml"
    ) * Lexing.position) = Obj.magic _menhir_stack in
    let (_v : 'tv_assign_unop) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv483 * _menhir_state * (
# 53 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 540 "var/cll/CLLParser.ml"
    ) * Lexing.position) = Obj.magic _menhir_stack in
    let ((op : 'tv_assign_unop) : 'tv_assign_unop) = _v in
    ((let (_menhir_stack, _menhir_s, (l : (
# 53 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 546 "var/cll/CLLParser.ml"
    )), _startpos_l_) = _menhir_stack in
    let _startpos = _startpos_l_ in
    let _v : (
# 55 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 552 "var/cll/CLLParser.ml"
    ) = 
# 233 "var/cll/CLLParser.mly"
    (
      UnopAssign(l, op)
    )
# 558 "var/cll/CLLParser.ml"
     in
    _menhir_goto_assign _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv484)) : 'freshtv486)

and _menhir_goto_assign_binop : _menhir_env -> 'ttv_tail -> 'tv_assign_binop -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv481 * _menhir_state * (
# 53 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 569 "var/cll/CLLParser.ml"
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
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState75) : 'freshtv482)

and _menhir_goto_assign : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 55 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 600 "var/cll/CLLParser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState5 | MenhirState6 | MenhirState58 | MenhirState59 | MenhirState126 | MenhirState118 | MenhirState120 | MenhirState123 | MenhirState121 | MenhirState63 | MenhirState113 | MenhirState110 | MenhirState108 | MenhirState105 | MenhirState103 | MenhirState96 | MenhirState81 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv471 * _menhir_state * (
# 55 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 610 "var/cll/CLLParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv469 * _menhir_state * (
# 55 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 616 "var/cll/CLLParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (a : (
# 55 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 621 "var/cll/CLLParser.ml"
        )), _startpos_a_) = _menhir_stack in
        let _startpos = _startpos_a_ in
        let _v : (
# 54 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 627 "var/cll/CLLParser.ml"
        ) = 
# 223 "var/cll/CLLParser.mly"
    ( a )
# 631 "var/cll/CLLParser.ml"
         in
        _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv470)) : 'freshtv472)
    | MenhirState65 | MenhirState99 | MenhirState101 | MenhirState94 | MenhirState78 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv479 * _menhir_state * (
# 55 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 639 "var/cll/CLLParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COMMA ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv473 * _menhir_state * (
# 55 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 649 "var/cll/CLLParser.ml"
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
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState94) : 'freshtv474)
        | RP | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv475 * _menhir_state * (
# 55 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 667 "var/cll/CLLParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (a : (
# 55 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 672 "var/cll/CLLParser.ml"
            )), _startpos_a_) = _menhir_stack in
            let _v : (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 677 "var/cll/CLLParser.ml"
            ) = 
# 305 "var/cll/CLLParser.mly"
            ( Cycle.from_elt a )
# 681 "var/cll/CLLParser.ml"
             in
            _menhir_goto_assigns _menhir_env _menhir_stack _menhir_s _v) : 'freshtv476)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv477 * _menhir_state * (
# 55 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 691 "var/cll/CLLParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv478)) : 'freshtv480)
    | _ ->
        _menhir_fail ()

and _menhir_run23 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 701 "var/cll/CLLParser.ml"
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
# 733 "var/cll/CLLParser.ml"
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
# 765 "var/cll/CLLParser.ml"
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
# 797 "var/cll/CLLParser.ml"
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
# 829 "var/cll/CLLParser.ml"
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
# 862 "var/cll/CLLParser.ml"
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
# 894 "var/cll/CLLParser.ml"
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
# 926 "var/cll/CLLParser.ml"
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
# 958 "var/cll/CLLParser.ml"
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
# 990 "var/cll/CLLParser.ml"
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
# 1022 "var/cll/CLLParser.ml"
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
# 1054 "var/cll/CLLParser.ml"
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
# 1086 "var/cll/CLLParser.ml"
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
# 1118 "var/cll/CLLParser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState6 | MenhirState58 | MenhirState113 | MenhirState63 | MenhirState110 | MenhirState108 | MenhirState105 | MenhirState103 | MenhirState96 | MenhirState81 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv461 * _menhir_state * (
# 57 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 1128 "var/cll/CLLParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv459 * _menhir_state * (
# 57 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 1134 "var/cll/CLLParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (c : (
# 57 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 1139 "var/cll/CLLParser.ml"
        )), _startpos_c_) = _menhir_stack in
        let _v : (
# 56 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 1144 "var/cll/CLLParser.ml"
        ) = 
# 258 "var/cll/CLLParser.mly"
    ( c )
# 1148 "var/cll/CLLParser.ml"
         in
        _menhir_goto_block _menhir_env _menhir_stack _menhir_s _v) : 'freshtv460)) : 'freshtv462)
    | MenhirState5 | MenhirState59 | MenhirState126 | MenhirState120 | MenhirState123 | MenhirState121 | MenhirState118 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv467 * _menhir_state * (
# 57 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 1156 "var/cll/CLLParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BREAK ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | COLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv463 * _menhir_state * (
# 57 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 1168 "var/cll/CLLParser.ml"
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
                _menhir_reduce61 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState123) : 'freshtv464)
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
            let (_menhir_stack : 'freshtv465 * _menhir_state * (
# 57 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 1226 "var/cll/CLLParser.ml"
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
                _menhir_reduce61 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState121) : 'freshtv466)
        | WHILE ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | RB ->
            _menhir_reduce61 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState120) : 'freshtv468)
    | _ ->
        _menhir_fail ()

and _menhir_goto_data_declarations : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 59 "var/cll/CLLParser.mly"
      (ARTTree.datas ARTTree.compiler_type)
# 1275 "var/cll/CLLParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState139 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv449 * _menhir_state * (
# 46 "var/cll/CLLParser.mly"
       (string)
# 1285 "var/cll/CLLParser.ml"
        ) * Lexing.position)) * (
# 45 "var/cll/CLLParser.mly"
       (int)
# 1289 "var/cll/CLLParser.ml"
        )) * _menhir_state * (
# 59 "var/cll/CLLParser.mly"
      (ARTTree.datas ARTTree.compiler_type)
# 1293 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv447 * _menhir_state * (
# 46 "var/cll/CLLParser.mly"
       (string)
# 1299 "var/cll/CLLParser.ml"
        ) * Lexing.position)) * (
# 45 "var/cll/CLLParser.mly"
       (int)
# 1303 "var/cll/CLLParser.ml"
        )) * _menhir_state * (
# 59 "var/cll/CLLParser.mly"
      (ARTTree.datas ARTTree.compiler_type)
# 1307 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s, (t : (
# 46 "var/cll/CLLParser.mly"
       (string)
# 1312 "var/cll/CLLParser.ml"
        )), _startpos_t_), (v : (
# 45 "var/cll/CLLParser.mly"
       (int)
# 1316 "var/cll/CLLParser.ml"
        ))), _, (s : (
# 59 "var/cll/CLLParser.mly"
      (ARTTree.datas ARTTree.compiler_type)
# 1320 "var/cll/CLLParser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : (
# 59 "var/cll/CLLParser.mly"
      (ARTTree.datas ARTTree.compiler_type)
# 1326 "var/cll/CLLParser.ml"
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
# 1342 "var/cll/CLLParser.ml"
         in
        _menhir_goto_data_declarations _menhir_env _menhir_stack _menhir_s _v) : 'freshtv448)) : 'freshtv450)
    | MenhirState136 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv457 * _menhir_state * (
# 60 "var/cll/CLLParser.mly"
      ((CLLTree.procedure_definition Cycle.cycle) ARTTree.compiler_type)
# 1350 "var/cll/CLLParser.ml"
        ) * Lexing.position)) * _menhir_state * (
# 59 "var/cll/CLLParser.mly"
      (ARTTree.datas ARTTree.compiler_type)
# 1354 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EOF ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv453 * _menhir_state * (
# 60 "var/cll/CLLParser.mly"
      ((CLLTree.procedure_definition Cycle.cycle) ARTTree.compiler_type)
# 1364 "var/cll/CLLParser.ml"
            ) * Lexing.position)) * _menhir_state * (
# 59 "var/cll/CLLParser.mly"
      (ARTTree.datas ARTTree.compiler_type)
# 1368 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv451 * _menhir_state * (
# 60 "var/cll/CLLParser.mly"
      ((CLLTree.procedure_definition Cycle.cycle) ARTTree.compiler_type)
# 1374 "var/cll/CLLParser.ml"
            ) * Lexing.position)) * _menhir_state * (
# 59 "var/cll/CLLParser.mly"
      (ARTTree.datas ARTTree.compiler_type)
# 1378 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (globals : (
# 60 "var/cll/CLLParser.mly"
      ((CLLTree.procedure_definition Cycle.cycle) ARTTree.compiler_type)
# 1383 "var/cll/CLLParser.ml"
            )), _startpos_globals_), _, (data : (
# 59 "var/cll/CLLParser.mly"
      (ARTTree.datas ARTTree.compiler_type)
# 1387 "var/cll/CLLParser.ml"
            ))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : (
# 50 "var/cll/CLLParser.mly"
      (CLLTree.cll_prog ARTTree.compiler_type)
# 1394 "var/cll/CLLParser.ml"
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
# 1407 "var/cll/CLLParser.ml"
             in
            _menhir_goto_program _menhir_env _menhir_stack _menhir_s _v) : 'freshtv452)) : 'freshtv454)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv455 * _menhir_state * (
# 60 "var/cll/CLLParser.mly"
      ((CLLTree.procedure_definition Cycle.cycle) ARTTree.compiler_type)
# 1417 "var/cll/CLLParser.ml"
            ) * Lexing.position)) * _menhir_state * (
# 59 "var/cll/CLLParser.mly"
      (ARTTree.datas ARTTree.compiler_type)
# 1421 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv456)) : 'freshtv458)
    | _ ->
        _menhir_fail ()

and _menhir_run67 : _menhir_env -> 'ttv_tail -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv445) = Obj.magic _menhir_stack in
    ((let _1 = () in
    let _v : 'tv_assign_binop = 
# 241 "var/cll/CLLParser.mly"
              ( SubAssign )
# 1437 "var/cll/CLLParser.ml"
     in
    _menhir_goto_assign_binop _menhir_env _menhir_stack _v) : 'freshtv446)

and _menhir_run68 : _menhir_env -> 'ttv_tail -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv443) = Obj.magic _menhir_stack in
    ((let _1 = () in
    let _v : 'tv_assign_binop = 
# 242 "var/cll/CLLParser.mly"
              ( MultAssign )
# 1450 "var/cll/CLLParser.ml"
     in
    _menhir_goto_assign_binop _menhir_env _menhir_stack _v) : 'freshtv444)

and _menhir_run69 : _menhir_env -> 'ttv_tail -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv441) = Obj.magic _menhir_stack in
    ((let _1 = () in
    let _v : 'tv_assign_unop = 
# 247 "var/cll/CLLParser.mly"
              ( Incr )
# 1463 "var/cll/CLLParser.ml"
     in
    _menhir_goto_assign_unop _menhir_env _menhir_stack _v) : 'freshtv442)

and _menhir_run70 : _menhir_env -> 'ttv_tail -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv439) = Obj.magic _menhir_stack in
    ((let _1 = () in
    let _v : 'tv_assign_binop = 
# 243 "var/cll/CLLParser.mly"
              ( DivAssign )
# 1476 "var/cll/CLLParser.ml"
     in
    _menhir_goto_assign_binop _menhir_env _menhir_stack _v) : 'freshtv440)

and _menhir_run71 : _menhir_env -> 'ttv_tail -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv437) = Obj.magic _menhir_stack in
    ((let _1 = () in
    let _v : 'tv_assign_unop = 
# 248 "var/cll/CLLParser.mly"
              ( Decr )
# 1489 "var/cll/CLLParser.ml"
     in
    _menhir_goto_assign_unop _menhir_env _menhir_stack _v) : 'freshtv438)

and _menhir_run72 : _menhir_env -> 'ttv_tail -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv435) = Obj.magic _menhir_stack in
    ((let _1 = () in
    let _v : 'tv_assign_binop = 
# 239 "var/cll/CLLParser.mly"
              ( Standard )
# 1502 "var/cll/CLLParser.ml"
     in
    _menhir_goto_assign_binop _menhir_env _menhir_stack _v) : 'freshtv436)

and _menhir_run73 : _menhir_env -> 'ttv_tail -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv433) = Obj.magic _menhir_stack in
    ((let _1 = () in
    let _v : 'tv_assign_binop = 
# 240 "var/cll/CLLParser.mly"
              ( AddAssign )
# 1515 "var/cll/CLLParser.ml"
     in
    _menhir_goto_assign_binop _menhir_env _menhir_stack _v) : 'freshtv434)

and _menhir_reduce29 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 53 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1522 "var/cll/CLLParser.ml"
) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (l : (
# 53 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1528 "var/cll/CLLParser.ml"
    )), _startpos_l_) = _menhir_stack in
    let _v : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1533 "var/cll/CLLParser.ml"
    ) = 
# 149 "var/cll/CLLParser.mly"
    ( l )
# 1537 "var/cll/CLLParser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1544 "var/cll/CLLParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState16 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv295 * _menhir_state) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1554 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv293 * _menhir_state) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1560 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _, (e : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1565 "var/cll/CLLParser.ml"
        ))) = _menhir_stack in
        let _1 = () in
        let _v : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1571 "var/cll/CLLParser.ml"
        ) = 
# 181 "var/cll/CLLParser.mly"
    ( Unop(Cpl, e) )
# 1575 "var/cll/CLLParser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv294)) : 'freshtv296)
    | MenhirState13 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv303 * _menhir_state) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1583 "var/cll/CLLParser.ml"
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
            let (_menhir_stack : ('freshtv299 * _menhir_state) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1617 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv297 * _menhir_state) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1624 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (e : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1629 "var/cll/CLLParser.ml"
            ))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1636 "var/cll/CLLParser.ml"
            ) = 
# 151 "var/cll/CLLParser.mly"
    ( e )
# 1640 "var/cll/CLLParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv298)) : 'freshtv300)
        | SUB ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv301 * _menhir_state) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1652 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv302)) : 'freshtv304)
    | MenhirState23 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv309 * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1661 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1665 "var/cll/CLLParser.ml"
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
            let (_menhir_stack : (('freshtv305 * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1681 "var/cll/CLLParser.ml"
            ))) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1685 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (e1 : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1690 "var/cll/CLLParser.ml"
            ))), _, (e2 : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1694 "var/cll/CLLParser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1700 "var/cll/CLLParser.ml"
            ) = 
# 155 "var/cll/CLLParser.mly"
    ( Binop(e1, Sub, e2) )
# 1704 "var/cll/CLLParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv306)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv307 * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1714 "var/cll/CLLParser.ml"
            ))) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1718 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv308)) : 'freshtv310)
    | MenhirState25 ->
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
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv311 * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1737 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1741 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (e1 : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1746 "var/cll/CLLParser.ml"
        ))), _, (e2 : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1750 "var/cll/CLLParser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1756 "var/cll/CLLParser.ml"
        ) = 
# 161 "var/cll/CLLParser.mly"
    ( Binop(e1, Rem, e2) )
# 1760 "var/cll/CLLParser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv312)) : 'freshtv314)
    | MenhirState27 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv317 * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1768 "var/cll/CLLParser.ml"
        )) * Lexing.position) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1772 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv315 * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1778 "var/cll/CLLParser.ml"
        )) * Lexing.position) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1782 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s, (e1 : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1787 "var/cll/CLLParser.ml"
        ))), _startpos__2_), _, (e2 : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1791 "var/cll/CLLParser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1797 "var/cll/CLLParser.ml"
        ) = 
# 157 "var/cll/CLLParser.mly"
    ( Binop(e1, Mult, e2) )
# 1801 "var/cll/CLLParser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv316)) : 'freshtv318)
    | MenhirState29 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv321 * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1809 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1813 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv319 * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1819 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1823 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (e1 : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1828 "var/cll/CLLParser.ml"
        ))), _, (e2 : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1832 "var/cll/CLLParser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1838 "var/cll/CLLParser.ml"
        ) = 
# 159 "var/cll/CLLParser.mly"
    ( Binop(e1, Div, e2) )
# 1842 "var/cll/CLLParser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv320)) : 'freshtv322)
    | MenhirState32 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv327 * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1850 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1854 "var/cll/CLLParser.ml"
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
            let (_menhir_stack : (('freshtv323 * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1886 "var/cll/CLLParser.ml"
            ))) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1890 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (e1 : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1895 "var/cll/CLLParser.ml"
            ))), _, (e2 : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1899 "var/cll/CLLParser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1905 "var/cll/CLLParser.ml"
            ) = 
# 165 "var/cll/CLLParser.mly"
    ( Binop(e1, Or, e2) )
# 1909 "var/cll/CLLParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv324)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv325 * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1919 "var/cll/CLLParser.ml"
            ))) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1923 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv326)) : 'freshtv328)
    | MenhirState34 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv333 * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1932 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1936 "var/cll/CLLParser.ml"
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
            let (_menhir_stack : (('freshtv329 * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1956 "var/cll/CLLParser.ml"
            ))) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1960 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (e1 : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1965 "var/cll/CLLParser.ml"
            ))), _, (e2 : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1969 "var/cll/CLLParser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1975 "var/cll/CLLParser.ml"
            ) = 
# 177 "var/cll/CLLParser.mly"
    ( Binop(e1, Neq, e2) )
# 1979 "var/cll/CLLParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv330)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv331 * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1989 "var/cll/CLLParser.ml"
            ))) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 1993 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv332)) : 'freshtv334)
    | MenhirState36 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv339 * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2002 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2006 "var/cll/CLLParser.ml"
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
            let (_menhir_stack : (('freshtv335 * _menhir_state * (
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
# 153 "var/cll/CLLParser.mly"
    ( Binop(e1, Add, e2) )
# 2045 "var/cll/CLLParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv336)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv337 * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2055 "var/cll/CLLParser.ml"
            ))) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2059 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv338)) : 'freshtv340)
    | MenhirState38 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv345 * _menhir_state * (
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
            let (_menhir_stack : (('freshtv341 * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2092 "var/cll/CLLParser.ml"
            ))) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2096 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (e1 : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2101 "var/cll/CLLParser.ml"
            ))), _, (e2 : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2105 "var/cll/CLLParser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2111 "var/cll/CLLParser.ml"
            ) = 
# 167 "var/cll/CLLParser.mly"
    ( Binop(e1, Lt, e2) )
# 2115 "var/cll/CLLParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv342)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv343 * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2125 "var/cll/CLLParser.ml"
            ))) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2129 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv344)) : 'freshtv346)
    | MenhirState40 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv351 * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2138 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2142 "var/cll/CLLParser.ml"
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
            let (_menhir_stack : (('freshtv347 * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2162 "var/cll/CLLParser.ml"
            ))) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2166 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (e1 : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2171 "var/cll/CLLParser.ml"
            ))), _, (e2 : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2175 "var/cll/CLLParser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2181 "var/cll/CLLParser.ml"
            ) = 
# 169 "var/cll/CLLParser.mly"
    ( Binop(e1, Le, e2) )
# 2185 "var/cll/CLLParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv348)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv349 * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2195 "var/cll/CLLParser.ml"
            ))) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2199 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv350)) : 'freshtv352)
    | MenhirState42 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv357 * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2208 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2212 "var/cll/CLLParser.ml"
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
            let (_menhir_stack : (('freshtv353 * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2232 "var/cll/CLLParser.ml"
            ))) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2236 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (e1 : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2241 "var/cll/CLLParser.ml"
            ))), _, (e2 : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2245 "var/cll/CLLParser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2251 "var/cll/CLLParser.ml"
            ) = 
# 171 "var/cll/CLLParser.mly"
    ( Binop(e1, Gt, e2) )
# 2255 "var/cll/CLLParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv354)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv355 * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2265 "var/cll/CLLParser.ml"
            ))) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2269 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv356)) : 'freshtv358)
    | MenhirState44 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv363 * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2278 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2282 "var/cll/CLLParser.ml"
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
            let (_menhir_stack : (('freshtv359 * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2302 "var/cll/CLLParser.ml"
            ))) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2306 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (e1 : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2311 "var/cll/CLLParser.ml"
            ))), _, (e2 : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2315 "var/cll/CLLParser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2321 "var/cll/CLLParser.ml"
            ) = 
# 173 "var/cll/CLLParser.mly"
    ( Binop(e1, Ge, e2) )
# 2325 "var/cll/CLLParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv360)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv361 * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2335 "var/cll/CLLParser.ml"
            ))) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2339 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv362)) : 'freshtv364)
    | MenhirState46 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv369 * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2348 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2352 "var/cll/CLLParser.ml"
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
            let (_menhir_stack : (('freshtv365 * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2372 "var/cll/CLLParser.ml"
            ))) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2376 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (e1 : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2381 "var/cll/CLLParser.ml"
            ))), _, (e2 : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2385 "var/cll/CLLParser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2391 "var/cll/CLLParser.ml"
            ) = 
# 175 "var/cll/CLLParser.mly"
    ( Binop(e1, Eq, e2) )
# 2395 "var/cll/CLLParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv366)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv367 * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2405 "var/cll/CLLParser.ml"
            ))) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2409 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv368)) : 'freshtv370)
    | MenhirState48 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv375 * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2418 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2422 "var/cll/CLLParser.ml"
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
            let (_menhir_stack : (('freshtv371 * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2454 "var/cll/CLLParser.ml"
            ))) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2458 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (e1 : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2463 "var/cll/CLLParser.ml"
            ))), _, (e2 : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2467 "var/cll/CLLParser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2473 "var/cll/CLLParser.ml"
            ) = 
# 163 "var/cll/CLLParser.mly"
    ( Binop(e1, And, e2) )
# 2477 "var/cll/CLLParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv372)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv373 * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2487 "var/cll/CLLParser.ml"
            ))) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2491 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv374)) : 'freshtv376)
    | MenhirState12 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv379 * _menhir_state * Lexing.position) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2500 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv377 * _menhir_state * Lexing.position) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2506 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _startpos__1_), _, (l : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2511 "var/cll/CLLParser.ml"
        ))) = _menhir_stack in
        let _1 = () in
        let _startpos = _startpos__1_ in
        let _v : (
# 53 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2518 "var/cll/CLLParser.ml"
        ) = 
# 192 "var/cll/CLLParser.mly"
    ( LStar l )
# 2522 "var/cll/CLLParser.ml"
         in
        _menhir_goto_l_expr _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv378)) : 'freshtv380)
    | MenhirState11 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv383 * _menhir_state) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2530 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv381 * _menhir_state) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2536 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _, (e : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2541 "var/cll/CLLParser.ml"
        ))) = _menhir_stack in
        let _1 = () in
        let _v : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2547 "var/cll/CLLParser.ml"
        ) = 
# 183 "var/cll/CLLParser.mly"
    ( Unop(Not, e) )
# 2551 "var/cll/CLLParser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv382)) : 'freshtv384)
    | MenhirState10 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv389 * _menhir_state) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2559 "var/cll/CLLParser.ml"
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
            let (_menhir_stack : ('freshtv385 * _menhir_state) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2575 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (e : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2580 "var/cll/CLLParser.ml"
            ))) = _menhir_stack in
            let _1 = () in
            let _v : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2586 "var/cll/CLLParser.ml"
            ) = 
# 179 "var/cll/CLLParser.mly"
    ( Unop(Minus, e) )
# 2590 "var/cll/CLLParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv386)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv387 * _menhir_state) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2600 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv388)) : 'freshtv390)
    | MenhirState9 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv397 * _menhir_state * Lexing.position)) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2609 "var/cll/CLLParser.ml"
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
            let (_menhir_stack : (('freshtv393 * _menhir_state * Lexing.position)) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2643 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv391 * _menhir_state * Lexing.position)) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2650 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, _startpos__1_), _, (e : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2655 "var/cll/CLLParser.ml"
            ))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _v : (
# 54 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 2664 "var/cll/CLLParser.ml"
            ) = 
# 201 "var/cll/CLLParser.mly"
    ( Print e )
# 2668 "var/cll/CLLParser.ml"
             in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv392)) : 'freshtv394)
        | SUB ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv395 * _menhir_state * Lexing.position)) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2680 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv396)) : 'freshtv398)
    | MenhirState56 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv403 * _menhir_state * Lexing.position) * _menhir_state) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2689 "var/cll/CLLParser.ml"
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
            let (_menhir_stack : (('freshtv399 * _menhir_state * Lexing.position) * _menhir_state) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2723 "var/cll/CLLParser.ml"
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
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState58) : 'freshtv400)
        | SUB ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv401 * _menhir_state * Lexing.position) * _menhir_state) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2765 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv402)) : 'freshtv404)
    | MenhirState61 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv409 * _menhir_state * Lexing.position)) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2774 "var/cll/CLLParser.ml"
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
            let (_menhir_stack : (('freshtv405 * _menhir_state * Lexing.position)) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2808 "var/cll/CLLParser.ml"
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
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState63) : 'freshtv406)
        | SUB ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv407 * _menhir_state * Lexing.position)) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2850 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv408)) : 'freshtv410)
    | MenhirState75 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv415 * _menhir_state * (
# 53 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2859 "var/cll/CLLParser.ml"
        ) * Lexing.position) * 'tv_assign_binop) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2863 "var/cll/CLLParser.ml"
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
            let (_menhir_stack : (('freshtv411 * _menhir_state * (
# 53 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2899 "var/cll/CLLParser.ml"
            ) * Lexing.position) * 'tv_assign_binop) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2903 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (l : (
# 53 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2908 "var/cll/CLLParser.ml"
            )), _startpos_l_), (op : 'tv_assign_binop)), _, (e : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2912 "var/cll/CLLParser.ml"
            ))) = _menhir_stack in
            let _startpos = _startpos_l_ in
            let _v : (
# 55 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 2918 "var/cll/CLLParser.ml"
            ) = 
# 228 "var/cll/CLLParser.mly"
    (
      BinopAssign(l, op, e)
    )
# 2924 "var/cll/CLLParser.ml"
             in
            _menhir_goto_assign _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv412)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv413 * _menhir_state * (
# 53 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2934 "var/cll/CLLParser.ml"
            ) * Lexing.position) * 'tv_assign_binop) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2938 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv414)) : 'freshtv416)
    | MenhirState65 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv423 * _menhir_state * Lexing.position)) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2947 "var/cll/CLLParser.ml"
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
            let (_menhir_stack : (('freshtv417 * _menhir_state * Lexing.position)) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 2981 "var/cll/CLLParser.ml"
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
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState96) : 'freshtv418)
        | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv419 * _menhir_state * Lexing.position)) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3019 "var/cll/CLLParser.ml"
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
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState78) : 'freshtv420)
        | SUB ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv421 * _menhir_state * Lexing.position)) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3041 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv422)) : 'freshtv424)
    | MenhirState99 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv431 * _menhir_state * Lexing.position)) * _menhir_state * (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3050 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3054 "var/cll/CLLParser.ml"
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
            let (_menhir_stack : (((('freshtv425 * _menhir_state * Lexing.position)) * _menhir_state * (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3088 "var/cll/CLLParser.ml"
            ))) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3092 "var/cll/CLLParser.ml"
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
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState105) : 'freshtv426)
        | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv427 * _menhir_state * Lexing.position)) * _menhir_state * (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3130 "var/cll/CLLParser.ml"
            ))) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3134 "var/cll/CLLParser.ml"
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
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState101) : 'freshtv428)
        | SUB ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv429 * _menhir_state * Lexing.position)) * _menhir_state * (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3156 "var/cll/CLLParser.ml"
            ))) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3160 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv430)) : 'freshtv432)
    | _ ->
        _menhir_fail ()

and _menhir_goto_block : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 56 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3170 "var/cll/CLLParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState81 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv251 * _menhir_state * Lexing.position)) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3180 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3184 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 56 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3188 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv249 * _menhir_state * Lexing.position)) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3194 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3198 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 56 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3202 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((((_menhir_stack, _menhir_s, _startpos__1_), _, (_3 : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3207 "var/cll/CLLParser.ml"
        ))), _, (_5 : (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3211 "var/cll/CLLParser.ml"
        ))), _, (_7 : (
# 56 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3215 "var/cll/CLLParser.ml"
        ))) = _menhir_stack in
        let _6 = () in
        let _4 = () in
        let _2 = () in
        let _1 = () in
        let _startpos = _startpos__1_ in
        let _v : (
# 57 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3225 "var/cll/CLLParser.ml"
        ) = let _startpos = _startpos__1_ in
        
# 294 "var/cll/CLLParser.mly"
    (
      raise_syntax_error _startpos "Ill-formed 'for' loop"
    )
# 3232 "var/cll/CLLParser.ml"
         in
        _menhir_goto_control _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv250)) : 'freshtv252)
    | MenhirState96 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv255 * _menhir_state * Lexing.position)) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3240 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 56 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3244 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv253 * _menhir_state * Lexing.position)) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3250 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 56 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3254 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s, _startpos__1_), _, (_3 : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3259 "var/cll/CLLParser.ml"
        ))), _, (_5 : (
# 56 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3263 "var/cll/CLLParser.ml"
        ))) = _menhir_stack in
        let _4 = () in
        let _2 = () in
        let _1 = () in
        let _startpos = _startpos__1_ in
        let _v : (
# 57 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3272 "var/cll/CLLParser.ml"
        ) = let _startpos = _startpos__1_ in
        
# 299 "var/cll/CLLParser.mly"
    (
      raise_syntax_error _startpos "Ill-formed 'for' loop; You may want to use a 'while' loop instead."
    )
# 3279 "var/cll/CLLParser.ml"
         in
        _menhir_goto_control _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv254)) : 'freshtv256)
    | MenhirState103 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((((('freshtv259 * _menhir_state * Lexing.position)) * _menhir_state * (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3287 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3291 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3295 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 56 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3299 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((((('freshtv257 * _menhir_state * Lexing.position)) * _menhir_state * (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3305 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3309 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3313 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 56 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3317 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (((((_menhir_stack, _menhir_s, _startpos__1_), _, (init : (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3322 "var/cll/CLLParser.ml"
        ))), _, (cond : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3326 "var/cll/CLLParser.ml"
        ))), _, (it : (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3330 "var/cll/CLLParser.ml"
        ))), _, (b : (
# 56 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3334 "var/cll/CLLParser.ml"
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
# 3345 "var/cll/CLLParser.ml"
        ) = 
# 281 "var/cll/CLLParser.mly"
    (
      Cycle.from_elt (For(init, cond, it, b))
    )
# 3351 "var/cll/CLLParser.ml"
         in
        _menhir_goto_control _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv258)) : 'freshtv260)
    | MenhirState105 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv263 * _menhir_state * Lexing.position)) * _menhir_state * (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3359 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3363 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 56 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3367 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv261 * _menhir_state * Lexing.position)) * _menhir_state * (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3373 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3377 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 56 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3381 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((((_menhir_stack, _menhir_s, _startpos__1_), _, (_3 : (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3386 "var/cll/CLLParser.ml"
        ))), _, (_5 : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3390 "var/cll/CLLParser.ml"
        ))), _, (_7 : (
# 56 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3394 "var/cll/CLLParser.ml"
        ))) = _menhir_stack in
        let _6 = () in
        let _4 = () in
        let _2 = () in
        let _1 = () in
        let _startpos = _startpos__1_ in
        let _v : (
# 57 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3404 "var/cll/CLLParser.ml"
        ) = let _startpos = _startpos__1_ in
        
# 294 "var/cll/CLLParser.mly"
    (
      raise_syntax_error _startpos "Ill-formed 'for' loop"
    )
# 3411 "var/cll/CLLParser.ml"
         in
        _menhir_goto_control _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv262)) : 'freshtv264)
    | MenhirState108 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv267 * _menhir_state * Lexing.position)) * _menhir_state * (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3419 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3423 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 56 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3427 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv265 * _menhir_state * Lexing.position)) * _menhir_state * (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3433 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3437 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 56 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3441 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((((_menhir_stack, _menhir_s, _startpos__1_), _, (_3 : (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3446 "var/cll/CLLParser.ml"
        ))), _, (_5 : (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3450 "var/cll/CLLParser.ml"
        ))), _, (_7 : (
# 56 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3454 "var/cll/CLLParser.ml"
        ))) = _menhir_stack in
        let _6 = () in
        let _4 = () in
        let _2 = () in
        let _1 = () in
        let _startpos = _startpos__1_ in
        let _v : (
# 57 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3464 "var/cll/CLLParser.ml"
        ) = let _startpos = _startpos__1_ in
        
# 294 "var/cll/CLLParser.mly"
    (
      raise_syntax_error _startpos "Ill-formed 'for' loop"
    )
# 3471 "var/cll/CLLParser.ml"
         in
        _menhir_goto_control _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv266)) : 'freshtv268)
    | MenhirState110 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv271 * _menhir_state * Lexing.position)) * _menhir_state * (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3479 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 56 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3483 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv269 * _menhir_state * Lexing.position)) * _menhir_state * (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3489 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 56 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3493 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s, _startpos__1_), _, (_3 : (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3498 "var/cll/CLLParser.ml"
        ))), _, (_5 : (
# 56 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3502 "var/cll/CLLParser.ml"
        ))) = _menhir_stack in
        let _4 = () in
        let _2 = () in
        let _1 = () in
        let _startpos = _startpos__1_ in
        let _v : (
# 57 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3511 "var/cll/CLLParser.ml"
        ) = let _startpos = _startpos__1_ in
        
# 294 "var/cll/CLLParser.mly"
    (
      raise_syntax_error _startpos "Ill-formed 'for' loop"
    )
# 3518 "var/cll/CLLParser.ml"
         in
        _menhir_goto_control _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv270)) : 'freshtv272)
    | MenhirState63 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv279 * _menhir_state * Lexing.position)) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3526 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 56 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3530 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ELSE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv273 * _menhir_state * Lexing.position)) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3540 "var/cll/CLLParser.ml"
            ))) * _menhir_state * (
# 56 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3544 "var/cll/CLLParser.ml"
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
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState113) : 'freshtv274)
        | BREAK | COLON | CONTINUE | EXIT | FOR | IF | LABEL _ | MULT | NOP | PRINT | RB | RETURN | SEMI | WHILE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv275 * _menhir_state * Lexing.position)) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3582 "var/cll/CLLParser.ml"
            ))) * _menhir_state * (
# 56 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3586 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, _startpos__1_), _, (e : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3591 "var/cll/CLLParser.ml"
            ))), _, (b : (
# 56 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3595 "var/cll/CLLParser.ml"
            ))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _v : (
# 57 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3604 "var/cll/CLLParser.ml"
            ) = 
# 266 "var/cll/CLLParser.mly"
    (
      Cycle.from_elt (CLLTree.If(e, b))
    )
# 3610 "var/cll/CLLParser.ml"
             in
            _menhir_goto_control _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv276)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv277 * _menhir_state * Lexing.position)) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3620 "var/cll/CLLParser.ml"
            ))) * _menhir_state * (
# 56 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3624 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv278)) : 'freshtv280)
    | MenhirState113 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv283 * _menhir_state * Lexing.position)) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3633 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 56 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3637 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 56 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3641 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv281 * _menhir_state * Lexing.position)) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3647 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 56 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3651 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 56 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3655 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((((_menhir_stack, _menhir_s, _startpos__1_), _, (c : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3660 "var/cll/CLLParser.ml"
        ))), _, (t : (
# 56 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3664 "var/cll/CLLParser.ml"
        ))), _, (e : (
# 56 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3668 "var/cll/CLLParser.ml"
        ))) = _menhir_stack in
        let _6 = () in
        let _4 = () in
        let _2 = () in
        let _1 = () in
        let _startpos = _startpos__1_ in
        let _v : (
# 57 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3678 "var/cll/CLLParser.ml"
        ) = 
# 271 "var/cll/CLLParser.mly"
    ( 
      Cycle.from_elt (CLLTree.IfElse(c, t, e))
    )
# 3684 "var/cll/CLLParser.ml"
         in
        _menhir_goto_control _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv282)) : 'freshtv284)
    | MenhirState58 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv287 * _menhir_state * Lexing.position) * _menhir_state) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3692 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 56 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3696 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv285 * _menhir_state * Lexing.position) * _menhir_state) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3702 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 56 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3706 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((((_menhir_stack, _menhir_s, _startpos__1_), _), _, (e : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 3711 "var/cll/CLLParser.ml"
        ))), _, (b : (
# 56 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3715 "var/cll/CLLParser.ml"
        ))) = _menhir_stack in
        let _4 = () in
        let _2 = () in
        let _1 = () in
        let _startpos = _startpos__1_ in
        let _v : (
# 57 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3724 "var/cll/CLLParser.ml"
        ) = 
# 276 "var/cll/CLLParser.mly"
    (
      Cycle.from_elt (CLLTree.While(e, b))
    )
# 3730 "var/cll/CLLParser.ml"
         in
        _menhir_goto_control _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv286)) : 'freshtv288)
    | MenhirState6 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv291 * _menhir_state * Lexing.position) * _menhir_state * (
# 56 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3738 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv289 * _menhir_state * Lexing.position) * _menhir_state * (
# 56 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3744 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _startpos__1_), _, (_2 : (
# 56 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3749 "var/cll/CLLParser.ml"
        ))) = _menhir_stack in
        let _1 = () in
        let _startpos = _startpos__1_ in
        let _v : (
# 57 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3756 "var/cll/CLLParser.ml"
        ) = let _startpos = _startpos__1_ in
        
# 286 "var/cll/CLLParser.mly"
    (
      raise_syntax_error _startpos "No condition found for 'while'"
    )
# 3763 "var/cll/CLLParser.ml"
         in
        _menhir_goto_control _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv290)) : 'freshtv292)
    | _ ->
        _menhir_fail ()

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_reduce26 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (
# 59 "var/cll/CLLParser.mly"
      (ARTTree.datas ARTTree.compiler_type)
# 3779 "var/cll/CLLParser.ml"
    ) = 
# 324 "var/cll/CLLParser.mly"
    ( {syntax_tree = Cycle.empty_cycle; tag_set = empty} )
# 3783 "var/cll/CLLParser.ml"
     in
    _menhir_goto_data_declarations _menhir_env _menhir_stack _menhir_s _v

and _menhir_run137 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 46 "var/cll/CLLParser.mly"
       (string)
# 3790 "var/cll/CLLParser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COLON ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv245 * _menhir_state * (
# 46 "var/cll/CLLParser.mly"
       (string)
# 3802 "var/cll/CLLParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | INT _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv241 * _menhir_state * (
# 46 "var/cll/CLLParser.mly"
       (string)
# 3812 "var/cll/CLLParser.ml"
            ) * Lexing.position)) = Obj.magic _menhir_stack in
            let (_v : (
# 45 "var/cll/CLLParser.mly"
       (int)
# 3817 "var/cll/CLLParser.ml"
            )) = _v in
            ((let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LABEL _v ->
                _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState139 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EOF ->
                _menhir_reduce26 _menhir_env (Obj.magic _menhir_stack) MenhirState139
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState139) : 'freshtv242)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv243 * _menhir_state * (
# 46 "var/cll/CLLParser.mly"
       (string)
# 3838 "var/cll/CLLParser.ml"
            ) * Lexing.position)) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv244)) : 'freshtv246)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv247 * _menhir_state * (
# 46 "var/cll/CLLParser.mly"
       (string)
# 3849 "var/cll/CLLParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv248)

and _menhir_goto_instructions : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 51 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3857 "var/cll/CLLParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState59 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv213 * _menhir_state) * _menhir_state * (
# 51 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3867 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RB ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv209 * _menhir_state) * _menhir_state * (
# 51 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3877 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv207 * _menhir_state) * _menhir_state * (
# 51 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3885 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            let (_endpos__3_ : Lexing.position) = _endpos in
            ((let ((_menhir_stack, _menhir_s), _, (i : (
# 51 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3891 "var/cll/CLLParser.ml"
            ))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (
# 56 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3898 "var/cll/CLLParser.ml"
            ) = 
# 261 "var/cll/CLLParser.mly"
    ( i )
# 3902 "var/cll/CLLParser.ml"
             in
            _menhir_goto_block _menhir_env _menhir_stack _menhir_s _v) : 'freshtv208)) : 'freshtv210)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv211 * _menhir_state) * _menhir_state * (
# 51 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3912 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv212)) : 'freshtv214)
    | MenhirState118 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv217 * _menhir_state * (
# 54 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 3921 "var/cll/CLLParser.ml"
        ) * Lexing.position)) * _menhir_state * (
# 51 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3925 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv215 * _menhir_state * (
# 54 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 3931 "var/cll/CLLParser.ml"
        ) * Lexing.position)) * _menhir_state * (
# 51 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3935 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (i : (
# 54 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 3940 "var/cll/CLLParser.ml"
        )), _startpos_i_), _, (s : (
# 51 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3944 "var/cll/CLLParser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : (
# 51 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3950 "var/cll/CLLParser.ml"
        ) = 
# 124 "var/cll/CLLParser.mly"
    (
      Cycle.prepend s i
    )
# 3956 "var/cll/CLLParser.ml"
         in
        _menhir_goto_instructions _menhir_env _menhir_stack _menhir_s _v) : 'freshtv216)) : 'freshtv218)
    | MenhirState121 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv221 * _menhir_state * (
# 57 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3964 "var/cll/CLLParser.ml"
        ) * Lexing.position) * _menhir_state) * _menhir_state * (
# 51 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3968 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv219 * _menhir_state * (
# 57 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3974 "var/cll/CLLParser.ml"
        ) * Lexing.position) * _menhir_state) * _menhir_state * (
# 51 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3978 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s, (c : (
# 57 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3983 "var/cll/CLLParser.ml"
        )), _startpos_c_), _), _, (s : (
# 51 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3987 "var/cll/CLLParser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : (
# 51 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 3993 "var/cll/CLLParser.ml"
        ) = 
# 130 "var/cll/CLLParser.mly"
    (
      Cycle.extend c s
    )
# 3999 "var/cll/CLLParser.ml"
         in
        _menhir_goto_instructions _menhir_env _menhir_stack _menhir_s _v) : 'freshtv220)) : 'freshtv222)
    | MenhirState123 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv225 * _menhir_state * (
# 57 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 4007 "var/cll/CLLParser.ml"
        ) * Lexing.position) * _menhir_state) * _menhir_state * (
# 51 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 4011 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv223 * _menhir_state * (
# 57 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 4017 "var/cll/CLLParser.ml"
        ) * Lexing.position) * _menhir_state) * _menhir_state * (
# 51 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 4021 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s, (_1 : (
# 57 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 4026 "var/cll/CLLParser.ml"
        )), _startpos__1_), _), _, (_3 : (
# 51 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 4030 "var/cll/CLLParser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : (
# 51 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 4036 "var/cll/CLLParser.ml"
        ) = let _startpos = _startpos__1_ in
        
# 136 "var/cll/CLLParser.mly"
    (
      raise_syntax_error _startpos "Expected ';', found ':'."
    )
# 4043 "var/cll/CLLParser.ml"
         in
        _menhir_goto_instructions _menhir_env _menhir_stack _menhir_s _v) : 'freshtv224)) : 'freshtv226)
    | MenhirState120 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv229 * _menhir_state * (
# 57 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 4051 "var/cll/CLLParser.ml"
        ) * Lexing.position) * _menhir_state * (
# 51 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 4055 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv227 * _menhir_state * (
# 57 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 4061 "var/cll/CLLParser.ml"
        ) * Lexing.position) * _menhir_state * (
# 51 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 4065 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (c : (
# 57 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 4070 "var/cll/CLLParser.ml"
        )), _startpos_c_), _, (s : (
# 51 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 4074 "var/cll/CLLParser.ml"
        ))) = _menhir_stack in
        let _v : (
# 51 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 4079 "var/cll/CLLParser.ml"
        ) = 
# 130 "var/cll/CLLParser.mly"
    (
      Cycle.extend c s
    )
# 4085 "var/cll/CLLParser.ml"
         in
        _menhir_goto_instructions _menhir_env _menhir_stack _menhir_s _v) : 'freshtv228)) : 'freshtv230)
    | MenhirState126 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv233 * _menhir_state * (
# 54 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 4093 "var/cll/CLLParser.ml"
        ) * Lexing.position)) * _menhir_state * (
# 51 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 4097 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv231 * _menhir_state * (
# 54 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 4103 "var/cll/CLLParser.ml"
        ) * Lexing.position)) * _menhir_state * (
# 51 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 4107 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (_1 : (
# 54 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 4112 "var/cll/CLLParser.ml"
        )), _startpos__1_), _, (_3 : (
# 51 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 4116 "var/cll/CLLParser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : (
# 51 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 4122 "var/cll/CLLParser.ml"
        ) = let _startpos = _startpos__1_ in
        
# 136 "var/cll/CLLParser.mly"
    (
      raise_syntax_error _startpos "Expected ';', found ':'."
    )
# 4129 "var/cll/CLLParser.ml"
         in
        _menhir_goto_instructions _menhir_env _menhir_stack _menhir_s _v) : 'freshtv232)) : 'freshtv234)
    | MenhirState5 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv239 * _menhir_state * (
# 46 "var/cll/CLLParser.mly"
       (string)
# 4137 "var/cll/CLLParser.ml"
        ) * Lexing.position)))) * _menhir_state * (
# 51 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 4141 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RB ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv235 * _menhir_state * (
# 46 "var/cll/CLLParser.mly"
       (string)
# 4151 "var/cll/CLLParser.ml"
            ) * Lexing.position)))) * _menhir_state * (
# 51 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 4155 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_stack = (_menhir_stack, _endpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LABEL _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState131 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | DATA | EOF ->
                _menhir_reduce65 _menhir_env (Obj.magic _menhir_stack) MenhirState131
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState131) : 'freshtv236)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv237 * _menhir_state * (
# 46 "var/cll/CLLParser.mly"
       (string)
# 4177 "var/cll/CLLParser.ml"
            ) * Lexing.position)))) * _menhir_state * (
# 51 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 4181 "var/cll/CLLParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv238)) : 'freshtv240)
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
        _menhir_reduce61 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState59

and _menhir_goto_l_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 53 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 4226 "var/cll/CLLParser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState75 | MenhirState61 | MenhirState56 | MenhirState9 | MenhirState10 | MenhirState11 | MenhirState12 | MenhirState48 | MenhirState46 | MenhirState44 | MenhirState42 | MenhirState40 | MenhirState38 | MenhirState36 | MenhirState34 | MenhirState32 | MenhirState29 | MenhirState27 | MenhirState25 | MenhirState23 | MenhirState13 | MenhirState16 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv185 * _menhir_state * (
# 53 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 4236 "var/cll/CLLParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        (_menhir_reduce29 _menhir_env (Obj.magic _menhir_stack) : 'freshtv186)
    | MenhirState99 | MenhirState65 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv189 * _menhir_state * (
# 53 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 4244 "var/cll/CLLParser.ml"
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
            _menhir_reduce29 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv187 * _menhir_state * (
# 53 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 4272 "var/cll/CLLParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv188)) : 'freshtv190)
    | MenhirState101 | MenhirState94 | MenhirState78 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv193 * _menhir_state * (
# 53 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 4281 "var/cll/CLLParser.ml"
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
            let (_menhir_stack : 'freshtv191 * _menhir_state * (
# 53 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 4307 "var/cll/CLLParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv192)) : 'freshtv194)
    | MenhirState5 | MenhirState6 | MenhirState58 | MenhirState126 | MenhirState120 | MenhirState123 | MenhirState121 | MenhirState118 | MenhirState59 | MenhirState113 | MenhirState63 | MenhirState110 | MenhirState108 | MenhirState105 | MenhirState103 | MenhirState96 | MenhirState81 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv205 * _menhir_state * (
# 53 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 4316 "var/cll/CLLParser.ml"
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
            let (_menhir_stack : 'freshtv201 * _menhir_state * (
# 53 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 4336 "var/cll/CLLParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | RP ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv197 * _menhir_state * (
# 53 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 4346 "var/cll/CLLParser.ml"
                ) * Lexing.position)) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv195 * _menhir_state * (
# 53 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 4353 "var/cll/CLLParser.ml"
                ) * Lexing.position)) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, (l_e : (
# 53 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 4358 "var/cll/CLLParser.ml"
                )), _startpos_l_e_) = _menhir_stack in
                let _3 = () in
                let _2 = () in
                let _startpos = _startpos_l_e_ in
                let _v : (
# 54 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 4366 "var/cll/CLLParser.ml"
                ) = 
# 218 "var/cll/CLLParser.mly"
    (
      Call l_e
    )
# 4372 "var/cll/CLLParser.ml"
                 in
                _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv196)) : 'freshtv198)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv199 * _menhir_state * (
# 53 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 4382 "var/cll/CLLParser.ml"
                ) * Lexing.position)) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv200)) : 'freshtv202)
        | MULTASSIGN ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | SUBASSIGN ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv203 * _menhir_state * (
# 53 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 4397 "var/cll/CLLParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv204)) : 'freshtv206)
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
# 4494 "var/cll/CLLParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv183) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((i : (
# 45 "var/cll/CLLParser.mly"
       (int)
# 4504 "var/cll/CLLParser.ml"
    )) : (
# 45 "var/cll/CLLParser.mly"
       (int)
# 4508 "var/cll/CLLParser.ml"
    )) = _v in
    ((let _v : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 4513 "var/cll/CLLParser.ml"
    ) = 
# 145 "var/cll/CLLParser.mly"
    ( Int i )
# 4517 "var/cll/CLLParser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv184)

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
# 4553 "var/cll/CLLParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv181) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((b : (
# 44 "var/cll/CLLParser.mly"
       (bool)
# 4563 "var/cll/CLLParser.ml"
    )) : (
# 44 "var/cll/CLLParser.mly"
       (bool)
# 4567 "var/cll/CLLParser.ml"
    )) = _v in
    ((let _v : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 4572 "var/cll/CLLParser.ml"
    ) = 
# 147 "var/cll/CLLParser.mly"
    ( Bool b )
# 4576 "var/cll/CLLParser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv182)

and _menhir_run18 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LABEL _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv177 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        let (_v : (
# 46 "var/cll/CLLParser.mly"
       (string)
# 4592 "var/cll/CLLParser.ml"
        )) = _v in
        let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv175 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        let ((t : (
# 46 "var/cll/CLLParser.mly"
       (string)
# 4601 "var/cll/CLLParser.ml"
        )) : (
# 46 "var/cll/CLLParser.mly"
       (string)
# 4605 "var/cll/CLLParser.ml"
        )) = _v in
        let (_startpos_t_ : Lexing.position) = _startpos in
        ((let (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
        let _1 = () in
        let _v : (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 4613 "var/cll/CLLParser.ml"
        ) = let _startpos = _startpos__1_ in
        
# 185 "var/cll/CLLParser.mly"
    ( Address (make_node _startpos t) )
# 4618 "var/cll/CLLParser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv176)) : 'freshtv178)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv179 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv180)

and _menhir_goto_instruction : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 54 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 4632 "var/cll/CLLParser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState6 | MenhirState58 | MenhirState113 | MenhirState63 | MenhirState110 | MenhirState108 | MenhirState105 | MenhirState103 | MenhirState96 | MenhirState81 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv165 * _menhir_state * (
# 54 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 4642 "var/cll/CLLParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv161 * _menhir_state * (
# 54 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 4652 "var/cll/CLLParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv159 * _menhir_state * (
# 54 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 4659 "var/cll/CLLParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (i : (
# 54 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 4664 "var/cll/CLLParser.ml"
            )), _startpos_i_) = _menhir_stack in
            let _2 = () in
            let _v : (
# 56 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 4670 "var/cll/CLLParser.ml"
            ) = 
# 253 "var/cll/CLLParser.mly"
    (
      Cycle.from_elt i
    )
# 4676 "var/cll/CLLParser.ml"
             in
            _menhir_goto_block _menhir_env _menhir_stack _menhir_s _v) : 'freshtv160)) : 'freshtv162)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv163 * _menhir_state * (
# 54 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 4686 "var/cll/CLLParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv164)) : 'freshtv166)
    | MenhirState5 | MenhirState126 | MenhirState120 | MenhirState123 | MenhirState121 | MenhirState118 | MenhirState59 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv173 * _menhir_state * (
# 54 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 4695 "var/cll/CLLParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv167 * _menhir_state * (
# 54 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 4705 "var/cll/CLLParser.ml"
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
                _menhir_reduce61 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState126) : 'freshtv168)
        | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv169 * _menhir_state * (
# 54 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 4743 "var/cll/CLLParser.ml"
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
                _menhir_reduce61 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState118) : 'freshtv170)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv171 * _menhir_state * (
# 54 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 4783 "var/cll/CLLParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv172)) : 'freshtv174)
    | _ ->
        _menhir_fail ()

and _menhir_goto_program : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 50 "var/cll/CLLParser.mly"
      (CLLTree.cll_prog ARTTree.compiler_type)
# 4793 "var/cll/CLLParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv157) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : (
# 50 "var/cll/CLLParser.mly"
      (CLLTree.cll_prog ARTTree.compiler_type)
# 4802 "var/cll/CLLParser.ml"
    )) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv155) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : (
# 50 "var/cll/CLLParser.mly"
      (CLLTree.cll_prog ARTTree.compiler_type)
# 4810 "var/cll/CLLParser.ml"
    )) : (
# 50 "var/cll/CLLParser.mly"
      (CLLTree.cll_prog ARTTree.compiler_type)
# 4814 "var/cll/CLLParser.ml"
    )) = _v in
    (Obj.magic _1 : 'freshtv156)) : 'freshtv158)

and _menhir_goto_procedure_definitions : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 60 "var/cll/CLLParser.mly"
      ((CLLTree.procedure_definition Cycle.cycle) ARTTree.compiler_type)
# 4821 "var/cll/CLLParser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState131 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv143 * _menhir_state * (
# 46 "var/cll/CLLParser.mly"
       (string)
# 4831 "var/cll/CLLParser.ml"
        ) * Lexing.position)))) * _menhir_state * (
# 51 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 4835 "var/cll/CLLParser.ml"
        )) * Lexing.position) * _menhir_state * (
# 60 "var/cll/CLLParser.mly"
      ((CLLTree.procedure_definition Cycle.cycle) ARTTree.compiler_type)
# 4839 "var/cll/CLLParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv141 * _menhir_state * (
# 46 "var/cll/CLLParser.mly"
       (string)
# 4845 "var/cll/CLLParser.ml"
        ) * Lexing.position)))) * _menhir_state * (
# 51 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 4849 "var/cll/CLLParser.ml"
        )) * Lexing.position) * _menhir_state * (
# 60 "var/cll/CLLParser.mly"
      ((CLLTree.procedure_definition Cycle.cycle) ARTTree.compiler_type)
# 4853 "var/cll/CLLParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((((_menhir_stack, _menhir_s, (name : (
# 46 "var/cll/CLLParser.mly"
       (string)
# 4858 "var/cll/CLLParser.ml"
        )), _startpos_name_), _, (b : (
# 51 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 4862 "var/cll/CLLParser.ml"
        ))), _endpos__6_), _, (s : (
# 60 "var/cll/CLLParser.mly"
      ((CLLTree.procedure_definition Cycle.cycle) ARTTree.compiler_type)
# 4866 "var/cll/CLLParser.ml"
        )), _startpos_s_) = _menhir_stack in
        let _6 = () in
        let _4 = () in
        let _3 = () in
        let _2 = () in
        let _startpos = _startpos_name_ in
        let _v : (
# 60 "var/cll/CLLParser.mly"
      ((CLLTree.procedure_definition Cycle.cycle) ARTTree.compiler_type)
# 4876 "var/cll/CLLParser.ml"
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
# 4892 "var/cll/CLLParser.ml"
         in
        _menhir_goto_procedure_definitions _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv142)) : 'freshtv144)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv153 * _menhir_state * (
# 60 "var/cll/CLLParser.mly"
      ((CLLTree.procedure_definition Cycle.cycle) ARTTree.compiler_type)
# 4900 "var/cll/CLLParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DATA ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv145 * _menhir_state * (
# 60 "var/cll/CLLParser.mly"
      ((CLLTree.procedure_definition Cycle.cycle) ARTTree.compiler_type)
# 4910 "var/cll/CLLParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LABEL _v ->
                _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EOF ->
                _menhir_reduce26 _menhir_env (Obj.magic _menhir_stack) MenhirState136
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState136) : 'freshtv146)
        | EOF ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv149 * _menhir_state * (
# 60 "var/cll/CLLParser.mly"
      ((CLLTree.procedure_definition Cycle.cycle) ARTTree.compiler_type)
# 4928 "var/cll/CLLParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv147 * _menhir_state * (
# 60 "var/cll/CLLParser.mly"
      ((CLLTree.procedure_definition Cycle.cycle) ARTTree.compiler_type)
# 4934 "var/cll/CLLParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (globals : (
# 60 "var/cll/CLLParser.mly"
      ((CLLTree.procedure_definition Cycle.cycle) ARTTree.compiler_type)
# 4939 "var/cll/CLLParser.ml"
            )), _startpos_globals_) = _menhir_stack in
            let _2 = () in
            let _v : (
# 50 "var/cll/CLLParser.mly"
      (CLLTree.cll_prog ARTTree.compiler_type)
# 4945 "var/cll/CLLParser.ml"
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
# 4958 "var/cll/CLLParser.ml"
             in
            _menhir_goto_program _menhir_env _menhir_stack _menhir_s _v) : 'freshtv148)) : 'freshtv150)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv151 * _menhir_state * (
# 60 "var/cll/CLLParser.mly"
      ((CLLTree.procedure_definition Cycle.cycle) ARTTree.compiler_type)
# 4968 "var/cll/CLLParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv152)) : 'freshtv154)
    | _ ->
        _menhir_fail ()

and _menhir_reduce61 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (
# 51 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 4980 "var/cll/CLLParser.ml"
    ) = 
# 140 "var/cll/CLLParser.mly"
    ( Cycle.empty_cycle )
# 4984 "var/cll/CLLParser.ml"
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
        let (_menhir_stack : 'freshtv139 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
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
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState56) : 'freshtv140)
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
    let (_menhir_stack : 'freshtv137) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_startpos__1_ : Lexing.position) = _startpos in
    ((let _1 = () in
    let _startpos = _startpos__1_ in
    let _v : (
# 54 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 5065 "var/cll/CLLParser.ml"
    ) = 
# 213 "var/cll/CLLParser.mly"
    (
      Return
    )
# 5071 "var/cll/CLLParser.ml"
     in
    _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv138)

and _menhir_run8 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LP ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv133 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
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
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState9) : 'freshtv134)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv135 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv136)

and _menhir_run55 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv131) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_startpos__1_ : Lexing.position) = _startpos in
    ((let _1 = () in
    let _startpos = _startpos__1_ in
    let _v : (
# 54 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 5129 "var/cll/CLLParser.ml"
    ) = 
# 197 "var/cll/CLLParser.mly"
    ( Nop )
# 5133 "var/cll/CLLParser.ml"
     in
    _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv132)

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
# 5169 "var/cll/CLLParser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv129) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((t : (
# 46 "var/cll/CLLParser.mly"
       (string)
# 5179 "var/cll/CLLParser.ml"
    )) : (
# 46 "var/cll/CLLParser.mly"
       (string)
# 5183 "var/cll/CLLParser.ml"
    )) = _v in
    let (_startpos_t_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos_t_ in
    let _v : (
# 53 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 5190 "var/cll/CLLParser.ml"
    ) = let _startpos = _startpos_t_ in
    
# 190 "var/cll/CLLParser.mly"
    ( Id (make_node _startpos t) )
# 5195 "var/cll/CLLParser.ml"
     in
    _menhir_goto_l_expr _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv130)

and _menhir_run60 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
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
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState61) : 'freshtv126)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv127 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv128)

and _menhir_run64 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LP ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv121 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
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
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState65) : 'freshtv122)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv123 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv124)

and _menhir_run82 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
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
# 5295 "var/cll/CLLParser.ml"
    ) = 
# 199 "var/cll/CLLParser.mly"
    ( Exit )
# 5299 "var/cll/CLLParser.ml"
     in
    _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv120)

and _menhir_run83 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
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
# 5315 "var/cll/CLLParser.ml"
    ) = let _startpos = _startpos__1_ in
    
# 208 "var/cll/CLLParser.mly"
    ( 
      Continue (make_node _startpos ())
    )
# 5322 "var/cll/CLLParser.ml"
     in
    _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv118)

and _menhir_run84 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv115) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_startpos__1_ : Lexing.position) = _startpos in
    ((let _1 = () in
    let _startpos = _startpos__1_ in
    let _v : (
# 54 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 5338 "var/cll/CLLParser.ml"
    ) = let _startpos = _startpos__1_ in
    
# 203 "var/cll/CLLParser.mly"
    ( 
      Break (make_node _startpos ())
    )
# 5345 "var/cll/CLLParser.ml"
     in
    _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv116)

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState139 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv15 * _menhir_state * (
# 46 "var/cll/CLLParser.mly"
       (string)
# 5357 "var/cll/CLLParser.ml"
        ) * Lexing.position)) * (
# 45 "var/cll/CLLParser.mly"
       (int)
# 5361 "var/cll/CLLParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv16)
    | MenhirState136 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv17 * _menhir_state * (
# 60 "var/cll/CLLParser.mly"
      ((CLLTree.procedure_definition Cycle.cycle) ARTTree.compiler_type)
# 5370 "var/cll/CLLParser.ml"
        ) * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv18)
    | MenhirState131 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv19 * _menhir_state * (
# 46 "var/cll/CLLParser.mly"
       (string)
# 5379 "var/cll/CLLParser.ml"
        ) * Lexing.position)))) * _menhir_state * (
# 51 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 5383 "var/cll/CLLParser.ml"
        )) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv20)
    | MenhirState126 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv21 * _menhir_state * (
# 54 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 5392 "var/cll/CLLParser.ml"
        ) * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv22)
    | MenhirState123 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv23 * _menhir_state * (
# 57 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 5401 "var/cll/CLLParser.ml"
        ) * Lexing.position) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv24)
    | MenhirState121 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv25 * _menhir_state * (
# 57 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 5410 "var/cll/CLLParser.ml"
        ) * Lexing.position) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv26)
    | MenhirState120 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv27 * _menhir_state * (
# 57 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 5419 "var/cll/CLLParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv28)
    | MenhirState118 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv29 * _menhir_state * (
# 54 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 5428 "var/cll/CLLParser.ml"
        ) * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv30)
    | MenhirState113 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv31 * _menhir_state * Lexing.position)) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 5437 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 56 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 5441 "var/cll/CLLParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv32)
    | MenhirState110 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv33 * _menhir_state * Lexing.position)) * _menhir_state * (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 5450 "var/cll/CLLParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv34)
    | MenhirState108 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv35 * _menhir_state * Lexing.position)) * _menhir_state * (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 5459 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 5463 "var/cll/CLLParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv36)
    | MenhirState105 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv37 * _menhir_state * Lexing.position)) * _menhir_state * (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 5472 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 5476 "var/cll/CLLParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv38)
    | MenhirState103 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((('freshtv39 * _menhir_state * Lexing.position)) * _menhir_state * (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 5485 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 5489 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 5493 "var/cll/CLLParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv40)
    | MenhirState101 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv41 * _menhir_state * Lexing.position)) * _menhir_state * (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 5502 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 5506 "var/cll/CLLParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv42)
    | MenhirState99 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv43 * _menhir_state * Lexing.position)) * _menhir_state * (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 5515 "var/cll/CLLParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv44)
    | MenhirState96 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv45 * _menhir_state * Lexing.position)) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 5524 "var/cll/CLLParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv46)
    | MenhirState94 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv47 * _menhir_state * (
# 55 "var/cll/CLLParser.mly"
      (CLLTree.cll_instr)
# 5533 "var/cll/CLLParser.ml"
        ) * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv48)
    | MenhirState81 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv49 * _menhir_state * Lexing.position)) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 5542 "var/cll/CLLParser.ml"
        ))) * _menhir_state * (
# 58 "var/cll/CLLParser.mly"
      (CLLTree.cll_instrs)
# 5546 "var/cll/CLLParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv50)
    | MenhirState78 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv51 * _menhir_state * Lexing.position)) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 5555 "var/cll/CLLParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv52)
    | MenhirState75 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv53 * _menhir_state * (
# 53 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 5564 "var/cll/CLLParser.ml"
        ) * Lexing.position) * 'tv_assign_binop) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv54)
    | MenhirState65 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv55 * _menhir_state * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv56)
    | MenhirState63 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv57 * _menhir_state * Lexing.position)) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 5578 "var/cll/CLLParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv58)
    | MenhirState61 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv59 * _menhir_state * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv60)
    | MenhirState59 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv61 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv62)
    | MenhirState58 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv63 * _menhir_state * Lexing.position) * _menhir_state) * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 5597 "var/cll/CLLParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv64)
    | MenhirState56 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv65 * _menhir_state * Lexing.position) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv66)
    | MenhirState48 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv67 * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 5611 "var/cll/CLLParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv68)
    | MenhirState46 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv69 * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 5620 "var/cll/CLLParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv70)
    | MenhirState44 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv71 * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 5629 "var/cll/CLLParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv72)
    | MenhirState42 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv73 * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 5638 "var/cll/CLLParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv74)
    | MenhirState40 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv75 * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 5647 "var/cll/CLLParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv76)
    | MenhirState38 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv77 * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 5656 "var/cll/CLLParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv78)
    | MenhirState36 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv79 * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 5665 "var/cll/CLLParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv80)
    | MenhirState34 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv81 * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 5674 "var/cll/CLLParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv82)
    | MenhirState32 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv83 * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 5683 "var/cll/CLLParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv84)
    | MenhirState29 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv85 * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 5692 "var/cll/CLLParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv86)
    | MenhirState27 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv87 * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 5701 "var/cll/CLLParser.ml"
        )) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv88)
    | MenhirState25 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv89 * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 5710 "var/cll/CLLParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv90)
    | MenhirState23 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv91 * _menhir_state * (
# 52 "var/cll/CLLParser.mly"
      (ARTTree.expression)
# 5719 "var/cll/CLLParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv92)
    | MenhirState16 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv93 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv94)
    | MenhirState13 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv95 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv96)
    | MenhirState12 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv97 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv98)
    | MenhirState11 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv99 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv100)
    | MenhirState10 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv101 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv102)
    | MenhirState9 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv103 * _menhir_state * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv104)
    | MenhirState6 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv105 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv106)
    | MenhirState5 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv107 * _menhir_state * (
# 46 "var/cll/CLLParser.mly"
       (string)
# 5763 "var/cll/CLLParser.ml"
        ) * Lexing.position)))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv108)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv113) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv111) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState0 in
        let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv109) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_startpos__1_ : Lexing.position) = _startpos in
        ((let _1 = () in
        let _v : (
# 50 "var/cll/CLLParser.mly"
      (CLLTree.cll_prog ARTTree.compiler_type)
# 5782 "var/cll/CLLParser.ml"
        ) = let _startpos = _startpos__1_ in
        
# 96 "var/cll/CLLParser.mly"
    ( 
      raise_syntax_error _startpos "CLL program structure: list procedure declaration .data <declarations>" 
    )
# 5789 "var/cll/CLLParser.ml"
         in
        _menhir_goto_program _menhir_env _menhir_stack _menhir_s _v) : 'freshtv110)) : 'freshtv112)) : 'freshtv114)

and _menhir_reduce65 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let (_, _startpos) = Obj.magic _menhir_stack in
    let _v : (
# 60 "var/cll/CLLParser.mly"
      ((CLLTree.procedure_definition Cycle.cycle) ARTTree.compiler_type)
# 5799 "var/cll/CLLParser.ml"
    ) = 
# 117 "var/cll/CLLParser.mly"
  (
    {syntax_tree = Cycle.empty_cycle; tag_set = Tagset.empty}
  )
# 5805 "var/cll/CLLParser.ml"
     in
    _menhir_goto_procedure_definitions _menhir_env _menhir_stack _menhir_s _v _startpos

and _menhir_run2 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 46 "var/cll/CLLParser.mly"
       (string)
# 5812 "var/cll/CLLParser.ml"
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
# 5824 "var/cll/CLLParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RP ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv7 * _menhir_state * (
# 46 "var/cll/CLLParser.mly"
       (string)
# 5834 "var/cll/CLLParser.ml"
            ) * Lexing.position)) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LB ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv3 * _menhir_state * (
# 46 "var/cll/CLLParser.mly"
       (string)
# 5844 "var/cll/CLLParser.ml"
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
                    _menhir_reduce61 _menhir_env (Obj.magic _menhir_stack) MenhirState5
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
# 5884 "var/cll/CLLParser.ml"
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
# 5895 "var/cll/CLLParser.ml"
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
# 5906 "var/cll/CLLParser.ml"
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
# 5926 "var/cll/CLLParser.ml"
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
        _menhir_reduce65 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0) : 'freshtv2))

# 219 "/usr/share/menhir/standard.mly"
  


# 5958 "var/cll/CLLParser.ml"
