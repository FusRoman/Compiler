
module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | WHEN
    | UNOP of (
# 14 "art/ARTParser.mly"
       (char)
# 12 "art/ARTParser.ml"
  )
    | TWO_POINT
    | TEXT
    | SUP_EQUAL
    | SUP
    | SUB
    | SEMI
    | RP
    | PRINT
    | OR
    | NOT_EQUAL
    | NOP
    | MUL
    | MOD
    | LP
    | LEFT_EXPR_STAR
    | JUMP
    | INT of (
# 9 "art/ARTParser.mly"
       (int)
# 33 "art/ARTParser.ml"
  )
    | INF_EQUAL
    | INF
    | ID of (
# 12 "art/ARTParser.mly"
       (string)
# 40 "art/ARTParser.ml"
  )
    | EXIT
    | EQUAL
    | EOF
    | DIV
    | DATA
    | BOOL of (
# 13 "art/ARTParser.mly"
       (bool)
# 50 "art/ARTParser.ml"
  )
    | AND
    | AFFECT
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
  | MenhirState68
  | MenhirState62
  | MenhirState58
  | MenhirState54
  | MenhirState52
  | MenhirState47
  | MenhirState45
  | MenhirState39
  | MenhirState37
  | MenhirState35
  | MenhirState33
  | MenhirState31
  | MenhirState29
  | MenhirState26
  | MenhirState24
  | MenhirState22
  | MenhirState20
  | MenhirState18
  | MenhirState16
  | MenhirState14
  | MenhirState7
  | MenhirState6
  | MenhirState5
  | MenhirState4
  | MenhirState2

# 1 "art/ARTParser.mly"
  
  open Lexing
  open ART_SyntaxTree

# 102 "art/ARTParser.ml"

let rec _menhir_goto_data_declarations : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 32 "art/ARTParser.mly"
      (ART_SyntaxTree.datas)
# 107 "art/ARTParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState62 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv257) * _menhir_state * (
# 28 "art/ARTParser.mly"
      (ART_SyntaxTree.instrs)
# 117 "art/ARTParser.ml"
        ))) * _menhir_state * (
# 32 "art/ARTParser.mly"
      (ART_SyntaxTree.datas)
# 121 "art/ARTParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EOF ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv253) * _menhir_state * (
# 28 "art/ARTParser.mly"
      (ART_SyntaxTree.instrs)
# 131 "art/ARTParser.ml"
            ))) * _menhir_state * (
# 32 "art/ARTParser.mly"
      (ART_SyntaxTree.datas)
# 135 "art/ARTParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv251) * _menhir_state * (
# 28 "art/ARTParser.mly"
      (ART_SyntaxTree.instrs)
# 141 "art/ARTParser.ml"
            ))) * _menhir_state * (
# 32 "art/ARTParser.mly"
      (ART_SyntaxTree.datas)
# 145 "art/ARTParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _, (text : (
# 28 "art/ARTParser.mly"
      (ART_SyntaxTree.instrs)
# 150 "art/ARTParser.ml"
            ))), _, (data : (
# 32 "art/ARTParser.mly"
      (ART_SyntaxTree.datas)
# 154 "art/ARTParser.ml"
            ))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (
# 27 "art/ARTParser.mly"
      (ART_SyntaxTree.prog)
# 162 "art/ARTParser.ml"
            ) = 
# 42 "art/ARTParser.mly"
    ( Prog_Data (text,data) )
# 166 "art/ARTParser.ml"
             in
            _menhir_goto_source _menhir_env _menhir_stack _v) : 'freshtv252)) : 'freshtv254)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv255) * _menhir_state * (
# 28 "art/ARTParser.mly"
      (ART_SyntaxTree.instrs)
# 176 "art/ARTParser.ml"
            ))) * _menhir_state * (
# 32 "art/ARTParser.mly"
      (ART_SyntaxTree.datas)
# 180 "art/ARTParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv256)) : 'freshtv258)
    | MenhirState68 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv261 * _menhir_state * (
# 33 "art/ARTParser.mly"
      (ART_SyntaxTree.data)
# 189 "art/ARTParser.ml"
        )) * _menhir_state * (
# 32 "art/ARTParser.mly"
      (ART_SyntaxTree.datas)
# 193 "art/ARTParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv259 * _menhir_state * (
# 33 "art/ARTParser.mly"
      (ART_SyntaxTree.data)
# 199 "art/ARTParser.ml"
        )) * _menhir_state * (
# 32 "art/ARTParser.mly"
      (ART_SyntaxTree.datas)
# 203 "art/ARTParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (d : (
# 33 "art/ARTParser.mly"
      (ART_SyntaxTree.data)
# 208 "art/ARTParser.ml"
        ))), _, (ds : (
# 32 "art/ARTParser.mly"
      (ART_SyntaxTree.datas)
# 212 "art/ARTParser.ml"
        ))) = _menhir_stack in
        let _v : (
# 32 "art/ARTParser.mly"
      (ART_SyntaxTree.datas)
# 217 "art/ARTParser.ml"
        ) = 
# 103 "art/ARTParser.mly"
                                          ( Datas (d,ds) )
# 221 "art/ARTParser.ml"
         in
        _menhir_goto_data_declarations _menhir_env _menhir_stack _menhir_s _v) : 'freshtv260)) : 'freshtv262)
    | _ ->
        _menhir_fail ()

and _menhir_run14 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 230 "art/ARTParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
    | ID _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
    | INT _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
    | LEFT_EXPR_STAR ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | LP ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | UNOP _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState14

and _menhir_run26 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 256 "art/ARTParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
    | ID _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
    | INT _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
    | LEFT_EXPR_STAR ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | LP ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | UNOP _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState26

and _menhir_run16 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 282 "art/ARTParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
    | ID _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
    | INT _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
    | LEFT_EXPR_STAR ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | LP ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | UNOP _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState16

and _menhir_run29 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 308 "art/ARTParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | ID _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | INT _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | LEFT_EXPR_STAR ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | LP ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | UNOP _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState29

and _menhir_run31 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 334 "art/ARTParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
    | ID _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
    | INT _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
    | LEFT_EXPR_STAR ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | LP ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | UNOP _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState31

and _menhir_run18 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 360 "art/ARTParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
    | ID _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
    | INT _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
    | LEFT_EXPR_STAR ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | LP ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | UNOP _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState18

and _menhir_run20 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 386 "art/ARTParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
    | ID _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
    | INT _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
    | LEFT_EXPR_STAR ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | LP ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | UNOP _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState20

and _menhir_run33 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 412 "art/ARTParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
    | ID _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
    | INT _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
    | LEFT_EXPR_STAR ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | LP ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | UNOP _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState33

and _menhir_run35 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 438 "art/ARTParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
    | ID _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
    | INT _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
    | LEFT_EXPR_STAR ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | LP ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | UNOP _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState35

and _menhir_run37 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 464 "art/ARTParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | ID _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | INT _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | LEFT_EXPR_STAR ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | LP ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | UNOP _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState37

and _menhir_run22 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 490 "art/ARTParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
    | ID _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
    | INT _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
    | LEFT_EXPR_STAR ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | LP ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | UNOP _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState22

and _menhir_run39 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 516 "art/ARTParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | ID _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | INT _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | LEFT_EXPR_STAR ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | LP ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | UNOP _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState39

and _menhir_run24 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 542 "art/ARTParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v
    | ID _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v
    | INT _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v
    | LEFT_EXPR_STAR ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState24
    | LP ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState24
    | UNOP _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState24

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_reduce3 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (
# 32 "art/ARTParser.mly"
      (ART_SyntaxTree.datas)
# 575 "art/ARTParser.ml"
    ) = 
# 104 "art/ARTParser.mly"
  ( Empty_data )
# 579 "art/ARTParser.ml"
     in
    _menhir_goto_data_declarations _menhir_env _menhir_stack _menhir_s _v

and _menhir_run63 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 12 "art/ARTParser.mly"
       (string)
# 586 "art/ARTParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TWO_POINT ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv247 * _menhir_state * (
# 12 "art/ARTParser.mly"
       (string)
# 598 "art/ARTParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | INT _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv243 * _menhir_state * (
# 12 "art/ARTParser.mly"
       (string)
# 608 "art/ARTParser.ml"
            ))) = Obj.magic _menhir_stack in
            let (_v : (
# 9 "art/ARTParser.mly"
       (int)
# 613 "art/ARTParser.ml"
            )) = _v in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv241 * _menhir_state * (
# 12 "art/ARTParser.mly"
       (string)
# 620 "art/ARTParser.ml"
            ))) = Obj.magic _menhir_stack in
            let ((i : (
# 9 "art/ARTParser.mly"
       (int)
# 625 "art/ARTParser.ml"
            )) : (
# 9 "art/ARTParser.mly"
       (int)
# 629 "art/ARTParser.ml"
            )) = _v in
            ((let (_menhir_stack, _menhir_s, (t : (
# 12 "art/ARTParser.mly"
       (string)
# 634 "art/ARTParser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 33 "art/ARTParser.mly"
      (ART_SyntaxTree.data)
# 640 "art/ARTParser.ml"
            ) = 
# 108 "art/ARTParser.mly"
                       ( Data (t,i) )
# 644 "art/ARTParser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv239) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 33 "art/ARTParser.mly"
      (ART_SyntaxTree.data)
# 652 "art/ARTParser.ml"
            )) = _v in
            ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv237 * _menhir_state * (
# 33 "art/ARTParser.mly"
      (ART_SyntaxTree.data)
# 659 "art/ARTParser.ml"
            )) = Obj.magic _menhir_stack in
            ((assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ID _v ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
            | EOF ->
                _menhir_reduce3 _menhir_env (Obj.magic _menhir_stack) MenhirState68
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState68) : 'freshtv238)) : 'freshtv240)) : 'freshtv242)) : 'freshtv244)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv245 * _menhir_state * (
# 12 "art/ARTParser.mly"
       (string)
# 679 "art/ARTParser.ml"
            ))) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv246)) : 'freshtv248)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv249 * _menhir_state * (
# 12 "art/ARTParser.mly"
       (string)
# 690 "art/ARTParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv250)

and _menhir_goto_expression : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 698 "art/ARTParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState6 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv139 * _menhir_state) * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 708 "art/ARTParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADD ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | INF ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | INF_EQUAL ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | NOT_EQUAL ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | RP ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv135 * _menhir_state) * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 738 "art/ARTParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv133 * _menhir_state) * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 745 "art/ARTParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (e : (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 750 "art/ARTParser.ml"
            ))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 757 "art/ARTParser.ml"
            ) = 
# 86 "art/ARTParser.mly"
                     ( Expr_parenthese e )
# 761 "art/ARTParser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v) : 'freshtv134)) : 'freshtv136)
        | SUB ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | SUP ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | SUP_EQUAL ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv137 * _menhir_state) * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 777 "art/ARTParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv138)) : 'freshtv140)
    | MenhirState14 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv145 * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 786 "art/ARTParser.ml"
        ))) * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 790 "art/ARTParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADD ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | AND | EQUAL | INF | INF_EQUAL | NOT_EQUAL | OR | RP | SEMI | SUP | SUP_EQUAL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv141 * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 810 "art/ARTParser.ml"
            ))) * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 814 "art/ARTParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (e1 : (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 819 "art/ARTParser.ml"
            ))), _, (e2 : (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 823 "art/ARTParser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 829 "art/ARTParser.ml"
            ) = 
# 94 "art/ARTParser.mly"
                                        ( Binop (e1,SUP_EQUAL,e2) )
# 833 "art/ARTParser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v) : 'freshtv142)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv143 * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 843 "art/ARTParser.ml"
            ))) * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 847 "art/ARTParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv144)) : 'freshtv146)
    | MenhirState16 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv151 * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 856 "art/ARTParser.ml"
        ))) * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 860 "art/ARTParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIV ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | ADD | AND | EQUAL | INF | INF_EQUAL | NOT_EQUAL | OR | RP | SEMI | SUB | SUP | SUP_EQUAL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv147 * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 876 "art/ARTParser.ml"
            ))) * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 880 "art/ARTParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (e1 : (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 885 "art/ARTParser.ml"
            ))), _, (e2 : (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 889 "art/ARTParser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 895 "art/ARTParser.ml"
            ) = 
# 88 "art/ARTParser.mly"
                                  ( Binop (e1,SUB,e2) )
# 899 "art/ARTParser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v) : 'freshtv148)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv149 * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 909 "art/ARTParser.ml"
            ))) * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 913 "art/ARTParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv150)) : 'freshtv152)
    | MenhirState18 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv155 * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 922 "art/ARTParser.ml"
        ))) * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 926 "art/ARTParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv153 * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 932 "art/ARTParser.ml"
        ))) * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 936 "art/ARTParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (e1 : (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 941 "art/ARTParser.ml"
        ))), _, (e2 : (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 945 "art/ARTParser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 951 "art/ARTParser.ml"
        ) = 
# 90 "art/ARTParser.mly"
                                  ( Binop (e1,MULT,e2) )
# 955 "art/ARTParser.ml"
         in
        _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v) : 'freshtv154)) : 'freshtv156)
    | MenhirState20 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv159 * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 963 "art/ARTParser.ml"
        ))) * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 967 "art/ARTParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv157 * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 973 "art/ARTParser.ml"
        ))) * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 977 "art/ARTParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (e1 : (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 982 "art/ARTParser.ml"
        ))), _, (e2 : (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 986 "art/ARTParser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 992 "art/ARTParser.ml"
        ) = 
# 95 "art/ARTParser.mly"
                                  ( Binop (e1,REM,e2) )
# 996 "art/ARTParser.ml"
         in
        _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v) : 'freshtv158)) : 'freshtv160)
    | MenhirState22 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv163 * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 1004 "art/ARTParser.ml"
        ))) * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 1008 "art/ARTParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv161 * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 1014 "art/ARTParser.ml"
        ))) * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 1018 "art/ARTParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (e1 : (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 1023 "art/ARTParser.ml"
        ))), _, (e2 : (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 1027 "art/ARTParser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 1033 "art/ARTParser.ml"
        ) = 
# 89 "art/ARTParser.mly"
                                  ( Binop (e1,DIV,e2) )
# 1037 "art/ARTParser.ml"
         in
        _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v) : 'freshtv162)) : 'freshtv164)
    | MenhirState24 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv169 * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 1045 "art/ARTParser.ml"
        ))) * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 1049 "art/ARTParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIV ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | ADD | AND | EQUAL | INF | INF_EQUAL | NOT_EQUAL | OR | RP | SEMI | SUB | SUP | SUP_EQUAL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv165 * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 1065 "art/ARTParser.ml"
            ))) * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 1069 "art/ARTParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (e1 : (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 1074 "art/ARTParser.ml"
            ))), _, (e2 : (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 1078 "art/ARTParser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 1084 "art/ARTParser.ml"
            ) = 
# 87 "art/ARTParser.mly"
                                  ( Binop (e1,ADD,e2) )
# 1088 "art/ARTParser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v) : 'freshtv166)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv167 * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 1098 "art/ARTParser.ml"
            ))) * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 1102 "art/ARTParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv168)) : 'freshtv170)
    | MenhirState26 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv175 * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 1111 "art/ARTParser.ml"
        ))) * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 1115 "art/ARTParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADD ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | AND | EQUAL | INF | INF_EQUAL | NOT_EQUAL | OR | RP | SEMI | SUP | SUP_EQUAL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv171 * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 1135 "art/ARTParser.ml"
            ))) * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 1139 "art/ARTParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (e1 : (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 1144 "art/ARTParser.ml"
            ))), _, (e2 : (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 1148 "art/ARTParser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 1154 "art/ARTParser.ml"
            ) = 
# 93 "art/ARTParser.mly"
                                  ( Binop (e1,SUP,e2) )
# 1158 "art/ARTParser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v) : 'freshtv172)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv173 * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 1168 "art/ARTParser.ml"
            ))) * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 1172 "art/ARTParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv174)) : 'freshtv176)
    | MenhirState29 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv181 * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 1181 "art/ARTParser.ml"
        ))) * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 1185 "art/ARTParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADD ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | INF ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | INF_EQUAL ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | NOT_EQUAL ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | SUP ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | SUP_EQUAL ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | AND | OR | RP | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv177 * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 1217 "art/ARTParser.ml"
            ))) * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 1221 "art/ARTParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (e1 : (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 1226 "art/ARTParser.ml"
            ))), _, (e2 : (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 1230 "art/ARTParser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 1236 "art/ARTParser.ml"
            ) = 
# 99 "art/ARTParser.mly"
                                 ( Binop (e1,OR,e2) )
# 1240 "art/ARTParser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v) : 'freshtv178)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv179 * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 1250 "art/ARTParser.ml"
            ))) * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 1254 "art/ARTParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv180)) : 'freshtv182)
    | MenhirState31 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv187 * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 1263 "art/ARTParser.ml"
        ))) * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 1267 "art/ARTParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADD ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | AND | EQUAL | INF | INF_EQUAL | NOT_EQUAL | OR | RP | SEMI | SUP | SUP_EQUAL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv183 * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 1287 "art/ARTParser.ml"
            ))) * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 1291 "art/ARTParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (e1 : (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 1296 "art/ARTParser.ml"
            ))), _, (e2 : (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 1300 "art/ARTParser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 1306 "art/ARTParser.ml"
            ) = 
# 97 "art/ARTParser.mly"
                                        ( Binop (e1,NOT_EQUAL,e2) )
# 1310 "art/ARTParser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v) : 'freshtv184)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv185 * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 1320 "art/ARTParser.ml"
            ))) * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 1324 "art/ARTParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv186)) : 'freshtv188)
    | MenhirState33 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv193 * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 1333 "art/ARTParser.ml"
        ))) * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 1337 "art/ARTParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADD ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | AND | EQUAL | INF | INF_EQUAL | NOT_EQUAL | OR | RP | SEMI | SUP | SUP_EQUAL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv189 * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 1357 "art/ARTParser.ml"
            ))) * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 1361 "art/ARTParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (e1 : (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 1366 "art/ARTParser.ml"
            ))), _, (e2 : (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 1370 "art/ARTParser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 1376 "art/ARTParser.ml"
            ) = 
# 92 "art/ARTParser.mly"
                                        ( Binop (e1,INF_EQUAL,e2) )
# 1380 "art/ARTParser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v) : 'freshtv190)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv191 * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 1390 "art/ARTParser.ml"
            ))) * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 1394 "art/ARTParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv192)) : 'freshtv194)
    | MenhirState35 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv199 * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 1403 "art/ARTParser.ml"
        ))) * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 1407 "art/ARTParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADD ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | AND | EQUAL | INF | INF_EQUAL | NOT_EQUAL | OR | RP | SEMI | SUP | SUP_EQUAL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv195 * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 1427 "art/ARTParser.ml"
            ))) * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 1431 "art/ARTParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (e1 : (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 1436 "art/ARTParser.ml"
            ))), _, (e2 : (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 1440 "art/ARTParser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 1446 "art/ARTParser.ml"
            ) = 
# 91 "art/ARTParser.mly"
                                  ( Binop (e1,INF,e2) )
# 1450 "art/ARTParser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v) : 'freshtv196)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv197 * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 1460 "art/ARTParser.ml"
            ))) * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 1464 "art/ARTParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv198)) : 'freshtv200)
    | MenhirState37 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv205 * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 1473 "art/ARTParser.ml"
        ))) * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 1477 "art/ARTParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADD ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | AND | EQUAL | INF | INF_EQUAL | NOT_EQUAL | OR | RP | SEMI | SUP | SUP_EQUAL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv201 * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 1497 "art/ARTParser.ml"
            ))) * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 1501 "art/ARTParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (e1 : (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 1506 "art/ARTParser.ml"
            ))), _, (e2 : (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 1510 "art/ARTParser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 1516 "art/ARTParser.ml"
            ) = 
# 96 "art/ARTParser.mly"
                                    ( Binop (e1,EQUAL,e2) )
# 1520 "art/ARTParser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v) : 'freshtv202)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv203 * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 1530 "art/ARTParser.ml"
            ))) * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 1534 "art/ARTParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv204)) : 'freshtv206)
    | MenhirState39 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv211 * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 1543 "art/ARTParser.ml"
        ))) * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 1547 "art/ARTParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADD ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | INF ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | INF_EQUAL ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | NOT_EQUAL ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | SUP ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | SUP_EQUAL ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | AND | OR | RP | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv207 * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 1579 "art/ARTParser.ml"
            ))) * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 1583 "art/ARTParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (e1 : (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 1588 "art/ARTParser.ml"
            ))), _, (e2 : (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 1592 "art/ARTParser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 1598 "art/ARTParser.ml"
            ) = 
# 98 "art/ARTParser.mly"
                                  ( Binop (e1,AND,e2) )
# 1602 "art/ARTParser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v) : 'freshtv208)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv209 * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 1612 "art/ARTParser.ml"
            ))) * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 1616 "art/ARTParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv210)) : 'freshtv212)
    | MenhirState5 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv215 * _menhir_state * (
# 14 "art/ARTParser.mly"
       (char)
# 1625 "art/ARTParser.ml"
        )) * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 1629 "art/ARTParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv213 * _menhir_state * (
# 14 "art/ARTParser.mly"
       (char)
# 1635 "art/ARTParser.ml"
        )) * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 1639 "art/ARTParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (op : (
# 14 "art/ARTParser.mly"
       (char)
# 1644 "art/ARTParser.ml"
        ))), _, (e : (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 1648 "art/ARTParser.ml"
        ))) = _menhir_stack in
        let _v : (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 1653 "art/ARTParser.ml"
        ) = 
# 78 "art/ARTParser.mly"
                       (
  if op = '-' then 
    Unop (MINUS,e)
  else if op = '!' then 
    Unop (NOT,e)
  else
    raise ( Failure ( Printf.sprintf "Unknown unary operator '%c'" op))
)
# 1664 "art/ARTParser.ml"
         in
        _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v) : 'freshtv214)) : 'freshtv216)
    | MenhirState4 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv223 * _menhir_state)) * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 1672 "art/ARTParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADD ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | INF ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | INF_EQUAL ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | NOT_EQUAL ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | RP ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv219 * _menhir_state)) * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 1702 "art/ARTParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv217 * _menhir_state)) * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 1709 "art/ARTParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (e : (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 1714 "art/ARTParser.ml"
            ))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (
# 29 "art/ARTParser.mly"
      (ART_SyntaxTree.instr)
# 1722 "art/ARTParser.ml"
            ) = 
# 61 "art/ARTParser.mly"
                           ( Print e )
# 1726 "art/ARTParser.ml"
             in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v) : 'freshtv218)) : 'freshtv220)
        | SUB ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | SUP ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | SUP_EQUAL ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv221 * _menhir_state)) * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 1742 "art/ARTParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv222)) : 'freshtv224)
    | MenhirState47 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv229 * _menhir_state) * _menhir_state * (
# 30 "art/ARTParser.mly"
      (ART_SyntaxTree.l_expr)
# 1751 "art/ARTParser.ml"
        ))) * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 1755 "art/ARTParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADD ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | INF ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | INF_EQUAL ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | NOT_EQUAL ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | SUP ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | SUP_EQUAL ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv225 * _menhir_state) * _menhir_state * (
# 30 "art/ARTParser.mly"
      (ART_SyntaxTree.l_expr)
# 1791 "art/ARTParser.ml"
            ))) * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 1795 "art/ARTParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s), _, (l_e : (
# 30 "art/ARTParser.mly"
      (ART_SyntaxTree.l_expr)
# 1800 "art/ARTParser.ml"
            ))), _, (e : (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 1804 "art/ARTParser.ml"
            ))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (
# 29 "art/ARTParser.mly"
      (ART_SyntaxTree.instr)
# 1811 "art/ARTParser.ml"
            ) = 
# 65 "art/ARTParser.mly"
                                       ( JumpWhen (l_e,e) )
# 1815 "art/ARTParser.ml"
             in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v) : 'freshtv226)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv227 * _menhir_state) * _menhir_state * (
# 30 "art/ARTParser.mly"
      (ART_SyntaxTree.l_expr)
# 1825 "art/ARTParser.ml"
            ))) * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 1829 "art/ARTParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv228)) : 'freshtv230)
    | MenhirState54 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv235 * _menhir_state * (
# 30 "art/ARTParser.mly"
      (ART_SyntaxTree.l_expr)
# 1838 "art/ARTParser.ml"
        ))) * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 1842 "art/ARTParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADD ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | INF ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | INF_EQUAL ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | NOT_EQUAL ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | SUP ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | SUP_EQUAL ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv231 * _menhir_state * (
# 30 "art/ARTParser.mly"
      (ART_SyntaxTree.l_expr)
# 1878 "art/ARTParser.ml"
            ))) * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 1882 "art/ARTParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (l_e : (
# 30 "art/ARTParser.mly"
      (ART_SyntaxTree.l_expr)
# 1887 "art/ARTParser.ml"
            ))), _, (e : (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 1891 "art/ARTParser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 29 "art/ARTParser.mly"
      (ART_SyntaxTree.instr)
# 1897 "art/ARTParser.ml"
            ) = 
# 66 "art/ARTParser.mly"
                                    ( Affect (l_e,e) )
# 1901 "art/ARTParser.ml"
             in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v) : 'freshtv232)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv233 * _menhir_state * (
# 30 "art/ARTParser.mly"
      (ART_SyntaxTree.l_expr)
# 1911 "art/ARTParser.ml"
            ))) * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 1915 "art/ARTParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv234)) : 'freshtv236)
    | _ ->
        _menhir_fail ()

and _menhir_goto_l_express : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 30 "art/ARTParser.mly"
      (ART_SyntaxTree.l_expr)
# 1925 "art/ARTParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState7 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv113 * _menhir_state) * _menhir_state * (
# 30 "art/ARTParser.mly"
      (ART_SyntaxTree.l_expr)
# 1935 "art/ARTParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv111 * _menhir_state) * _menhir_state * (
# 30 "art/ARTParser.mly"
      (ART_SyntaxTree.l_expr)
# 1941 "art/ARTParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _, (e : (
# 30 "art/ARTParser.mly"
      (ART_SyntaxTree.l_expr)
# 1946 "art/ARTParser.ml"
        ))) = _menhir_stack in
        let _1 = () in
        let _v : (
# 30 "art/ARTParser.mly"
      (ART_SyntaxTree.l_expr)
# 1952 "art/ARTParser.ml"
        ) = 
# 71 "art/ARTParser.mly"
                             ( L_star e )
# 1956 "art/ARTParser.ml"
         in
        _menhir_goto_l_express _menhir_env _menhir_stack _menhir_s _v) : 'freshtv112)) : 'freshtv114)
    | MenhirState54 | MenhirState47 | MenhirState4 | MenhirState5 | MenhirState39 | MenhirState37 | MenhirState35 | MenhirState33 | MenhirState31 | MenhirState29 | MenhirState26 | MenhirState24 | MenhirState22 | MenhirState20 | MenhirState18 | MenhirState16 | MenhirState14 | MenhirState6 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv117 * _menhir_state * (
# 30 "art/ARTParser.mly"
      (ART_SyntaxTree.l_expr)
# 1964 "art/ARTParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv115 * _menhir_state * (
# 30 "art/ARTParser.mly"
      (ART_SyntaxTree.l_expr)
# 1970 "art/ARTParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (l_e : (
# 30 "art/ARTParser.mly"
      (ART_SyntaxTree.l_expr)
# 1975 "art/ARTParser.ml"
        ))) = _menhir_stack in
        let _v : (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 1980 "art/ARTParser.ml"
        ) = 
# 76 "art/ARTParser.mly"
                ( L_expr l_e )
# 1984 "art/ARTParser.ml"
         in
        _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v) : 'freshtv116)) : 'freshtv118)
    | MenhirState45 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv125 * _menhir_state) * _menhir_state * (
# 30 "art/ARTParser.mly"
      (ART_SyntaxTree.l_expr)
# 1992 "art/ARTParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | WHEN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv119 * _menhir_state) * _menhir_state * (
# 30 "art/ARTParser.mly"
      (ART_SyntaxTree.l_expr)
# 2002 "art/ARTParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BOOL _v ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
            | ID _v ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
            | INT _v ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
            | LEFT_EXPR_STAR ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState47
            | LP ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState47
            | UNOP _v ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState47) : 'freshtv120)
        | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv121 * _menhir_state) * _menhir_state * (
# 30 "art/ARTParser.mly"
      (ART_SyntaxTree.l_expr)
# 2028 "art/ARTParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (l_e : (
# 30 "art/ARTParser.mly"
      (ART_SyntaxTree.l_expr)
# 2033 "art/ARTParser.ml"
            ))) = _menhir_stack in
            let _1 = () in
            let _v : (
# 29 "art/ARTParser.mly"
      (ART_SyntaxTree.instr)
# 2039 "art/ARTParser.ml"
            ) = 
# 64 "art/ARTParser.mly"
                     ( Jump l_e )
# 2043 "art/ARTParser.ml"
             in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v) : 'freshtv122)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv123 * _menhir_state) * _menhir_state * (
# 30 "art/ARTParser.mly"
      (ART_SyntaxTree.l_expr)
# 2053 "art/ARTParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv124)) : 'freshtv126)
    | MenhirState2 | MenhirState58 | MenhirState52 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv131 * _menhir_state * (
# 30 "art/ARTParser.mly"
      (ART_SyntaxTree.l_expr)
# 2062 "art/ARTParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AFFECT ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv127 * _menhir_state * (
# 30 "art/ARTParser.mly"
      (ART_SyntaxTree.l_expr)
# 2072 "art/ARTParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BOOL _v ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
            | ID _v ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
            | INT _v ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
            | LEFT_EXPR_STAR ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState54
            | LP ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState54
            | UNOP _v ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState54) : 'freshtv128)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv129 * _menhir_state * (
# 30 "art/ARTParser.mly"
      (ART_SyntaxTree.l_expr)
# 2100 "art/ARTParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv130)) : 'freshtv132)
    | _ ->
        _menhir_fail ()

and _menhir_goto_source : _menhir_env -> 'ttv_tail -> (
# 27 "art/ARTParser.mly"
      (ART_SyntaxTree.prog)
# 2110 "art/ARTParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv109) = Obj.magic _menhir_stack in
    let (_v : (
# 27 "art/ARTParser.mly"
      (ART_SyntaxTree.prog)
# 2118 "art/ARTParser.ml"
    )) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv107) = Obj.magic _menhir_stack in
    let ((_1 : (
# 27 "art/ARTParser.mly"
      (ART_SyntaxTree.prog)
# 2125 "art/ARTParser.ml"
    )) : (
# 27 "art/ARTParser.mly"
      (ART_SyntaxTree.prog)
# 2129 "art/ARTParser.ml"
    )) = _v in
    (Obj.magic _1 : 'freshtv108)) : 'freshtv110)

and _menhir_goto_instructions : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 28 "art/ARTParser.mly"
      (ART_SyntaxTree.instrs)
# 2136 "art/ARTParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState52 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv91 * _menhir_state * (
# 34 "art/ARTParser.mly"
      (ART_SyntaxTree.tag)
# 2146 "art/ARTParser.ml"
        ))) * _menhir_state * (
# 28 "art/ARTParser.mly"
      (ART_SyntaxTree.instrs)
# 2150 "art/ARTParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv89 * _menhir_state * (
# 34 "art/ARTParser.mly"
      (ART_SyntaxTree.tag)
# 2156 "art/ARTParser.ml"
        ))) * _menhir_state * (
# 28 "art/ARTParser.mly"
      (ART_SyntaxTree.instrs)
# 2160 "art/ARTParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (t : (
# 34 "art/ARTParser.mly"
      (ART_SyntaxTree.tag)
# 2165 "art/ARTParser.ml"
        ))), _, (is : (
# 28 "art/ARTParser.mly"
      (ART_SyntaxTree.instrs)
# 2169 "art/ARTParser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : (
# 28 "art/ARTParser.mly"
      (ART_SyntaxTree.instrs)
# 2175 "art/ARTParser.ml"
        ) = 
# 53 "art/ARTParser.mly"
                                  ( Instrs_with_tag (t,is) )
# 2179 "art/ARTParser.ml"
         in
        _menhir_goto_instructions _menhir_env _menhir_stack _menhir_s _v) : 'freshtv90)) : 'freshtv92)
    | MenhirState58 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv95 * _menhir_state * (
# 29 "art/ARTParser.mly"
      (ART_SyntaxTree.instr)
# 2187 "art/ARTParser.ml"
        ))) * _menhir_state * (
# 28 "art/ARTParser.mly"
      (ART_SyntaxTree.instrs)
# 2191 "art/ARTParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv93 * _menhir_state * (
# 29 "art/ARTParser.mly"
      (ART_SyntaxTree.instr)
# 2197 "art/ARTParser.ml"
        ))) * _menhir_state * (
# 28 "art/ARTParser.mly"
      (ART_SyntaxTree.instrs)
# 2201 "art/ARTParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (i : (
# 29 "art/ARTParser.mly"
      (ART_SyntaxTree.instr)
# 2206 "art/ARTParser.ml"
        ))), _, (is : (
# 28 "art/ARTParser.mly"
      (ART_SyntaxTree.instrs)
# 2210 "art/ARTParser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : (
# 28 "art/ARTParser.mly"
      (ART_SyntaxTree.instrs)
# 2216 "art/ARTParser.ml"
        ) = 
# 52 "art/ARTParser.mly"
                                     ( Instrs (i,is) )
# 2220 "art/ARTParser.ml"
         in
        _menhir_goto_instructions _menhir_env _menhir_stack _menhir_s _v) : 'freshtv94)) : 'freshtv96)
    | MenhirState2 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv105) * _menhir_state * (
# 28 "art/ARTParser.mly"
      (ART_SyntaxTree.instrs)
# 2228 "art/ARTParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DATA ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv97) * _menhir_state * (
# 28 "art/ARTParser.mly"
      (ART_SyntaxTree.instrs)
# 2238 "art/ARTParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ID _v ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
            | EOF ->
                _menhir_reduce3 _menhir_env (Obj.magic _menhir_stack) MenhirState62
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState62) : 'freshtv98)
        | EOF ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv101) * _menhir_state * (
# 28 "art/ARTParser.mly"
      (ART_SyntaxTree.instrs)
# 2256 "art/ARTParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv99) * _menhir_state * (
# 28 "art/ARTParser.mly"
      (ART_SyntaxTree.instrs)
# 2262 "art/ARTParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, (text : (
# 28 "art/ARTParser.mly"
      (ART_SyntaxTree.instrs)
# 2267 "art/ARTParser.ml"
            ))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (
# 27 "art/ARTParser.mly"
      (ART_SyntaxTree.prog)
# 2274 "art/ARTParser.ml"
            ) = 
# 40 "art/ARTParser.mly"
    ( Prog text )
# 2278 "art/ARTParser.ml"
             in
            _menhir_goto_source _menhir_env _menhir_stack _v) : 'freshtv100)) : 'freshtv102)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv103) * _menhir_state * (
# 28 "art/ARTParser.mly"
      (ART_SyntaxTree.instrs)
# 2288 "art/ARTParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv104)) : 'freshtv106)
    | _ ->
        _menhir_fail ()

and _menhir_run5 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 14 "art/ARTParser.mly"
       (char)
# 2298 "art/ARTParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
    | ID _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
    | INT _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
    | LEFT_EXPR_STAR ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | LP ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | UNOP _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState5

and _menhir_run6 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
    | ID _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
    | INT _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
    | LEFT_EXPR_STAR ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | LP ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | UNOP _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState6

and _menhir_run10 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 9 "art/ARTParser.mly"
       (int)
# 2348 "art/ARTParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv87) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((i : (
# 9 "art/ARTParser.mly"
       (int)
# 2358 "art/ARTParser.ml"
    )) : (
# 9 "art/ARTParser.mly"
       (int)
# 2362 "art/ARTParser.ml"
    )) = _v in
    ((let _v : (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 2367 "art/ARTParser.ml"
    ) = 
# 75 "art/ARTParser.mly"
         ( Int i )
# 2371 "art/ARTParser.ml"
     in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v) : 'freshtv88)

and _menhir_run11 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 13 "art/ARTParser.mly"
       (bool)
# 2378 "art/ARTParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv85) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((b : (
# 13 "art/ARTParser.mly"
       (bool)
# 2388 "art/ARTParser.ml"
    )) : (
# 13 "art/ARTParser.mly"
       (bool)
# 2392 "art/ARTParser.ml"
    )) = _v in
    ((let _v : (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 2397 "art/ARTParser.ml"
    ) = 
# 77 "art/ARTParser.mly"
         ( Bool b )
# 2401 "art/ARTParser.ml"
     in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v) : 'freshtv86)

and _menhir_run8 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 12 "art/ARTParser.mly"
       (string)
# 2408 "art/ARTParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce31 _menhir_env (Obj.magic _menhir_stack)

and _menhir_reduce31 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 12 "art/ARTParser.mly"
       (string)
# 2418 "art/ARTParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (i : (
# 12 "art/ARTParser.mly"
       (string)
# 2424 "art/ARTParser.ml"
    ))) = _menhir_stack in
    let _v : (
# 30 "art/ARTParser.mly"
      (ART_SyntaxTree.l_expr)
# 2429 "art/ARTParser.ml"
    ) = 
# 70 "art/ARTParser.mly"
       ( Id i )
# 2433 "art/ARTParser.ml"
     in
    _menhir_goto_l_express _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_instruction : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 29 "art/ARTParser.mly"
      (ART_SyntaxTree.instr)
# 2440 "art/ARTParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv83 * _menhir_state * (
# 29 "art/ARTParser.mly"
      (ART_SyntaxTree.instr)
# 2448 "art/ARTParser.ml"
    )) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | SEMI ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv79 * _menhir_state * (
# 29 "art/ARTParser.mly"
      (ART_SyntaxTree.instr)
# 2458 "art/ARTParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EXIT ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | ID _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
        | JUMP ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | LEFT_EXPR_STAR ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | NOP ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | PRINT ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | DATA | EOF ->
            _menhir_reduce28 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState58) : 'freshtv80)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv81 * _menhir_state * (
# 29 "art/ARTParser.mly"
      (ART_SyntaxTree.instr)
# 2488 "art/ARTParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv82)) : 'freshtv84)

and _menhir_error0 : _menhir_env -> 'ttv_tail -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv77) = Obj.magic _menhir_stack in
    let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv75) = Obj.magic _menhir_stack in
    let (_startpos__1_ : Lexing.position) = _startpos in
    ((let _1 = () in
    let _v : (
# 27 "art/ARTParser.mly"
      (ART_SyntaxTree.prog)
# 2505 "art/ARTParser.ml"
    ) = let _startpos = _startpos__1_ in
    
# 43 "art/ARTParser.mly"
        ( let pos = _startpos in
          let message =
            Printf.sprintf "Syntax error at %d, %d" pos.pos_lnum (pos.pos_cnum - pos.pos_bol)
          in
          failwith message )
# 2514 "art/ARTParser.ml"
     in
    _menhir_goto_source _menhir_env _menhir_stack _v) : 'freshtv76)) : 'freshtv78)

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState68 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv25 * _menhir_state * (
# 33 "art/ARTParser.mly"
      (ART_SyntaxTree.data)
# 2526 "art/ARTParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv26)
    | MenhirState62 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv27) * _menhir_state * (
# 28 "art/ARTParser.mly"
      (ART_SyntaxTree.instrs)
# 2535 "art/ARTParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv28)
    | MenhirState58 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv29 * _menhir_state * (
# 29 "art/ARTParser.mly"
      (ART_SyntaxTree.instr)
# 2544 "art/ARTParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv30)
    | MenhirState54 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv31 * _menhir_state * (
# 30 "art/ARTParser.mly"
      (ART_SyntaxTree.l_expr)
# 2553 "art/ARTParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv32)
    | MenhirState52 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv33 * _menhir_state * (
# 34 "art/ARTParser.mly"
      (ART_SyntaxTree.tag)
# 2562 "art/ARTParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv34)
    | MenhirState47 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv35 * _menhir_state) * _menhir_state * (
# 30 "art/ARTParser.mly"
      (ART_SyntaxTree.l_expr)
# 2571 "art/ARTParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv36)
    | MenhirState45 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv37 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv38)
    | MenhirState39 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv39 * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 2585 "art/ARTParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv40)
    | MenhirState37 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv41 * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 2594 "art/ARTParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv42)
    | MenhirState35 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv43 * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 2603 "art/ARTParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv44)
    | MenhirState33 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv45 * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 2612 "art/ARTParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv46)
    | MenhirState31 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv47 * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 2621 "art/ARTParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv48)
    | MenhirState29 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv49 * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 2630 "art/ARTParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv50)
    | MenhirState26 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv51 * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 2639 "art/ARTParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv52)
    | MenhirState24 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv53 * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 2648 "art/ARTParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv54)
    | MenhirState22 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv55 * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 2657 "art/ARTParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv56)
    | MenhirState20 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv57 * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 2666 "art/ARTParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv58)
    | MenhirState18 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv59 * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 2675 "art/ARTParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv60)
    | MenhirState16 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv61 * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 2684 "art/ARTParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv62)
    | MenhirState14 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv63 * _menhir_state * (
# 31 "art/ARTParser.mly"
      (ART_SyntaxTree.expression)
# 2693 "art/ARTParser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv64)
    | MenhirState7 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv65 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv66)
    | MenhirState6 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv67 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv68)
    | MenhirState5 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv69 * _menhir_state * (
# 14 "art/ARTParser.mly"
       (char)
# 2712 "art/ARTParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv70)
    | MenhirState4 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv71 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv72)
    | MenhirState2 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv73) = Obj.magic _menhir_stack in
        (_menhir_error0 _menhir_env (Obj.magic _menhir_stack) : 'freshtv74)

and _menhir_reduce28 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (
# 28 "art/ARTParser.mly"
      (ART_SyntaxTree.instrs)
# 2731 "art/ARTParser.ml"
    ) = 
# 51 "art/ARTParser.mly"
              ( Empty_instr )
# 2735 "art/ARTParser.ml"
     in
    _menhir_goto_instructions _menhir_env _menhir_stack _menhir_s _v

and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LP ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv21 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BOOL _v ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
        | ID _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
        | INT _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
        | LEFT_EXPR_STAR ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState4
        | LP ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState4
        | UNOP _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState4) : 'freshtv22)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv23 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv24)

and _menhir_run44 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv19) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : (
# 29 "art/ARTParser.mly"
      (ART_SyntaxTree.instr)
# 2785 "art/ARTParser.ml"
    ) = 
# 62 "art/ARTParser.mly"
      (Nop)
# 2789 "art/ARTParser.ml"
     in
    _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v) : 'freshtv20)

and _menhir_run7 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
    | LEFT_EXPR_STAR ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState7

and _menhir_run45 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | LEFT_EXPR_STAR ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState45

and _menhir_run49 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 12 "art/ARTParser.mly"
       (string)
# 2826 "art/ARTParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TWO_POINT ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv15 * _menhir_state * (
# 12 "art/ARTParser.mly"
       (string)
# 2838 "art/ARTParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (i : (
# 12 "art/ARTParser.mly"
       (string)
# 2843 "art/ARTParser.ml"
        ))) = _menhir_stack in
        let _v : (
# 34 "art/ARTParser.mly"
      (ART_SyntaxTree.tag)
# 2848 "art/ARTParser.ml"
        ) = 
# 57 "art/ARTParser.mly"
      ( Tag (i^":") )
# 2852 "art/ARTParser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv13) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : (
# 34 "art/ARTParser.mly"
      (ART_SyntaxTree.tag)
# 2860 "art/ARTParser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv11 * _menhir_state * (
# 34 "art/ARTParser.mly"
      (ART_SyntaxTree.tag)
# 2867 "art/ARTParser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | TWO_POINT ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv7 * _menhir_state * (
# 34 "art/ARTParser.mly"
      (ART_SyntaxTree.tag)
# 2877 "art/ARTParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | EXIT ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState52
            | ID _v ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
            | JUMP ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState52
            | LEFT_EXPR_STAR ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState52
            | NOP ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState52
            | PRINT ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState52
            | DATA | EOF ->
                _menhir_reduce28 _menhir_env (Obj.magic _menhir_stack) MenhirState52
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState52) : 'freshtv8)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv9 * _menhir_state * (
# 34 "art/ARTParser.mly"
      (ART_SyntaxTree.tag)
# 2907 "art/ARTParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv10)) : 'freshtv12)) : 'freshtv14)) : 'freshtv16)
    | AFFECT ->
        _menhir_reduce31 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv17 * _menhir_state * (
# 12 "art/ARTParser.mly"
       (string)
# 2920 "art/ARTParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv18)

and _menhir_run50 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv5) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : (
# 29 "art/ARTParser.mly"
      (ART_SyntaxTree.instr)
# 2935 "art/ARTParser.ml"
    ) = 
# 63 "art/ARTParser.mly"
       (Exit)
# 2939 "art/ARTParser.ml"
     in
    _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v) : 'freshtv6)

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

and source : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 27 "art/ARTParser.mly"
      (ART_SyntaxTree.prog)
# 2958 "art/ARTParser.ml"
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
    let (_menhir_stack : 'freshtv3) = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    ((let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TEXT ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EXIT ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | ID _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
        | JUMP ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | LEFT_EXPR_STAR ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | NOP ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | PRINT ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | DATA | EOF ->
            _menhir_reduce28 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState2) : 'freshtv2)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_error0 _menhir_env (Obj.magic _menhir_stack)) : 'freshtv4))

# 233 "/home/fusroman/.opam/4.06.0/lib/menhir/standard.mly"
  

# 3009 "art/ARTParser.ml"
