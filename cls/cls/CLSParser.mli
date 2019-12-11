
(* The type of tokens. *)

type token = 
  | WHILE
  | VAR
  | TYPE
  | TSTRING
  | TINT
  | TFUN
  | SUBASSIGN
  | SUB
  | SEQ
  | SEMI
  | RS
  | RP
  | RETURN
  | REM
  | RB
  | PRINT
  | PIPE
  | OR
  | NSEQ
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
  | LABEL of (string)
  | INT of (int)
  | INCR
  | IF
  | GT
  | GE
  | FOR
  | EXTENDS
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
  | CLASS
  | BREAK
  | BOOL of (bool)
  | ASSIGN
  | ARROW
  | AND
  | ADDRESS
  | ADDASSIGN
  | ADD

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val program: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (CLSTree.cls_prog CLSTree.program)

val header: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (unit)
