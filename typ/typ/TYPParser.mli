
(* The type of tokens. *)

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
  | LABEL of (string)
  | INT of (int)
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

val program: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (TYPTree.typ_prog TYPTree.program)
