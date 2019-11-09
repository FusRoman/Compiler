
(* The type of tokens. *)

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
  | DIVASSIGN
  | DIV
  | DECR
  | DATA
  | CPL
  | CONTINUE
  | COMMA
  | COLON
  | BREAK
  | BOOL of (bool)
  | ASSIGN
  | AND
  | ADDRESS
  | ADDASSIGN
  | ADD

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val program: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (CLLTree.cll_prog ARTTree.compiler_type)
