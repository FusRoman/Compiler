
(* The type of tokens. *)

type token = 
  | WHEN
  | UNOP of (char)
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
  | INT of (int)
  | INF_EQUAL
  | INF
  | ID of (string)
  | EXIT
  | EQUAL
  | EOF
  | DIV
  | DATA
  | BOOL of (bool)
  | AND
  | AFFECT
  | ADD

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val source: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (ART_SyntaxTree.prog)
