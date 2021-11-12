
(* The type of tokens. *)

type token = 
  | VERTICALBARVERTICALBAR
  | VAR
  | TYPE
  | STRUCT
  | STRING of (string)
  | STAR
  | SLASH
  | SEMICOLON
  | RIGHTPAR
  | RIGHTBRACE
  | RETURN
  | PLUSPLUS
  | PLUS
  | PERCENT
  | PACKAGE
  | NIL
  | MINUSMINUS
  | MINUS
  | LEFTPAR
  | LEFTBRACE
  | IMPORT
  | IF
  | IDENT of (string)
  | FUNC
  | FOR
  | EQ
  | EOF
  | ELSE
  | DOT
  | CST of (Ast.constant)
  | COMP of (Ast.binop)
  | COMMA
  | COLONEQ
  | BANG
  | AMPERSANDAMPERSAND
  | AMP

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val file: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.pfile)
