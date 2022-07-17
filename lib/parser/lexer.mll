(* Lexer *)

(* defines [next_line] utility function;
   SyntaxError exception;
   sets up environment
*)
{
  open Lexing
  open Parse

  exception SyntaxError of string

  let next_line lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_bol = lexbuf.lex_curr_pos;
                 pos_lnum = pos.pos_lnum + 1
      }
}

(* convenient REs *)
let digits = ['+' '-']?['0'-'9']+
let var = ['a'-'z']+

let white = [' ' '\t']
let newline = '\r' | '\n' | "\r\n"

(* lexing rules *)
rule read =
  parse
  | white   { read lexbuf }
  | newline { next_line lexbuf; read lexbuf }
  | digits  { DIGITS (int_of_string (Lexing.lexeme lexbuf)) }
  | "bool"  { BOOL }
  | "int"   { INT }
  | "true"  { TRUE }
  | "false" { FALSE }
  | var     { VAR (Lexing.lexeme lexbuf) }
  | "|-"    { TURNSTILE }
  | "|/-"   { NTURNSTILE }
  | "->"    { ARROW }
  | '*'     { STAR }
  | ':'     { COLON }
  | ','     { COMMA }
  | '('     { LPAREN }
  | ')'     { RPAREN }
  | '.'     { DOT }
  | '\\'    { LAM }
  | "Pi"    { PI }
  | eof     { EOF }
  | _       { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
