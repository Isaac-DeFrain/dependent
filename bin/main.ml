open Core
open Lexing
open Parsing.Lexer
open Parsing.Parser

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf
    outx
    "%s:%d:%d"
    pos.pos_fname
    pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  try check_derivation read lexbuf with
  | SyntaxError msg ->
      fprintf stderr "%a: %s\n" print_position lexbuf msg ;
      None
  | Error ->
      fprintf stderr "%a: syntax error\n" print_position lexbuf ;
      exit (-1)

(* parse and print formatted code *)
(* let rec parse_and_print lexbuf =
  match parse_with_error lexbuf with
  | Some value ->
      printf "%a\n" Ast.print_tiny value ;
      parse_and_print lexbuf
  | None -> () *)

(* parse and print sexp *)
(* let rec parse_and_print_sexp lexbuf =
  match parse_with_error lexbuf with
  | Some value ->
      printf "%a\n" Type.print_sexp value ;
      parse_and_print_sexp lexbuf
  | None -> () *)

(* parse and check derivation *)
let parse_and_check lexbuf =
  match parse_with_error lexbuf with
  | Some value -> string_of_bool value |> print_endline
  | None -> ()

(* handler function *)
let handler f () =
  let open In_channel in
  match f with
  | None -> ()
  | Some filename ->
      let inx = create filename in
      let lexbuf = Lexing.from_channel inx in
      lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename } ;
      parse_and_check lexbuf ;
      close inx

(* cli *)
let () =
  Command.basic_spec
    ~summary:"Parse, display, and prove derivations"
    Command.Spec.(
      empty +> flag "-p" (optional string) ~doc:" Prove the derivation")
    handler
  |> Command.run
