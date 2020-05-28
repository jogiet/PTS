
open Lexing
open Parser
open Ast
open Printer
open Reduc
open Typer
open Options

let report (b,e) =
	let l = b.pos_lnum in
	let fc = b.pos_cnum - b.pos_bol + 1 in
	let lc = e.pos_cnum - b.pos_bol + 1 in
	Printf.printf "File \"%s\", line %d, coracter %d-%d: \n" file l fc lc

let syst_of_filename file =
  match Filename.extension file with
  | ".stlc" -> stlc
  | ".f" -> syst_f
  | ".fw" -> syst_fw
  | ".cc" -> cc
  | ext -> 
      Printf.printf 
        "Error: %s is not valid extension without type system\n"
        ext;
      exit 1

let main x syst =
  let _ = Printf.printf "term = %a\n" pretty_printer x in
  let _ = if !parse_only then exit 0 in
  let t, tree = type_check syst IdMap.empty [] x in
  let _ = if !get_proof then print_proof syst tree proof_file in
  let _ = Printf.printf "type = %a\n" pretty_printer t in
  let _ = if !type_only then exit 0 in
  let x = get_nf x in
  let _ = Printf.printf "norm = %a\n" pretty_printer x in
  ()

let _ =
  let chan = open_in file in
  let lb = Lexing.from_channel chan in
  try 
    let term, syst_opt = (Parser.file Lexer.next_tokens lb) in
    let syst = match syst_opt with
      | Some syst -> syst
      | None -> syst_of_filename file in
    main term syst
  with
  | Lexer.Lexing_error s ->
	report (lexeme_start_p lb , lexeme_end_p lb);
	Printf.printf "lexical error %s \n" s;
	exit 1
  | Parser.Error ->
	report (lexeme_start_p lb , lexeme_end_p lb);
	Printf.printf "Syntax error \n";
  exit 1
