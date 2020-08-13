
open Lexing
open Ast
open Printer
open Reduc
open Typer
open Options

let file, proof_file = files ()

let report (b,e) =
	let l = b.pos_lnum in
	let fc = b.pos_cnum - b.pos_bol + 1 in
	let lc = e.pos_cnum - b.pos_bol + 1 in
	Format.printf "File \"%s\", line %d, coracter %d-%d: \n" file l fc lc

let syst_of_filename file =
  match Filename.extension file with
  | ".stlc" -> stlc
  | ".f" -> syst_f
  | ".fw" -> syst_fw
  | ".cc" -> cc
  | ".u" ->
      Format.printf "/!\\ You're using an inconsistent logic system\n";
      syst_U
  | ext ->
      Format.printf 
        "Error: %s is not valid extension without type system\n"
        ext;
      exit 1

let main x syst =
  let _ = Format.printf "term = %a\n" pretty_printer x in
  let _ = flush_all () in
  let _ = if !parse_only then exit 0 in
  let t, tree = type_check syst IdMap.empty [] x in
  let _ = if !get_proof then print_proof syst (Option.get tree) proof_file in
  let _ = Format.printf "=> type : %a\n" pretty_printer t in
  let _ = flush_all () in
  let _ = if !get_metric && !get_proof then
    Format.printf "proof_size = %i\n" (proof_size @@ Option.get tree) in
  let _ = if !type_only then
    begin
    Format.printf "%a" print_all_let Typer.all_let; 
    exit 0 
    end
  in
  let x = get_nf x in
  let _ = Format.printf "=> normal form : %a\n" pretty_printer x in
  let _ = if !get_metric then
    Format.printf "#reduction steps = %i\n" !steps in
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
	Format.printf "lexical error %s \n" s;
	exit 1
  | Parser.Error ->
	report (lexeme_start_p lb , lexeme_end_p lb);
	Format.printf "Syntax error \n";
  exit 1
  | Type_error (msg, pos) ->
  report pos;
  Format.printf "Type Error: %s" msg;
  exit 1
