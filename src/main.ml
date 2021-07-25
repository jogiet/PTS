
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
	Format.printf "File \"%s\", line %d, character %d-%d: @ " file l fc lc

let get_syst file syst_opt =
  let from_extension = function
    | ".stlc" -> stlc
    | ".f" -> syst_f
    | ".fw" -> syst_fw
    | ".cc" | ".coc" -> cc
    | ".u" ->
        Format.printf " ⚠ You're using an inconsistent logic system@ ";
        syst_U
    | ext ->
        Format.printf
          "Error: %s is not a valid extension without type system@ "
          (if ext = "" then "\"\"" else ext);
        exit 1
  in
  if !syst_file <> "" then
    let chan = open_in !syst_file in
    let syst =
      chan
      |> Lexing.from_channel
      |> Parser.system Lexer.next_tokens in
    let _ = close_in chan in
    syst
  else if !syst_ext <> "" then
    from_extension ("."^ !syst_ext)
  else if Option.is_some syst_opt then
    Option.get syst_opt
  else
    from_extension (Filename.extension file)

let main x syst =
  let _ = Format.printf "=> term = @   %a@ "
    (pretty_printer_line "@ ") x in
  let _ = flush_all () in
  let _ = if !parse_only then exit 0 in
  let t, tree = type_check syst IdMap.empty [] x in
  let _ = if !get_proof then print_proof syst (Option.get tree) proof_file in
  let _ = Format.printf "=> type : %a@ " pretty_printer t in
  let _ = flush_all () in
  let _ = if !get_metric then
    Format.printf "memoize_success = %i@ " !memoize_success in
  let _ = if !get_metric && !get_proof then
    Format.printf "proof_size = %i@ " (proof_size @@ Option.get tree) in
  let _ = if !type_only then
    begin
    Format.printf "%a" print_all_let Typer.all_let;
    exit 0
    end
  in
  let x = get_nf x in
  let _ = Format.printf "=> normal form : %a@ " pretty_printer x in
  let _ = if !get_metric then
    Format.printf "#reduction steps = %i@ " !steps in
  ()

let _ =
  let _ = Format.printf "@[<v 0>" in
  let lb =
    if !interactive then
      let _ = Format.printf "# PTS: a Pure Type System interpreter #@ " in
      let _ = Format.printf "# ----------------------------------- #@ @." in
      stdin |> Lexing.from_channel
    else
      file |> open_in |> Lexing.from_channel
  in
  try
    let term, syst_opt = (Parser.file Lexer.next_tokens lb) in
    let syst = get_syst file syst_opt in
    let _ = main term syst in
    Format.printf "@]@."

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
