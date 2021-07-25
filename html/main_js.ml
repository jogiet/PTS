open Js_of_ocaml
open Lexing
open Ast
open Typer
open Reduc

let current_system = ref Ast.stlc

let report (b,e) =
  let _ = Js.Unsafe.set Js.Unsafe.global "compute_status" 1 in
	let l = b.pos_lnum in
	let fc = b.pos_cnum - b.pos_bol + 1 in
	let lc = e.pos_cnum - b.pos_bol + 1 in
	Format.sprintf "line %d, caracter %d-%d: \n" l fc lc

let set_system content system =
  let system = Js.to_string system in
  let lb = Lexing.from_string system in
  try 
    let _ = match Js.to_string content with
      | "STLC" -> current_system := Ast.stlc
      | "l*"   -> current_system := Ast.lamb_star
      | "F"    -> current_system := Ast.syst_f
      | "Fw"   -> current_system := Ast.syst_fw
      | "CC"   -> current_system := Ast.cc
      | "HOL"  -> current_system := Ast.syst_HOL
      | "U-"   -> current_system := Ast.syst_U_minus
      | "U"    -> current_system := Ast.syst_U
      | "Custom" ->
          let syst = Parser.system Lexer.next_tokens lb in
          current_system := syst
      | _ -> assert false
    in
    let _ = Js.Unsafe.set Js.Unsafe.global "compute_status" 0 in
    Js.string @@ Printer.(generic_to_string print_typing_syst !current_system)
  with
    | Ast.Not_functionnal s ->
      Js.string @@ Format.sprintf "%sThe system is not functionnal %s \n%s"
	      (report (lexeme_start_p lb , lexeme_end_p lb))
        s
        system
    | Lexer.Lexing_error s ->
      Js.string @@ Format.sprintf "%slexical error %s \n%s"
	      (report (lexeme_start_p lb , lexeme_end_p lb))
        s
        system
    | Parser.Error ->
	    Js.string @@ Format.sprintf "%sSyntax error \n%s"
	      (report (lexeme_start_p lb , lexeme_end_p lb))
        system

let typer s =
  let _ = Options.type_only := true in
  let _ = Queue.clear Typer.all_let in
  let s = Js.to_string s in
  let _ = Printf.printf "%s\n" s in
  let lb = Lexing.from_string s in
  try
    let term, _ = Parser.file Lexer.next_tokens lb in
    let typ, _ = type_check !current_system IdMap.empty [] term in
    let _ = Js.Unsafe.set Js.Unsafe.global "compute_status" 0 in
    let _ = Format.fprintf Format.str_formatter "@[<v 0>=> type : %a@ %a@]"
      pretty_printer typ
      Printer.print_all_let Typer.all_let in
    Js.string @@ Format.flush_str_formatter ()
  with
    | Lexer.Lexing_error s ->
      Js.string @@ Format.sprintf "%slexical error %s \n"
	      (report (lexeme_start_p lb , lexeme_end_p lb))
        s
    | Parser.Error ->
	    Js.string @@ Format.sprintf "%sSyntax error \n"
	      (report (lexeme_start_p lb , lexeme_end_p lb))
    | Type_error (msg, pos) ->
      Js.string @@ Format.sprintf "%sType Error: %s"
        (report pos) msg
    
let interpret s =
  let _ = Options.type_only := false in
  let s = Js.to_string s in
  let lb = Lexing.from_string s in
  try
    let term, _ = Parser.file Lexer.next_tokens lb in
    let typ, _ = type_check !current_system IdMap.empty [] term in
    let nf = get_nf term in
    let _ = Js.Unsafe.set Js.Unsafe.global "compute_status" 0 in
    let _ = Format.fprintf Format.str_formatter
      "@[<v 0>=> type        : %a@ => normal form : %a@]@."
          pretty_printer typ
          pretty_printer nf in
    Js.string @@ Format.flush_str_formatter ()
  with
    | Lexer.Lexing_error s ->
      Js.string @@ Format.sprintf "%slexical error %s \n"
	      (report (lexeme_start_p lb , lexeme_end_p lb))
        s
    | Parser.Error ->
	    Js.string @@ Format.sprintf "%sSyntax error \n"
	      (report (lexeme_start_p lb , lexeme_end_p lb))
    | Type_error (msg, pos) ->
      Js.string @@ Format.sprintf "%sType Error: %s"
        (report pos) msg

let () = Js.Unsafe.set Js.Unsafe.global "typer" (Js.wrap_callback typer)
let () = Js.Unsafe.set Js.Unsafe.global "interpret" (Js.wrap_callback interpret)
let () = Js.Unsafe.set Js.Unsafe.global "set_system"
  (Js.wrap_callback set_system)
