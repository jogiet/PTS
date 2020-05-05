


{
	open Lexing
	open Parser

	exception Lexing_error of string

	let newline lexbuf =
		let pos = lexbuf.lex_curr_p in
		lexbuf.lex_curr_p <-
			{pos with pos_lnum = pos.pos_lnum +1;
			 pos_bol = pos.pos_cnum }

  let find_keyword s =
    let _ = Printf.printf "lexing %s\n" s in
    match s with
    | "Π" -> FORALL
    | "λ" -> LAMBDA
    | _ -> IDENT s

  let handle_escaped s =
    let _ = Printf.printf "lexing %s\n" s in
    let simple = String.(sub s 1 (length s -1)) in
    match simple with
    | "l" | "lamb" | "lambda" -> LAMBDA
    | "alpha"     -> IDENT "α"
    | "beta"      -> IDENT "β"
    | "gamma"     -> IDENT "γ"
    | "delta"     -> IDENT "δ"
    | "epsilon"   -> IDENT "ε"
    | "zeta"      -> IDENT "ζ"
    | "eta"       -> IDENT "η"
    | "theta"     -> IDENT "θ"
    | "iota"      -> IDENT "ι"
    | "kappa"     -> IDENT "κ"
    | "mu"        -> IDENT "μ"
    | "nu"        -> IDENT "ν"
    | "xi"        -> IDENT "ξ"
    | "omicron"   -> IDENT "ο"
    | "pi"        -> IDENT "π"
    | "rho"       -> IDENT "ρ"
    | "sigma"     -> IDENT "σ"
    | "tau"       -> IDENT "τ"
    | "upsilon"   -> IDENT "υ"
    | "phi"       -> IDENT "φ"
    | "chi"       -> IDENT "χ"
    | "psi"       -> IDENT "ψ"
    | "omega"     -> IDENT "ω"
    | "P" | "forall" -> FORALL
    | _ -> raise (Lexing_error ("Illegal escaped sequence : "^s))
    
}

let var  = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
(* Works only for utf-8 unicode ! *)
let greek = ('\xce'['\x91'-'\xbf']) | ('\xcf'['\x80'-'\x89'])

let ident = (var|digit|greek|'_')+
let escaped = '\x5c' ident

let space = ' '|'\t'

rule next_tokens = parse
	| space+
		{next_tokens lexbuf}
	| '.' {let _ = Printf.printf "lexing .\n" in DOT}
  | ':' {let _ = Printf.printf "lexing :\n" in DDOT}
	| '(' {let _ = Printf.printf "lexing (\n" in LPAR}
	| eof {EOF}
	| ')' {let _ = Printf.printf "lexing )\n" in RPAR}
  | '*' {let _ = Printf.printf "lexing *\n" in IDENT "*"}
  | "∀" {let _ = Printf.printf "lexing ∀\n" in FORALL}
  | "->" {ARROW}
  | "→"  {ARROW}
  | "□" {IDENT "□"} 
	| ident as s {find_keyword s}
  | escaped as s {handle_escaped s}
	|'\n' {newline lexbuf; next_tokens lexbuf}
	| "--" {comment lexbuf}
	| _ as c
		{raise (Lexing_error ("illegal character : "^ String.make 1 c))}

and comment = parse
	| '\n' {newline lexbuf; next_tokens lexbuf}
	| eof {EOF}
	| _ {comment lexbuf}
