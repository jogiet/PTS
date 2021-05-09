

let file_name   = ref ""
and parse_only  = ref false
and type_only   = ref false
and type_debug  = ref false
and reduc_debug = ref false
and get_proof   = ref false
and verb_proof  = ref false
and short_let   = ref false
and proof_file  = ref ""
and inline_def  = ref false
and get_metric  = ref false
and color       = ref false
and no_arrow    = ref false
and no_memoize  = ref false
and interactive = ref false
and syst_file   = ref ""
and syst_ext    = ref ""


let spec =
	["--parse-only", Arg.Set parse_only, " Stop after parsing";
   "--type-only", Arg.Set type_only, " Stop after typing" ;
   "--type-debug", Arg.Set type_debug,
      " Print the intermediate typing judgments" ;
   "--reduc-debug", Arg.Set reduc_debug,
      " Print the intermediate reduction steps" ;
   "--get-proof", Arg.Set get_proof,
      " Print the typing tree in a tex file" ;
   "--verb-proof", Arg.Set verb_proof,
      " Print the typing tree in a verbose mode" ;
   "--short-let", Arg.Set short_let,
      " Print the typing tree of arguments of let-bindings in a separate tree" ;
   "--proof-file", Arg.Set_string proof_file,
      " Set the proof filename (default is filname.tex)";
   "--syst-ext", Arg.Set_string syst_ext,
      " Rewrite the file extension";
   "--syst-file", Arg.Set_string syst_file,
      " Use the system defined in this file";
   "--interactive", Arg.Set interactive,
      " Read the entry from stdin";
   "--inline-def", Arg.Set inline_def,
      " Inline the definition during typing";
   "--get-metric", Arg.Set get_metric,
      " prints some metrics such as the size of the proof and the number of
      reduction steps";
   "--color", Arg.Set color, " Print in stdout in color";
   "--no-arrow", Arg.Set no_arrow,
      " Don't print non dependent product types as arrows";
   "--no-memoize", Arg.Set no_memoize,
      " Don't memoize proof search (might increase time & memory consumption)"]

let usage = "usage: main [option] file.f"


let files _ =
	let file = ref None in
	let set_file s =
		file := Some s in
  let _ = Arg.parse spec set_file usage in
  let file = match !file with
		|Some f -> f
		|None ->
      if not !interactive then
        let _ = Format.printf "No filename provided\n" in
        let _ = Arg.usage (Arg.align spec) usage in
        exit 1
      else ""
      in
  if !proof_file = "" then
    file, (Filename.remove_extension file)^".tex"
  else
    file, !proof_file
