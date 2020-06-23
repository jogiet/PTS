

let file_name   = ref ""
and parse_only  = ref false
and type_only   = ref false
and type_debug  = ref false
and reduc_debug = ref false
and get_proof   = ref false
and verb_proof  = ref false
and short_let   = ref false
and proof_file  = ref ""
and get_metric  = ref false
and color       = ref false
and no_arrow    = ref false


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
   "--get-metric", Arg.Set get_metric,
      " prints some metrics such as the size of the proof and the number of
      reduction steps";
   "--color", Arg.Set color, " Print in stdout in color";
   "--no-arrow", Arg.Set no_arrow,
      " Don't print non dependent product types as arrows"]

let usage = "usage: main [option] file.f"


let file =
	let file = ref None in
	let set_file s =
		file := Some s
	in begin
		Arg.parse spec set_file usage;
		match !file with
		|Some f -> f
		|None -> Arg.usage (Arg.align spec) usage; exit 1
	end

let proof_file = 
  if !proof_file = "" then
    (Filename.remove_extension file)^".tex"
  else
    !proof_file
