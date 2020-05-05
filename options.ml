

let file_name   = ref ""
and parse_only  = ref false
and type_only   = ref false
and color       = ref false


let spec =
	["--parse-only", Arg.Set parse_only, " Stop after parsing";
   "--type-only",Arg.Set type_only, " Stop after typing" ;
   "--color", Arg.Set color, " Print in stdout in color"]

let usage = "usage: main [optioon] file.f"


let file =
	let file = ref None in
	let set_file s =
		if not (Filename.check_suffix s ".f") then
			raise (Arg.Bad "no .f extension");
		file := Some s
	in begin
		Arg.parse spec set_file usage;
		match !file with
		|Some f -> f
		|None -> Arg.usage (Arg.align spec) usage; exit 1
	end
