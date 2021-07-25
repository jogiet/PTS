(** This File contains all pretty-priniting functions *)
open Color
open Ast

let new_row = "\\\\\n\\\\\n\\\\\n"

  (** [pretty_printer fmt t] outputs a pretty-printed in [fmt] *)
let pretty_printer latex newline fmt (t: term located) =
  let _ = ignore (newline = "") in
  let colorize =
    let is_stdout = (not latex) && (fmt == Format.std_formatter) in
    print_effect is_stdout in
  let space = if latex then "\\ " else "" in
  let rec print_app fmt t =
    match fst t with
    | App (t1, t2) ->
        Format.fprintf fmt "%a%s@ %a"
        print_app_par t1
        space
        print_atom t2
    | Var id ->
        Format.fprintf fmt "%s" @@ fst id
    | Lam _ | Prod _ | Let _ | Cast _ ->
        Format.fprintf fmt "(%a)"
        print_atom t
  and print_app_par fmt t =
    match fst t with
    | App (t1, (Var s, _)) ->
        Format.fprintf fmt "%a%s@ %s"
        print_app t1
        space
        (fst s)
    | App (t1, t2) ->
        Format.fprintf fmt "%a%s@ (%a)"
        print_app t1
        space
        print_atom t2
    | Var id ->
        Format.fprintf fmt "%s" @@ fst id
    | Lam _ | Prod _ | Let _ | Cast _ ->
        Format.fprintf fmt "(%a)"
        print_atom t
  and print_atom fmt t =
    match fst t with
    | App _ ->
        Format.fprintf fmt "(@[<hov 1>%a@])"
        print_app t
    | Var id ->
        Format.fprintf fmt "%s" @@ fst id
    | Lam (id, t1, t2) ->
        Format.fprintf fmt "@[<hov 1>%aλ%a(%s:%a).@,%a@]"
          colorize Blue
          colorize Reset
          id
          print_atom t1
          print_lam_pi_box t2
    | Cast (t1, t2) ->
        Format.fprintf fmt "%a : %a"
          print_global t1
          print_global t2
    | Let (id, (Cast (t1, new_typ), _), t2) ->
        let let_bind = if latex then "\\texttt{ let }" else "let" in
        let in_bind = if latex then "\\texttt{ in }" else "in" in
        Format.fprintf fmt
          "@[<hov 0>@[<hov 2>%a%s%a %s %a:%a@ %a = @ %a@] %a%s%a@ %a@]"
        (*            let  t    :   typ = def    in    t *)
          colorize Blue
          let_bind
          colorize Reset
          id
          colorize Blue
          colorize Reset
          print_atom new_typ
          print_atom t1
          colorize Blue
          in_bind
          colorize Reset
          print_global t2
    | Let (id, t1, t2) ->
        let let_bind = if latex then "\\texttt{ let }" else "let" in
        let in_bind = if latex then "\\texttt{ in }" else "in" in
        Format.fprintf fmt "@[<hov 0>@[<hov 2>%a%s%a %s = %a @] %a%s%a@ %a@]"
          colorize Blue
          let_bind
          colorize Reset
          id
          print_atom t1
          colorize Blue
          in_bind
          colorize Reset
          print_global t2
    | Prod (id, t1, t2) ->
        if IdSet.mem id (get_fv t2) || !Options.no_arrow then
          Format.fprintf fmt "@[<hov 1>%a∀%a(%s:%a).@,%a@]"
            colorize Blue
            colorize Reset
            id
            print_atom t1
            print_lam_pi_box t2
        else
          Format.fprintf fmt "@[<hov 1>%a %a→ %a@ %a@]"
            print_left_arrow t1
            colorize Blue
            colorize Reset
            print_right_arrow t2
  and print_left_arrow fmt t =
    match fst t with
    | Var id -> Format.fprintf fmt "%s" @@ fst id
    | _ ->
        Format.fprintf fmt "(%a)"
          print_global t
  and print_right_arrow fmt t =
    match fst t with
    | Prod (id, t1, t2) when not @@ is_free id t2 ->
        Format.fprintf fmt "%a %a→ %a@ %a"
          print_left_arrow t1
          colorize Blue
          colorize Reset
          print_right_arrow t2
    | _ -> print_left_arrow fmt t
  and print_lam_pi_box fmt t =
    match fst t with
    | Lam (id, t1, t2) ->
        Format.fprintf fmt "%aλ%a(%s:%a).@,%a"
          colorize Blue
          colorize Reset
          id
          print_atom t1
          print_lam_pi_box t2
    | Prod (id, t1, t2) when (is_free id t2) || !Options.no_arrow ->
          Format.fprintf fmt "%a∀%a(%s:%a).@,%a"
            colorize Blue
            colorize Reset
            id
            print_atom t1
            print_lam_pi_box t2
    | _ -> print_global fmt t
  and print_global fmt t =
    match fst t with
    | Var id ->
        Format.fprintf fmt "%s" @@ fst id
    | Lam _ | Prod _ | Let _ | Cast _ ->
        print_atom fmt t
    | App _ ->
        Format.fprintf fmt "@[<hov 1>%a@]"
          print_app t
  in print_global fmt t

let pretty_printer_latex = pretty_printer true ""
let pretty_printer_line = pretty_printer false
let pretty_printer = pretty_printer false ""

let print_typing_def latex fmt def =
  if IdMap.is_empty def then () else
    let _ = if latex then Format.fprintf fmt "\\begin{array}{r%@{\\,}l}" in
    let _ = IdMap.iter
      (fun id t ->
        if latex then
          Format.fprintf fmt "%s&:= %a\\\\"
            id pretty_printer_latex t
        else
          Format.fprintf fmt "%s := %a; "
            id pretty_printer t)
      def
    in
    let _ = if latex then Format.fprintf fmt "\\end{array}" in
    ()

let print_typing_def_latex = print_typing_def true
let print_typing_def = print_typing_def false

let print_typing_env latex fmt env =
  if env = [] then () else
    let _ = if latex then Format.fprintf fmt "\\left\\|\\begin{array}{r%@{\\,}l}" in
    let _ = List.iter
      (fun (id, t) ->
        if latex then
          Format.fprintf fmt "%s:& %a\\\\"
            id pretty_printer_latex t
        else
          Format.fprintf fmt "%s: %a;"
            id pretty_printer t)
      env in
    let _ = if latex then Format.fprintf fmt "\\end{array}\\right." in
    ()

let print_typing_env_latex = print_typing_env true
let print_typing_env = print_typing_env false

let print_all_let fmt let_queue =
  let _ = Queue.iter
    (fun (id, typ) -> Format.fprintf fmt "%s\t: %a@ "
      id
      pretty_printer typ)
    let_queue in
  ()

let print_typing_judgment fmt (def, env, t1, t2) =
  let po, pf = if IdMap.is_empty def && env = [] then "", "" else
    "\\left\\{", "\\right." in
  Format.fprintf fmt "%s%a%a%s ⊢ %a:%a"
    po
    print_typing_def_latex def
    print_typing_env_latex env
    pf
    pretty_printer_latex t1
    pretty_printer_latex t2

let print_typing_syst latex fmt (syst: system) =
  let rec print_list f fmt = function
    | [] -> ()
    | [x] -> Format.fprintf fmt "%a" f x
    | t::q ->
      Format.fprintf fmt "%a; @," f t;
      print_list f fmt q
  in
  let acco = if latex then "\\{" else "{" in
  let accf = if latex then "\\}" else "}" in
  let sep  = if latex then "&\\," else ""  in
  let _ = Format.fprintf fmt "@[<v 0>" in
  let _ = if latex
  then Format.fprintf fmt "$\\left\\{\\begin{array}{r%@{}l}@ "
    else () in
  let _ = Format.fprintf fmt "𝒮 =%s %s@[<hov 0>%a@]%s@ "
    sep acco
    (print_list (fun fmt id -> Format.fprintf fmt "%s" id))
    (IdSet.elements syst.sorts)
    accf in
  let _ = if latex then Format.fprintf fmt "\\\\@ " else () in
  let _ = Format.fprintf fmt "𝒜 =%s %s@[<hov 0>%a@]%s@ "
    sep acco
    (print_list (fun fmt (id, bind) -> Format.fprintf fmt "(%s: %s)" id bind))
    (IdMap.bindings syst.axioms)
    accf in
  let _ = if latex then Format.fprintf fmt "\\\\@ " else () in
  let _ = Format.fprintf fmt "ℛ =%s %s@[<hov 0>%a@]%s@ "
    sep acco
    (print_list
       (fun fmt ((id1, id2), bind) ->
          Format.fprintf fmt "(%s, %s, %s)" id1 id2 bind))
    (Id2Map.bindings syst.rules)
    accf in
  let _ = if latex then Format.fprintf fmt "\\\\@ " else () in
  let _ = if latex
    then Format.fprintf fmt "\\end{array}\\right.$@ "
    else () in
  let _ = Format.fprintf fmt "@]" in
  ()

let print_typing_syst_latex = print_typing_syst true
let print_typing_syst = print_typing_syst false

let rec print_typing_tree offset let_bind fmt tree =
  match tree with
  | Ast.Axiom j ->
      let _, _, t, ty = j in
      let rule = "\\RightLabel{Ax}" in
      Format.fprintf fmt "%s\\AXC{$(%a, %a)∈ 𝒜 $}\n%s%s\n%s\\UIC{$%a$}\n"
        offset
        pretty_printer_latex t pretty_printer_latex ty
        offset rule
        offset print_typing_judgment j
  | Ast.Weakening (id, tree1, tree2, j) ->
      let rule = Format.sprintf "\\RightLabel{Weak $%s$}" id in
      let new_offset = "  "^offset in
      Format.fprintf fmt "%a%a%s%s\n%s\\BIC{$%a$}\n"
        (print_typing_tree new_offset let_bind) tree1
        (print_typing_tree new_offset let_bind) tree2
        offset rule
        offset print_typing_judgment j
  | Ast.Start (tree, j) ->
      let rule = "\\RightLabel{Start}" in
      let new_offset = "  "^offset in
      Format.fprintf fmt "%a%s%s\n%s\\UIC{$%a$}\n"
        (print_typing_tree new_offset let_bind) tree
        offset rule
        offset print_typing_judgment j
  | Ast.Product (tree1, tree2, j) ->
      let rule = "\\RightLabel{$∀$}" in
      let new_offset = "  "^offset in
      Format.fprintf fmt "%a%a%s%s\n%s\\BIC{$%a$}\n"
        (print_typing_tree new_offset let_bind) tree1
        (print_typing_tree new_offset let_bind) tree2
        offset rule
        offset print_typing_judgment j
  | Ast.Abstraction (tree1, tree2, tree3, j) ->
      let rule = "\\RightLabel{$λ$}" in
      let new_offset = "  "^offset in
      Format.fprintf fmt "%a%a%a%s%s\n%s\\TIC{$%a$}\n"
        (print_typing_tree new_offset let_bind) tree1
        (print_typing_tree new_offset let_bind) tree2
        (print_typing_tree new_offset let_bind) tree3
        offset rule
        offset print_typing_judgment j
  | Ast.LetIntro (_, tree2, j) when let_bind ->
      let _, _, term, _ = j in
      let ident = match fst term with
        | Let (id, _, _) -> id
        | _ -> assert false in
      let rule = "\\RightLabel{$Let$}" in
      let new_offset = "  "^offset in
      Format.fprintf fmt "%s\\AXC{\\Huge $%s$}\n%a%s%s\n%s\\BIC{$%a$}\n"
        new_offset ident
        (print_typing_tree new_offset let_bind) tree2
        offset rule
        offset print_typing_judgment j
  | Ast.LetIntro (tree1, tree2, j) ->
      let rule = "\\RightLabel{Let}" in
      let new_offset = "  "^offset in
      Format.fprintf fmt "%a%a%s%s\n%s\\BIC{$%a$}\n"
        (print_typing_tree new_offset let_bind) tree1
        (print_typing_tree new_offset let_bind) tree2
        offset rule
        offset print_typing_judgment j
  | Ast.Application (tree1, tree2, j) ->
      let rule = "\\RightLabel{App}" in
      let new_offset = "  "^offset in
      Format.fprintf fmt "%a%a%s%s\n%s\\BIC{$%a$}\n"
        (print_typing_tree new_offset let_bind) tree1
        (print_typing_tree new_offset let_bind) tree2
        offset rule
        offset print_typing_judgment j
  | Ast.Conversion (tree1, t1, t2, tree2, j) ->
      let rule = "\\RightLabel{Conv}" in
      let new_offset = "  "^offset in
      Format.fprintf fmt "%a%a%s\\AXC{$%a \\sim_{αβ} %a$}\n%s%s\n%s\\TIC{$%a$}\n"
        (print_typing_tree new_offset let_bind) tree1
        (print_typing_tree new_offset let_bind) tree2
        new_offset pretty_printer_latex t1 pretty_printer_latex t2
        offset rule
        offset print_typing_judgment j


let rec print_typing_tree_sparse offset let_bind fmt tree =
  match tree with
  | Ast.Axiom j ->
      let _, _, t, ty = j in
      let rule = "\\RightLabel{Ax}" in
      Format.fprintf fmt "%s\\AXC{$(%a, %a)∈ 𝒜 $}\n%s%s\n%s\\UIC{$%a$}\n"
        offset
        pretty_printer_latex t pretty_printer_latex ty
        offset rule
        offset print_typing_judgment j
  | Ast.Weakening (_, tree1, _, _) ->
      let new_offset = "  "^offset in
      Format.fprintf fmt "%a"
        (print_typing_tree_sparse new_offset let_bind) tree1
  | Ast.Start (_, j) ->
      let rule = "\\RightLabel{Start}" in
      Format.fprintf fmt "%s\\AXC{}\n%s%s\n%s\\UIC{$%a$}\n"
        offset
        offset rule
        offset print_typing_judgment j
  | Ast.Product (_, tree2, j) ->
      let rule = "\\RightLabel{$∀$}" in
      let new_offset = "  "^offset in
      Format.fprintf fmt "%a%s%s\n%s\\UIC{$%a$}\n"
        (print_typing_tree_sparse new_offset let_bind) tree2
        offset rule
        offset print_typing_judgment j
  | Ast.Abstraction (_, tree2, _, j) ->
      let rule = "\\RightLabel{$λ$}" in
      let new_offset = "  "^offset in
      Format.fprintf fmt "%a%s%s\n%s\\UIC{$%a$}\n"
        (print_typing_tree_sparse new_offset let_bind) tree2
        offset rule
        offset print_typing_judgment j
  | Ast.LetIntro (_, tree2, j) when let_bind ->
      let _, _, term, _ = j in
      let ident = match fst term with
        | Let (id, _, _) -> id
        | _ -> assert false in
      let rule = "\\RightLabel{$Let$}" in
      let new_offset = "  "^offset in
      Format.fprintf fmt "%s\\AXC{\\Huge \\hyperref[let:%s]{$%s$}}\n%a%s%s\n%s\\BIC{$%a$}\n"
        new_offset ident ident
        (print_typing_tree_sparse new_offset let_bind) tree2
        offset rule
        offset print_typing_judgment j
  | Ast.LetIntro (tree1, tree2, j) ->
      let rule = "\\RightLabel{$Let$}" in
      let new_offset = "  "^offset in
      Format.fprintf fmt "%a%a%s%s\n%s\\BIC{$%a$}\n"
        (print_typing_tree_sparse new_offset let_bind) tree1
        (print_typing_tree_sparse new_offset let_bind) tree2
        offset rule
        offset print_typing_judgment j
  | Ast.Application (tree1, tree2, j) ->
      let rule = "\\RightLabel{App}" in
      let new_offset = "  "^offset in
      Format.fprintf fmt "%a%a%s%s\n%s\\BIC{$%a$}\n"
        (print_typing_tree_sparse new_offset let_bind) tree1
        (print_typing_tree_sparse new_offset let_bind) tree2
        offset rule
        offset print_typing_judgment j
  | Ast.Conversion (tree1, _, _, _, j) ->
      let rule = "\\RightLabel{Conv}" in
      let new_offset = "  "^offset in
      Format.fprintf fmt "%a\n%s%s\n%s\\UIC{$%a$}\n"
        (print_typing_tree_sparse new_offset let_bind) tree1
        offset rule
        offset print_typing_judgment j

let rec print_let sparse fmt tree =
  match tree with
  | LetIntro (tree1, tree2, j) ->
      let _ = print_let sparse fmt tree1 in
      let _, _, term, _ = j in
      let ident = match fst term with
        | Let (id, _, _) -> id
        | _ -> assert false in
      Format.fprintf fmt "\\pdfbookmark[0]{%s}{$%s$}%a\n  \\UIC{\\Huge $%s$\\label{let:%s}}\n\\DisplayProof\n%s"
        ident ident
        (if sparse
          then print_typing_tree_sparse "  " true
          else print_typing_tree "  " true)
        tree1
        ident
        ident
        new_row;
      print_let sparse fmt tree2
  | Axiom _ -> ()
  | Start (tree, _) -> print_let sparse fmt tree
  | Conversion (tree1, _, _, tree2, _)
  | Application (tree1, tree2, _)
  | Product (tree1, tree2, _)
  | Weakening (_, tree1, tree2, _) ->
      print_let sparse fmt tree1;
      print_let sparse fmt tree2
  | Abstraction (tree1, tree2, tree3, _) ->
      print_let sparse fmt tree1;
      print_let sparse fmt tree2;
      print_let sparse fmt tree3

let print_let sparse let_bind fmt tree =
  if let_bind then
    print_let sparse fmt tree
  else
    ()


let print_proof syst tree file =
  let header =
"\\documentclass[margin=5mm]{standalone}
\\usepackage[utf8]{inputenc}
\\usepackage[T1]{fontenc}
\\usepackage{unicode-math}
\\usepackage{bussproofs}
\\usepackage{hyperref}
\\begin{document}
\\EnableBpAbbreviations
\\begin{tabular}{c}"
  and footer =
"\\DisplayProof
\\end{tabular}
\\end{document}"
  in
  let chan = open_out file in
  let let_bind = !Options.short_let in
  let printer = if !Options.verb_proof  then
    print_typing_tree "  " let_bind
  else
    print_typing_tree_sparse "  " let_bind
  in
  let fmt = Format.make_formatter
      (Stdlib.output_substring chan)
      (fun () -> Stdlib.flush chan)  in
  let _ = Format.fprintf fmt "%s\n\n%a\n%s%a\n\n%a\n\n%s"
    header
    print_typing_syst_latex syst
    new_row
    (print_let (not !Options.verb_proof) let_bind) tree
    printer tree
    footer in
  let _ = close_out chan in
  ()

let generic_to_string f x =
  let _ = Format.(fprintf str_formatter "%a" f x) in
  Format.flush_str_formatter ()
