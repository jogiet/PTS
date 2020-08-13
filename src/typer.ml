open Ast
open Reduc
open Printer
open Options

let is_sort syst s =
  IdSet.mem s syst.sorts

let all_let: (ident * term located) Queue.t = Queue.create ()

(** [ident] coresponds to the current indentation in Typing progress printing
 * *)
let ident = ref ""
let add () = ident := "|  "^(!ident)
let sub () = ident := String.(sub !ident 0 (length !ident -3))

let subst x t t' = 
  let res = subst x t t' in
  let _ = if !type_debug then Format.printf "%s%a{%s ← %a} = %a\n"
    !ident
    pretty_printer t'
    x
    pretty_printer t
    pretty_printer res in
  res

let (let*) o f =
  match o with
  | None -> None
  | Some x -> (f x)
let return x = Some x

exception Type_error of string located

exception Def_found of ident

  (** [type_check syst def env term]  returns a [typ] such that the judgment 
   [def];[env]⊢ [term]:[typ] in [syst] holds *)
let rec type_check (syst: system) (def: typing_def) (env: typing_env) term : 
  term located * typing_tree option  =
  let _ = if !type_debug then
    Format.printf "%s|->trying to type : %a\n" 
      !ident pretty_printer term in
  let _ = add () in
  let _ = if !type_debug then
    let _ = Format.printf "%senv = %a\n" 
      !ident print_typing_env env in
    let _ = Format.printf "%sdef = %a\n" 
      !ident print_typing_def def 
    in ()
  in
  let pos = snd term in
  match fst term, env with
  (* Axiom Rule *)
  | Var (s_id, _ as s), [] ->
  begin
    let _ = if not (is_sort syst s_id) then
      Format.printf "Error : %s ∉ Γ\n" s_id in
    let _ = if !type_debug then Format.printf "%sApply Axiom\n" !ident in
      try 
        let s' = add_loc @@ IdMap.find s_id syst.axioms in
        let j = if !Options.get_proof 
          then Some (Axiom (def, [], add_loc @@ Var s, add_loc @@ Var s'))
          else None
        in
        let _ = if !type_debug then 
          Format.printf "%sτ = %a\n" !ident pretty_printer (add_loc @@ Var s') in
        let _ = sub () in
        add_loc @@ Var s', j
      with Not_found ->
        let err = Format.sprintf "(%s, _) ∉ 𝒜 " s_id in
        raise (Type_error (err, pos))
  end
  (* Start Rule *)
  | Var (x_id, _), (y, typ)::new_env when x_id = y ->
  begin
    let _ = if !type_debug then Format.printf "%sApply Start\n" !ident in
    let new_def = IdMap.remove y def in
    let _, tree = type_check syst new_def new_env typ in
    let proof = 
      let j = def, env, term, typ in 
      let* tree = tree in
      return (Start (tree, j)) in
    let _ = if !type_debug then 
      Format.printf "%sτ = %a\n" !ident pretty_printer typ in
    let _ = sub () in
    typ, proof
  end
  (* Weaken Rule *)
  | _, (id, _)::_ when not ((is_free id term) (* || (IdMap.mem id def) *)) ->
      weaken syst def env term
  | Var _, env ->
      weaken syst def env term
  | Lam (x, _, _), env
  | Let (x, _, _), env 
  | Prod (x, _, _), env when (List.mem_assoc x env) ->
      term |> alpha_rename |> type_check syst def env
  | Cast (t, new_typ), env ->
      let typ_res, tree = type_check syst def env t in
      new_typ, conversion syst def env t typ_res tree new_typ 
  | Lam (x, t1, t2), env ->
      let _ = if !type_debug then 
        Format.printf "%sApply abstraction\n" !ident in
      let _ = assert (not (is_sort syst x)) in
      let _, tree1 = type_check syst def env t1 in
      let new_env = (x, t1) :: env in
      let typ2, tree2 = type_check syst def new_env t2 in
      let _, tree3 = type_check syst def new_env typ2 in
      let typ_res = add_loc @@ Prod (x, t1, typ2) in
      let proof = 
        let* tree1 = tree1 in
        let* tree2 = tree2 in
        let* tree3 = tree3 in
        let j = def, env, term, typ_res in
        return (Abstraction (tree1, tree2, tree3, j)) in
      let _ = if !type_debug then 
        Format.printf "%sτ = %a\n" !ident pretty_printer typ_res in
      let _ = sub () in
      find_def syst def env term typ_res proof
  | Let (id, d, t), env ->
      let _ = if !type_debug then 
        Format.printf "%sApply Let\n" !ident in
      let tyd, tree1 = type_check syst def env d in
      let _ = if !type_only then
        Queue.add (id, tyd) all_let in
      let new_def, new_env, new_t =
        if !inline_def then
          def, env, subst id d t
        else
          IdMap.add id d def, (id, tyd)::env, t in
      let typ_res, tree2 = type_check syst new_def new_env new_t in
      let proof =
        let* tree1 = tree1 in
        let* tree2 = tree2 in
        let j = def, env, term, typ_res in
        return (LetIntro (tree1, tree2, j)) in
      let _ = if !type_debug then 
        Format.printf "%sτ = %a\n" !ident pretty_printer typ_res in
      let _ = sub () in
      typ_res, proof
  | Prod (x, t1, t2), env ->
  begin
      let _ = if !type_debug then Format.printf "%sApply Product\n" !ident in
      let _ = assert (not (is_sort syst x)) in
      let s1, tree1 = type_check syst def env t1 in
      let new_env = (x, t1) :: env in
      let s2, tree2 = type_check syst def new_env t2 in
      match fst s1, fst s2 with
      | Var (s1_id, _), Var (s2_id, _) 
        when is_sort syst s1_id && is_sort syst s2_id ->
      begin
          try 
            let s3_id = Id2Map.find (s1_id, s2_id) syst.rules in
            let typ_res = add_loc @@ Var (add_loc s3_id) in
            let proof = 
              let* tree1 = tree1 in
              let* tree2 = tree2 in
              let j = def, env, term, typ_res in
              return (Product (tree1, tree2, j)) in
            let _ = if !type_debug then 
              Format.printf "%sτ = %a\n" !ident pretty_printer typ_res in
            let _ = sub () in
            find_def syst def env term typ_res proof
          with
          | Not_found ->
              let err = Format.sprintf "(%s, %s, _) ∉ ℛ " s1_id s2_id in
              raise (Type_error (err, pos))
      end
      | Var _, x
      | x, _ ->
          let x = add_loc x in
          let err = Format.sprintf "%s is not a sort"
              (generic_to_string pretty_printer x) in
          raise (Type_error (err, pos))
  end
  | App (t1, t2), env ->
      let _ = if !type_debug then 
        Format.printf "%sApply Application\n" !ident in
      let ty1, tree1 = type_check syst def env t1 in
      let ty2, tree2 = type_check syst def env t2 in
      let x, ty1', ty_res =
        match fst ty1 with
        | Prod (x, ty1', ty_res) -> x, ty1', ty_res
        | _ -> match fst @@ get_nf_def def ty1 with
        | Prod (x, ty1', ty_res) -> x, ty1', ty_res
        | _ -> 
          let err = Format.sprintf "%s is not an arrow type"
              (generic_to_string pretty_printer ty1) in
          raise (Type_error (err, snd t1))
      in
      let tree2 = conversion syst def env t2 ty2 tree2 ty1' in
      let typ_res = subst x t2 ty_res in
      let proof = 
        let* tree1 = tree1 in
        let* tree2 = tree2 in
        let j = def, env, term, typ_res in
        return (Application (tree1, tree2, j)) in
      let _ = if !type_debug then 
        Format.printf "%sτ = %a\n" !ident pretty_printer typ_res in
      let _ = sub () in
      find_def syst def env term typ_res proof

and weaken (syst: system) (def: typing_def) (env: typing_env) term :
  term located * typing_tree option =
  let y, typ_y = List.hd env in
  let _ = if !type_debug then 
    Format.printf "%sApply Weakening: %s\n" !ident y in
  let new_env = List.tl env in
  let new_def = IdMap.remove y def in
  let typ_res , tree1 = type_check syst new_def new_env term in
  let _ , tree2 = type_check syst new_def new_env typ_y in
  let _ = if !type_debug then 
    Format.printf "%sτ = %a\n" !ident pretty_printer typ_res in
  let _ = sub () in
  let proof = 
    let* tree1 = tree1 in
    let* tree2 = tree2 in
    let j = def, env, term, typ_res in
    return (Weakening (y, tree1, tree2, j)) in
  typ_res, proof

and find_def (syst: system) (def: typing_def) (env: typing_env) term typ tree :
  term located * typing_tree option =
  match fst typ with
  | Var _ -> typ, tree
  | _ ->
  try 
    let _  = IdMap.iter
      (fun id term -> if (alpha_equiv def term typ) then raise (Def_found id))
      def in
    typ, tree
  with
  | Def_found id ->
      let _ = if !type_debug then
        Format.printf "%sWe match %a ~α %s\n" !ident
          pretty_printer typ id in
      let new_typ = add_loc @@ Var (add_loc id) in
      let proof =
        let* tree = tree in
        let* tree2 = snd @@ type_check syst def env new_typ in
        let j = def, env, term, new_typ in
        return (Conversion (tree, typ, new_typ, tree2, j)) in
      new_typ , proof


and conversion (syst: system) def env t typ tree new_typ =
  if typ = new_typ then tree else
  let _ = if !type_debug then
    let _ = Format.printf "%strying to convert : \n%s  %a\n%s  %a\n" 
      !ident 
      !ident pretty_printer typ
      !ident pretty_printer new_typ in
    let _ = Format.printf "%senv =%a\n" !ident print_typing_env env in
    let _ = Format.printf "%sApply Conversion\n" !ident in 
    ()
  in
  let _, tree2 = type_check syst def env new_typ in
  let nf_t = get_nf_def def typ in
  let nf_t' = get_nf_def def new_typ in
  if alpha_equiv def nf_t nf_t' then
    let* tree = tree in
    let* tree2 = tree2 in
    let j = (def, env, t, new_typ) in
    return (Conversion (tree, typ, new_typ, tree2, j))
  else 
    let err = Format.sprintf "Cannot convert types\n%s\n%s"
      (generic_to_string pretty_printer typ)
      (generic_to_string pretty_printer new_typ) in
    raise (Type_error (err, snd t))
