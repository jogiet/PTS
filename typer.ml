open Ast
open Reduc
open Printer


module B = Buffer

let is_sort syst s =
  IdSet.mem s syst.sorts

(** [ident] coresponds to the current indentation in Typing progress printing
 * *)
let ident = ref ""
let add () = ident := "|  "^(!ident)
let sub () = ident := String.(sub !ident 0 (length !ident -3))

let subst x t t' = 
  let res = subst x t t' in
  let _ = Printf.printf "%s%a{%s ← %a} = %a\n"
    !ident
    pretty_printer t'
    x
    pretty_printer t
    pretty_printer res in
  res

  (** [type_check syst env term]  returns a [typ] such that the judgment 
   [syst];[env]⊢ [term]:[typ] holds *)
let rec type_check syst env term : term * typing_tree  =
  let _ = add () in
  let _ = Printf.printf "%strying to type : %a\n" !ident pretty_printer term in
  let _ = Printf.printf "%senv = %a\n" !ident print_typing_env env in
  match term with
  (* Axiom Rule *)
  | Var s when is_sort syst s ->
  begin
    let _ = Printf.printf "%sApply Axiom\n" !ident in
      try 
        let s' = IdMap.find s syst.axioms in
        let j = [], Var s, Var s' in
        let _ = sub () in
        Var s', empty_env env syst (Axiom j)
      with Not_found ->
        let err = Printf.sprintf "(%s, _) ∉ 𝒜 " s in
        failwith err
  end
  | Var x ->
  begin
    let _ = Printf.printf "%sApply Start\n" !ident in
      try 
        let typ_s = List.assoc x env in
        let new_env = prune_env x env syst in
        let _, tree = type_check syst new_env typ_s in
        let j = (x, typ_s)::new_env, term, typ_s in 
        let _ = sub () in
        typ_s, weaken x env syst (Start (tree, j))
      with
      | Not_found ->
          let err = Printf.sprintf "%s ∉ 𝚪" x in
          failwith err
  end
  | Lam (x, t1, t2) ->
      let _ = Printf.printf "%sApply abstraction\n" !ident in
      let _ = assert (not (is_sort syst x)) in
      let env = prune_env x env syst in
      let _, tree1 = type_check syst env t1 in
      let new_env = (x, t1) :: env in
      let typ2, tree2 = type_check syst new_env t2 in
      let _, tree3 = type_check syst new_env typ2 in
      let typ_res = Prod (x, t1, typ2) in
      let j = env, term, typ_res in
      let _ = sub () in
      typ_res, Abstraction (tree1, tree2, tree3, j)
  | Prod (x, t1, t2) ->
  begin
      let _ = Printf.printf "%sApply Product\n" !ident in
      let _ = assert (not (is_sort syst x)) in
      let env = prune_env x env syst in
      let s1, tree1 = type_check syst env t1 in
      let new_env = (x, t1) :: env in
      let s2, tree2 = type_check syst new_env t2 in
      match s1, s2 with
      | Var s1, Var s2 when is_sort syst s1 && is_sort syst s2 ->
      begin
          try 
            let s3 = Id2Map.find (s1, s2) syst.rules in
            let typ_res = Var s3 in
            let j = env, term, typ_res in
            let _ = sub () in
            typ_res, Product (tree1, tree2, j)
          with
          | Not_found ->
              let err = Printf.sprintf "(%s, %s, _) ∉ ℛ " s1 s2 in
              failwith err
      end
      | _ -> assert false

  end
  | App (t1, t2) ->
      let _ = Printf.printf "%sApply Application\n" !ident in
      let ty1, tree1 = type_check syst env t1 in
      let ty2, tree2 = type_check syst env t2 in
      match ty1 with
      | Prod (x, ty1', ty_res) ->
          let _ = conversion syst env ty1' ty2 in
          let typ_res = subst x t2 ty_res in
          let j = env, term, typ_res in
          let _ = sub () in
          typ_res, Application (tree1, tree2, j)
      | _ -> assert false

and prune_env x env syst : typing_env =
  (* Maybe the best should be to perform α-substitution on new types term to
   * avoid this weakening *)
  let rec aux = function 
    | (id, typ) :: env when id = x -> env
    | (id, typ) :: env -> aux env
    | [] -> assert false
  in
  if List.mem_assoc x env then 
    aux env
  else
    env

  (** [weaken x env syst tree] performs the necessary weakening steps to apply
   * {i Start} rule on [x], that correspionds to the [tree] given as argument
   *)
and weaken x env syst tree : typing_tree =
  match env with
  | (id, t) :: env when id = x -> tree
  | (id, t) :: env -> 
      let _ = Printf.printf "%s Weakening %s\n" !ident id in
      let tree = weaken x env syst tree in
      let _, tree_s = type_check syst env t in
      let (env, term, typ) = get_judgment tree in
      let j = (id, t):: env , term, typ in
      Weakening (tree, tree_s, j)
  | [] -> assert false

and empty_env env syst tree : typing_tree = 
  match env with
  | [] -> tree
  | (id, t) :: env ->
      let tree = empty_env env syst tree in
      let _, tree_s = type_check syst env t in
      let (env, term, typ) = get_judgment tree in
      let j = (id, t):: env , term, typ in
      Weakening (tree, tree_s, j)

and conversion syst env t' t =
  let _ = Printf.printf "%strying to convert : \n%s  %a\n%s  %a\n" 
    !ident 
    !ident pretty_printer t'
    !ident pretty_printer t in
  let _ = Printf.printf "%senv =%a\n" !ident print_typing_env env in
  let _ = Printf.printf "%sApply Conversion\n" !ident in
  let _ = type_check syst env t' in  
  let nf_t = get_nf t in
  let nf_t' = get_nf t' in
  if alpha_equiv nf_t nf_t' then
    ()
  else 
    let _ = Printf.printf "%a !~α %a\n"
      pretty_printer t'
      pretty_printer t in
    failwith "Failure in conversion rule"
