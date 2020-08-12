open Ast
open Printer
open Options

let new_id = 
  let count = ref 0 in
  fun id -> incr count; Format.sprintf "%s_%i" id !count

  (** [alpha_equiv t t'] returns [true] iff [t] ~α [t'] *)
let alpha_equiv def t t' = 
  let rec aux map t t' = 
    match fst t, fst t' with
    | Var (s, _), Var (s', _) when s = s' -> true
    | Var (x, _), _ when IdMap.mem x def ->
        aux map (IdMap.find x def) t'
    | _, Var (x, _) when IdMap.mem x def ->
        aux map t (IdMap.find x def)
    | Var (x, _), Var (x', _) ->
    begin
        try 
          IdMap.find x map = x'
        with Not_found -> false
    end
    | App (t1, t2), App (t1', t2') ->
        aux map t1 t1' && aux map t2 t2'
    | Lam (x, t1, t2), Lam (x', t1', t2')
    | Prod (x, t1, t2), Prod (x', t1', t2')
    | Let (x, t1, t2), Let (x', t1', t2') ->
        let map' = IdMap.add x x' map in
        aux map t1 t1' && aux map' t2 t2'
    | Cast (t, _), _ -> aux map t t'
    | _, Cast (t', _) -> aux map t t'
    | _, _ -> false
  in aux IdMap.empty t t'

  (** [subst x t t'] remplaces all free occurences of [x] by [t] in [t'] *)
let rec subst x (t : term located) ( t' : term located) =
  match fst t', fst t with
  | Var (id, pos), _ when id = x -> fst t, pos
  | Var _, _ -> t'
  | Lam (id, t1, t2), _ when id = x ->
      Lam (id, subst x t t1, t2), snd t'
  | Prod (id, t1, t2),_  when id = x ->
      Prod (id, subst x t t1, t2), snd t'
  | Let (id, t1, t2), _ when id = x ->
      Let (id, subst x t t1, t2), snd t'
  | Lam (id, t1, t2), Var (id', _) when id = id' ->
      let nid = new_id id in
      let t2 = t2
        |> subst id (add_loc @@ Var (add_loc nid))
        |> subst x t
      in
      Lam (nid, subst x t t1, t2), snd t'
  | Prod (id, t1, t2), Var (id', _) when id = id' ->
      let nid = new_id id in
      let t2 = t2
        |> subst id (add_loc @@ Var (add_loc nid))
        |> subst x t
      in
      Prod (nid, subst x t t1, t2), snd t'
  | Let (id, t1, t2), Var (id', _) when id = id' ->
      let nid = new_id id in
      let t2 = t2 
        |> subst id (add_loc @@ Var (add_loc nid))
        |> subst x t
      in
      Let (nid, subst x t t1, t2), snd t'
  | Lam (id, t1, t2), _ ->
      Lam (id, subst x t t1, subst x t t2), snd t'
  | Prod (id, t1, t2), _ ->
      Prod (id, subst x t t1, subst x t t2), snd t'
  | Let (id, t1, t2), _ ->
      Let (id, subst x t t1, subst x t t2), snd t'
  | App (t1, t2), _ -> 
      App (subst x t t1, subst x t t2), snd t'
  | Cast (t1, new_typ), _ ->
      Cast (subst x t t1, subst x t new_typ), snd t'

  (** [beta_reduc t] returns [t', true] if [t] → β [t'] and [t, false] if no
   reduction has been performed *)
let beta_reduc_def def term : term located * bool =
  let rec aux term : term located * bool =
  let pos = snd term in
  match fst term with
  | Var (id, _)  when IdMap.mem id def -> 
      IdMap.find id def, true
  | Var _ -> term, false
  | Lam (id, t1, t2) ->
      let t1', b1 = aux t1 in
      if b1 then
        (Lam (id, t1', t2), pos), b1
      else
        let t2', b2 = aux t2 in
        (Lam (id, t1, t2'), pos), b2
  | Prod (id, t1, t2) ->
      let t1', b1 = aux t1 in
      if b1 then
        (Prod (id, t1', t2), pos), b1
      else
        let t2', b2 = aux t2 in
        (Prod (id, t1, t2'), pos), b2
   | Let (x, t1, t2) ->
       let _ = if !reduc_debug then
         Format.printf "Pattern found : id = %s\n" x in
       subst x t1 t2, true
   | App ((Prod (x, _, t2), _), t) ->
       let _ = if !reduc_debug then
         Format.printf "Pattern found : id = %s\n" x in
       subst x t t2, true
   | App ((Lam (x, _, t2), _), t) ->
       let _ = if !reduc_debug then
         Format.printf "Pattern found : id = %s\n" x in
       subst x t t2, true
   | App (t1, t2) ->
      let t1', b1 = aux t1 in
      if b1 then
        (App (t1', t2), pos), b1
      else
        let t2', b2 = aux t2 in
        (App (t1, t2'), pos), b2
   | Cast (t, _) -> t, true
   in aux term

let beta_reduc = beta_reduc_def IdMap.empty

let steps = ref 0

  (** [get_nf t] tries to compute the normal form of [t]. *i.e.*, it applies
   {!beta_reduc} as long as it returns [t', true] *)
let rec get_nf_def def t =
  let t, b = beta_reduc_def def t in
  if b then
    let _ = incr steps in
    let _ = if !reduc_debug then
      Format.printf "On continue...\n  %a\n" pretty_printer t in
    get_nf_def def t
  else
    let _ = if !reduc_debug then
      Format.printf "Forme normale atteinte en %i étapes\n" !steps in
    t

let get_nf = get_nf_def IdMap.empty
