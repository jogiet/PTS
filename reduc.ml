open Ast
open Printer
open Options

  (** [alpha_equiv t t'] returns [true] iff [t] ~α [t'] *)
let alpha_equiv def t t' = 
  let rec aux map t t' = 
    match t, t' with
    | Var s, Var s' when s = s' -> true
    | Var x, _ when IdMap.mem x def ->
        aux map (IdMap.find x def) t'
    | _, Var x when IdMap.mem x def ->
        aux map t (IdMap.find x def)
    | Var x, Var x' ->
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
    | _, _ -> false
  in aux IdMap.empty t t'

  (** [subst x t t'] remplaces all free occurences of [x] by [t] in [t'] *)
let rec subst x t = function
  | Var id when id = x -> t
  | Var id -> Var id
  | Lam (id, t1, t2) when id = x ->
      Lam (id, subst x t t1, t2)
  | Prod (id, t1, t2) when id = x ->
      Prod (id, subst x t t1, t2)
  | Let (id, t1, t2) when id = x ->
      Let (id, subst x t t1, t2)
  | Lam (id, t1, t2) ->
      Lam (id, subst x t t1, subst x t t2)
  | Prod (id, t1, t2) ->
      Prod (id, subst x t t1, subst x t t2)
  | Let (id, t1, t2) ->
      Let (id, subst x t t1, subst x t t2)
  | App (t1, t2) -> 
      App (subst x t t1, subst x t t2)

  (** [beta_reduc t] returns [t', true] if [t] → β [t'] and [t, false] if no
   reduction has been performed *)
let rec beta_reduc = function
  | Var id -> Var id, false
  | Lam (id, t1, t2) ->
      let t1', b1 = beta_reduc t1 in
      if b1 then
        Lam (id, t1', t2), b1
      else
        let t2', b2 = beta_reduc t2 in
        Lam (id, t1, t2'), b2
  | Prod (id, t1, t2) ->
      let t1', b1 = beta_reduc t1 in
      if b1 then
        Prod (id, t1', t2), b1
      else
        let t2', b2 = beta_reduc t2 in
        Prod (id, t1, t2'), b2
   | Let (x, t1, t2) ->
       let _ = if !reduc_debug then
         Printf.printf "Pattern found : id = %s\n" x in
       subst x t1 t2, true
   | App (Prod (x, t1, t2), t) ->
       let _ = if !reduc_debug then
         Printf.printf "Pattern found : id = %s\n" x in
       subst x t t2, true
   | App (Lam (x, t1, t2), t) ->
       let _ = if !reduc_debug then
         Printf.printf "Pattern found : id = %s\n" x in
       subst x t t2, true
   | App (t1, t2) ->
      let t1', b1 = beta_reduc t1 in
      if b1 then
        App (t1', t2), b1
      else
        let t2', b2 = beta_reduc t2 in
        App (t1, t2'), b2

let steps = ref 0

  (** [get_nf t] tries to compute the normal form of [t]. *i.e.*, it applies
   {!beta_reduc} as long as it returns [t', true] *)
let rec get_nf t =
  let t, b = beta_reduc t in
  if b then
    let _ = incr steps in
    let _ = if !reduc_debug then
      Printf.printf "On continue...\n  %a\n" pretty_printer t in
    get_nf t
  else
    let _ = Printf.printf "Forme normale atteinte en %i étapes\n" !steps in
    t
