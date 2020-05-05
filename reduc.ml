open Ast
open Printer

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
   | App (Prod (x, t1, t2), t) ->
       let _ = Printf.printf "Pattern found : id = %s\n" x in
       subst x t t2, true
   | App (Lam (x, t1, t2), t) ->
       let _ = Printf.printf "Pattern found : id = %s\n" x in
       subst x t t2, true
   | App (t1, t2) ->
      let t1', b1 = beta_reduc t1 in
      if b1 then
        App (t1', t2), b1
      else
        let t2', b2 = beta_reduc t2 in
        App (t1, t2'), b2

  (** [get_nf t] tries to compute the normal form of [t]. *i.e.*, it applies
   {!beta_reduc} as long as it returns [t', true] *)
let rec get_nf t =
  let t, b = beta_reduc t in
  if b then
    let _ = Printf.printf "On continue...\n  %a\n" pretty_printer t in
    get_nf t
  else
    t
