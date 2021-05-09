(** This file contains the AST of Pure Type Systems as well as the datatype used
  in type checking and also some utilitatries functions *)

open Lexing
type 'a located = ('a * (position * position))

let add_loc x = x, Lexing.(dummy_pos, dummy_pos)

(** {5 Identifiers & Utilities} *)

type ident = string

module Id = String
module Id2 = struct
  type t = ident * ident
  let compare = compare
end

module Id3 = struct
  type t = ident * ident * ident
  let compare = compare
end

module IdSet = Set.Make(Id)
module IdMap = Map.Make(Id)

module Id2Map = Map.Make(Id2)

(** {5 AST of PTS and important types } *)

type term =
  | Var of ident located            (** [Var x] is variable [x] *)
  | App of (term located * term located)            (** Function application *)
  | Lam of (ident * term located * term located)
  (** Lambda abstraction : [Lam (x, A, B)] : λ(x: A).B *)
  | Prod of (ident * term located * term located)
  (** Product abstraction : [Prod (x, A, B)] : ∀(x: A). B *)
  | Let of (ident * term located * term located)
  | Cast of (term located * term located)

  (** We assume that a system is functionnal *)
type system =
  { sorts:  IdSet.t  ;
    (** A set of sort: 𝒮 *)
    axioms: ident IdMap.t ;
    (** A set of axioms: 𝒜  ⊆ 𝒮 ² *)
    rules:  ident Id2Map.t
    (** A set of rules: ℛ ⊆ 𝒮 ³ *)
  }

exception Not_functionnal of string

(** {5 Typing} *)

type typing_def = term located IdMap.t

  (** A typing environment is a map [x] ↦ [term].
   Sort should not appear in typing environment *)
type typing_env = (ident * term located) list

type typing_judgement = typing_def * typing_env * term located * term located

type typing_tree =
  | Axiom of typing_judgement
  | Weakening of
      (ident * typing_tree * typing_tree * typing_judgement)
  | Start of
      (typing_tree * typing_judgement)
  | Product of
      (typing_tree * typing_tree * typing_judgement)
  | Abstraction of
      (typing_tree * typing_tree * typing_tree * typing_judgement)
  | LetIntro of
      (typing_tree * typing_tree * typing_judgement)
  | Application of
      (typing_tree * typing_tree * typing_judgement)
  | Conversion of
      (typing_tree * term located * term located * typing_tree * typing_judgement)

(** {5 Utilities} *)

(** [change_var syst set t] returns a new term equal to [t] such that all
  equal string variables are physically equal *)
let rec change_var syst set t =
  match fst t with
  | Var (id, loc) ->
    if IdSet.mem id set then
      Var (IdSet.find id set, loc), snd t
    else if IdSet.mem id syst.sorts then
      Var (IdSet.find id syst.sorts, loc), snd t
    else
      assert false
  | App (u, v) ->
    App (change_var syst set u, change_var syst set v), snd t
  | Lam (id, typ, u) ->
    let new_set = set |> IdSet.remove id |> IdSet.add id in
    Lam (id, change_var syst set typ, change_var syst new_set u), snd t
  | Prod (id, typ, u) ->
    let new_set = set |> IdSet.remove id |> IdSet.add id in
    Prod (id, change_var syst set typ, change_var syst new_set u), snd t
  | Let (id, def, u) ->
    let new_set = set |> IdSet.remove id |> IdSet.add id in
    Let (id, change_var syst set def, change_var syst new_set u), snd t
  | Cast (u, v) ->
    Cast (change_var syst set u, change_var syst set v), snd t

let get_judgment = function
  | Axiom j -> j
  | Weakening (_, _, _, j) -> j
  | Start (_, j) -> j
  | Product (_, _, j) -> j
  | Abstraction (_, _, _, j) -> j
  | LetIntro (_, _, j) -> j
  | Application (_, _, j) -> j
  | Conversion (_, _, _, _, j) -> j

let rec get_fv t =
  match fst t with
  | Var id -> IdSet.singleton @@ fst id
  | Lam (id, t1, t2)
  | Prod (id, t1, t2)
  | Let (id, t1, t2) ->
      let set = IdSet.union (get_fv t1) (get_fv t2) in
      IdSet.remove id set
  | App (t1, t2)
  | Cast (t1, t2) ->
      IdSet.union (get_fv t1) (get_fv t2)

let rec is_free x t =
  match fst t with
  | Var id-> fst id = x
  | Lam (id, t1, t2)
  | Prod (id, t1, t2)
  | Let (id, t1, t2) ->
      id != x && (is_free x t1 || is_free x t2)
  | App (t1, t2)
  | Cast (t1, t2) ->
      is_free x t1 || is_free x t2

let rec proof_size = function
  | Axiom _ -> 1
  | Start (t, _) -> 1 + (proof_size t)
  | Weakening (_, t1, t2, _)
  | Product (t1, t2, _)
  | Application (t1, t2, _)
  | LetIntro (t1, t2, _)
  | Conversion (t1, _, _, t2, _) ->
      1 + (proof_size t1) + (proof_size t2)
  | Abstraction (t1, t2, t3, _) ->
      1 + (proof_size t1) + (proof_size t2) + (proof_size t3)


(** {5 Major PTS : the λ-cube} *)

let sort = IdSet.of_list ["*"; "□ "]
let axioms = IdMap.add "*" "□ " IdMap.empty
let r_stlc = Id2Map.add ("*", "*") "*" Id2Map.empty
let r_f = Id2Map.add ("□ ", "*") "*" r_stlc
let r_fw = Id2Map.add ("□ ", "□ ") "□ " r_f
let r_cc = Id2Map.add ("*", "□ ") "□ " r_fw

let stlc =
  { sorts  = sort   ;
    axioms = axioms ;
    rules  = r_stlc }
let syst_f =
  { stlc with
    rules  = r_f }
let syst_fw =
  { stlc with
    rules = r_fw }
let cc =
  { stlc with
    rules = r_cc }

(** {5 Some paradoxal PTS} *)

let lamb_star =
  { sorts = IdSet.singleton "*";
    axioms = IdMap.singleton "*" "*";
    rules = Id2Map.singleton ("*", "*") "*" }

let sortU = IdSet.add "△ " sort
let axiomsU = IdMap.add "□ " "△ " axioms
let r_U_minus = Id2Map.add ("△ ", "□ ") "□ " r_fw
let r_U = Id2Map.add ("△ ", "*") "*" r_U_minus
let r_HOL = Id2Map.add ("△ ", "*") "*" r_fw

let syst_HOL =
  { sorts  = sortU     ;
    axioms = axiomsU   ;
    rules  = r_HOL     }

let syst_U_minus =
  { sorts  = sortU     ;
    axioms = axiomsU   ;
    rules  = r_U_minus }

let syst_U =
  { sorts  = sortU   ;
    axioms = axiomsU ;
    rules  = r_U     }

