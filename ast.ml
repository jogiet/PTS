(** This file contains the AST of Pure Type Systems as well as the datatype used
  in type checking and also some utilitatries functions *)

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
  | Var of ident                    (** [Var x] is variable [x] *)
  | App of (term * term)            (** Function application *)
  | Lam of (ident * term * term)    
  (** Lambda abstraction : [Lam (x, A, B)] : λ(x: A).B *)
  | Prod of (ident * term * term)   
  (** Product abstraction : [Prod (x, A, B)] : ∀(x: A). B *)
  | Let of (ident * term * term)

  (** We assume that a system is functionnal *)
type system = 
  { sorts:  IdSet.t  ;
    (** A set of sort: 𝒮 *)
    axioms: ident IdMap.t ; 
    (** A set of axioms: 𝒜  ⊆ 𝒮 ² *)
    rules:  ident Id2Map.t 
    (** A set of rules: ℛ ⊆ 𝒮 ³ *)
  }

(** {5 Typing} *)

type typing_def = term IdMap.t

  (** A typing environment is a map [x] ↦ [term].
   Sort should not appear in typing environment *)
type typing_env = (ident * term) list
    
type typing_judgement = typing_def * typing_env * term * term

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
      (typing_tree * term * term * typing_tree * typing_judgement)

(** {5 Utilities} *)

let get_judgment = function
  | Axiom j -> j
  | Weakening (_, _, _, j) -> j
  | Start (_, j) -> j
  | Product (_, _, j) -> j
  | Abstraction (_, _, _, j) -> j
  | LetIntro (_, _, j) -> j
  | Application (_, _, j) -> j
  | Conversion (_, _, _, _, j) -> j

let rec get_fv = function
  | Var id -> IdSet.singleton id
  | Lam (id, t1, t2)
  | Prod (id, t1, t2)
  | Let (id, t1, t2) ->
      let set = IdSet.union (get_fv t1) (get_fv t2) in
      IdSet.remove id set
  | App (t1, t2) ->
      IdSet.union (get_fv t1) (get_fv t2)

let rec is_free x = function
  | Var id-> id = x
  | Lam (id, t1, t2)
  | Prod (id, t1, t2)
  | Let (id, t1, t2) ->
      id != x && (is_free x t1 || is_free x t2)
  | App (t1, t2) ->
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

