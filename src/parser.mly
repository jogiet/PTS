/*              Parser for Lambda-Calculus         */

%{

open Ast

let make_syst sorts axioms rules =
  let sorts = IdSet.of_list sorts in
  let axioms = List.fold_left
    (fun acc (s1, s2) ->
      if IdMap.mem s1 acc then
        let err = Format.sprintf "%s has type %s & %s!"
          s1 s2 (IdMap.find s1 acc) in
        raise (Not_functionnal err)
      else IdMap.add s1 s2 acc )
    IdMap.empty
    axioms
  in
  let rules = List.fold_left
    (fun acc (s1, s2, s3) ->
      if Id2Map.mem (s1, s2) acc then
        let err = Format.sprintf "(%s, %s) has rule %s & %s!"
          s1 s2 s3 (Id2Map.find (s1, s2) acc) in
        raise (Not_functionnal err)
      else Id2Map.add (s1, s2) s3 acc )
    Id2Map.empty
    rules
  in
  { sorts  = sorts   ;
    axioms = axioms ;
    rules  = rules  }

%}

%token S A R
%token LAMBDA 
%token FORALL 
%token LET IN
%token ARROW
%token EQUAL
%token EOF
%token LPAR RPAR LCB RCB DDOT DOT COMMA SEMI

%token <Ast.ident> IDENT

%start file
%start system

%type <(Ast.term Ast.located) * (Ast.system option)> file
%type <Ast.system> system

%%

file: 
    | l = application; EOF
		{l, None}
    | s = system; l = application; EOF
    {l, Some s}

system:
    | s = sorts; a = axioms; r = rules
    { make_syst s a r }

sorts:
    | S; EQUAL; LCB; s = separated_nonempty_list(SEMI, ident); RCB
    { s }

axioms:
    | A; EQUAL; LCB; a = separated_nonempty_list(SEMI, axiom); RCB
    { a }
   
axiom:
    | s1 = ident; DDOT; s2 = ident
    { s1, s2 }
    | LPAR; s1 = ident; DDOT; s2 = ident; RPAR
    { s1, s2 }

rules:
    | R; EQUAL; LCB; r = separated_nonempty_list(SEMI, rule); RCB
    { r }
   
rule:
    | s1 = ident; COMMA; s2 = ident; COMMA; s3 = ident
    { s1, s2, s3 }
    | LPAR; s1 = ident; COMMA; s2 = ident; COMMA; s3 = ident RPAR
    { s1, s2, s3 }

application:
    | l = atom_app+;
    { match l with
      | [] -> assert false
      | t::q ->
          List.fold_left 
            (fun l1 l2 -> App (l1, l2), (fst @@ snd l1, snd @@ snd l2 ))
            t q
    }

atom_app :
    | LPAR; a = application; RPAR {a}
    | a = abstraction {a}
    | v = var {v}

atom_arrow :
    | LPAR; a = application; RPAR {a}
    | v = var {v}

abstraction :
  | LAMBDA; LPAR ; li = ident+; DDOT; ty = application; RPAR ; DOT; t = application
    {List.fold_right (fun i acc -> Lam (i, ty, acc), ($startpos, $endpos)) li t }
  | LAMBDA; li = ident+; DDOT; ty = application; DOT; t = application
    {List.fold_right (fun i acc -> Lam (i, ty, acc), ($startpos, $endpos)) li t }
  | FORALL; LPAR ; li = ident+; DDOT; ty = application; RPAR ; DOT; t = application
    {List.fold_right (fun i acc -> Prod (i, ty, acc), ($startpos, $endpos)) li t }
  | FORALL; li = ident+; DDOT; ty = application; DOT; t = application
    {List.fold_right (fun i acc -> Prod (i, ty, acc), ($startpos, $endpos)) li t }
  | LET; i = ident; EQUAL; d = application; IN; l = application
    {Let (i, d, l), ($startpos, $endpos) }
  | LET; i = ident; DDOT; typ = application EQUAL; d = application; IN; l = application
    { let pos = $startpos, $endpos in
      Let (i, (Cast (d, typ), pos), l), pos }
  | l_arrow = separated_nonempty_list(ARROW, atom_arrow);
    { let rec aux = function
      | [] -> assert false
      | [x] -> x
      | t::q -> Prod ("-", t, aux q), (fst @@ snd t, $endpos) in
      aux l_arrow 
    }

ident:
	| i = IDENT 
		{i}

var :
	| i = ident 
    { let pos = $startpos, $endpos in
    Var (i, pos), pos }

