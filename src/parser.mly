/*              Parser for Lambda-Calculus         */

%{

open Ast

%}


%token LAMBDA 
%token FORALL 
%token LET IN
%token ARROW
%token EQUAL
%token EOF
%token LPAR RPAR DDOT DOT

%token <Ast.ident> IDENT

%start file

%type <(Ast.term Ast.located) * (Ast.system option)> file

%%

file: 
    | l = application; EOF
		{l, None}

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

