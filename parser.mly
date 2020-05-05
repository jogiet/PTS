/*              Parser for Lambda-Calculus         */

%{

open Ast

%}


%token LAMBDA 
%token FORALL
%token ARROW
%token EOF
%token LPAR RPAR DDOT DOT

%token <Ast.ident> IDENT

%start file

%type <Ast.term> file

%%

file: 
    | l = application; EOF
		{l}

application:
    | l = atom_app+;
    { match l with
      | [] -> assert false
      | t::q ->
          List.fold_left (fun l1 l2 -> App (l1, l2)) t q
    }

atom_app :
    | LPAR; a = application; RPAR {a}
    | a = abstraction {a}
    | v = var {v}

atom_arrow :
    | LPAR; a = application; RPAR {a}
    | v = var {v}

abstraction :
  | LAMBDA; LPAR ; i = ident; DDOT; t = application; RPAR ; DOT; l = application
		{Lam (i, t, l)}
  | LAMBDA; i = ident; DDOT; t = application; DOT; l = application
		{Lam (i, t, l)}
  | FORALL; LPAR ; i = ident; DDOT; t = application; RPAR ; DOT; l = application
		{Prod (i, t, l)}
  | FORALL; i = ident; DDOT; t = application; DOT; l = application
		{Prod (i, t, l)}
  | l_arrow = separated_nonempty_list(ARROW, atom_arrow);
    { let rec aux = function
      | [] -> assert false
      | [x] -> x
      | t::q -> Prod ("_", t, aux q) in
      aux l_arrow 
    }

ident:
	| i = IDENT 
		{i}

var :
	| i = ident 
		{Var i}

