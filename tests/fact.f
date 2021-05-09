let nat = ∀ (α: *). (α → α) → α → α in
let Z = λ α : *. λ f: α → α. λ x: α. x in
let S = λ(n: nat). λ(α : *). λ(f: α → α). λ (x: α). n α f (f x) in

let add: nat -> nat -> nat = \ n m: nat. n nat S m in
let mult: nat -> nat -> nat = \ n m: nat. n nat (add m) Z in

let pair = ∀(γ:*).(nat → nat → γ) → γ in

let pairc: nat -> nat -> pair =
  λ x y: nat. λ γ: *. λ f: nat -> nat → γ. f x y in

let fst: pair → nat =
  λ p: pair. p nat (λ x y: nat. x) in

let snd: pair -> nat =
  λ p: pair. p nat (λ x y: nat. y) in


-- the recursion scheme concerns all functions f: nat -> nat that can be defined
-- with the following equations :
--   f 0 = c_0
--   f (S n) = g (f n) (S n)

let nat_r: nat -> (nat -> nat -> nat) -> nat -> nat =
	\ c_0: nat. \ g:  nat -> nat -> nat. \ n: nat.
	let tuple_0: pair =
		pairc Z c_0 in
	let f_tuple: pair -> pair =
		\ p: pair. 
		let p1: nat = fst p in
		let p2: nat = snd p in
		pairc (S p1) (g p2 (S p1)) in
  let tuple_n: pair =
		n pair f_tuple tuple_0 in
	snd tuple_n in

let fact: nat -> nat = nat_r (S Z) mult in

let quatre = S (S (S (S Z))) in 

fact quatre
        
