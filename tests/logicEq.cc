-- Here we see equality as indescernability.
-- In major proof assistant (e.g. Coq), equality and indescernability are equivalents
let equal = λ a: *. λ x y: a. ∀ p: a -> *. (p x) -> (p y) in

-- first lemma : equality is reflexive
let eqRefl : ∀ a: *. ∀ x: a. equal a x x
  = λ a: *. λ x: a. λ p: a -> *. λ proof: p x. proof in

-- second lemma : equality is transitive  
let eqTrans : ∀ a: *. ∀ x y z: a. (equal a x y) -> (equal a y z) -> (equal a x z)
  = λ a: *. λ x y z: a.
     λ Hxy: equal a x y. λ Hyz: equal a y z.
     λ p: a -> *. λ Hpx: p x. Hyz p (Hxy p Hpx) in

-- final Lemma : equality is symmetric.
-- The proof consists to apply the «x = y» hypothesis on the «H := z ↦ z = x»
-- property since «H x» obviously (by reflexivity) then so «H y». 
let eqSym : ∀ a: *. ∀ x y: a. (equal a x y) -> (equal a y x)
  = λ a: *. λ x y: a. λ Hxy: equal a x y.
      Hxy (λ (z: a). equal a z x) (eqRefl a x) in

-- One final observation : because of the conversion rule, 
-- equality holds modulo αβ-conversion

let nat = ∀ α : *. (α -> α) -> α -> α in
let Z = λ α : *. λ f: α -> α. λ x: α. x in
let succ = λ(n: nat). λ(α : *). λ(f: α -> α). λ (x: α). n α f (f x) in
let add = λ(n : nat). λ(m: nat). n nat succ m in
let 2 = succ (succ Z) in
let 4 = succ (succ (succ (succ Z))) in

let example: equal nat (add 2 2) 4 = eqRefl nat 4 in
  example
