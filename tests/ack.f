-- Encoding the ackermann function
-- Formal definition : 
-- Ack : 0,   n   -> n+1
--     : m+1, 0   -> Ack(m, 1)
--     : m+1, n+1 -> Ack(m, Ack(m+1, n+1))
-- So we use the foolowing encoding, with curryfication
-- Iter : f , 0   -> f 1
--      : f , n+1 -> f (Iter f n)
-- Ack  : 0    -> S
--      : n+1  -> Iter(Ack n)

let nat = ∀ (α: *). (α -> α) -> α -> α in
let Z = λ α : *. λ f: α -> α. λ x: α. x in
let S = λ(n: nat). λ(α : *). λ(f: α -> α). λ (x: α). n α f (f x) in
let Un = S Z in
let iter = λ(f: nat -> nat). λ (n: nat). n nat f (f Un) in
let ack = λ (m: nat). λ (n: nat). (m (nat -> nat) iter S) n in
  ack (S (S Un)) (S (S Un))
