-- First test to work with Church arithmetic
let nat = ∀ α : *. (α -> α) -> α -> α in
let Z = λ α : *. λ f: α -> α. λ x: α. x in
let succ = λ(n: nat). λ(α : *). λ(f: α -> α). λ (x: α). n α f (f x) in
let add = λ(n : nat). λ(m: nat). n nat succ m in
let mult = λ(n: nat). λ(m: nat). n nat (add m) Z in
let deux = succ (succ Z) in
  mult deux deux
      
