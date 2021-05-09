-- list definition with the two "operators"
-- the list is equivalent to fold_right.
let list = λ α : *. ∀ γ: *. γ → (α → γ → γ) → γ in
let empty: ∀ α: *. list α = λ α : *. λ γ: *. λ x: γ. λ f: α → γ → γ. x in
let cons: ∀ α: *. α → (list α) → (list α) 
  = λ α : *. λ t: α. λ l: list α. λ γ: *. λ x: γ. λ f: α → γ → γ. f t (l γ x f) in

-- For the length function we need the nat type
let nat = ∀ α: *. α → (α → α) → α in

let length: ∀ α: *. (list α) → nat
  = let Z: nat
    = λ α: *. λ x: α. λ f: α → α. x in
    let S: nat → nat
    = λ n: nat. λ α: *. λ x: α. λ f: α → α. n α (f x) f in
    λ α: *. λ l: list α. l nat Z (λ x: α. S) in

-- For the "head" construction we need the option type
let option = λ α: *. ∀ γ: *. γ → (α → γ) → γ in

let head: ∀ α: *. (list α) → (option α)
  = let None: ∀ α: *. option α
    = λ α: *. λ γ: *. λ y: γ. λ f: α → γ. y in
    let Some: ∀ α: *. α → (option α)
    = λ α: *. λ x: α. λ γ: *. λ y: γ. λ f: α → γ. f x in
    λ α: *. λ l : list α. l (option α) (None α) λ t: α. λ x: option α. Some α t in



let l  = cons (∀ α: *. list α) empty (empty ∀ α: *. list α) in
length (∀(α:*).list α) l
