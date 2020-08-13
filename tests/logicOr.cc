let false = ∀p: *. p in
let neg = λ P: *. P -> false in
let or = λ P Q: *. ∀ R: *. (P -> R) -> (Q -> R) -> R in
let leftOr: ∀ P Q: *. P -> (or P Q)
  = λ P Q: *. λ Hp: P.
      λ R: *. λ fp: P -> R. λ fq: Q -> R. fp Hp in
let rightOr: ∀ P Q: *. Q -> (or P Q)
  = λ P Q: *. λ Hq: Q.
      λ R: *. λ fp: P -> R. λ fq: Q -> R. fq Hq in
let NNem: ∀ P: *. neg (neg ( or P (neg P)))
  = λ P: *. λ Hnem: neg (or P (neg P)).
      Hnem (rightOr P (neg P) (λ Hp: P. Hnem (leftOr P (neg P) Hp))) in
NNem
