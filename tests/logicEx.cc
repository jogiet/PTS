let exist = λ A: *. λ P: A -> *. ∀ Q: *. (∀ x: A. (P x) -> Q) -> Q in
let exIntro : ∀ A: *. ∀ P: A -> *. ∀ x: A. (P x) -> (exist A P)
  = λ A: *. λ P: A -> *. λ t: A. λ Hpt: (P t).
      λ Q: *. λ H: (∀ x: A. (P x) -> Q). H t Hpt in
let example: ∀ X: *. ∀ R: X -> X -> *. 
  (exist X (λ x: X. ∀ y: X. R x y)) ->
  (∀ y: X. exist X (λ x: X. R x y))
  = λ X: *. λ R: X -> X -> *. λ Hex : exist X (λ x: X. ∀ y2: X. R x y2).
      λ y: X. 
        Hex 
          (exist X (λ x: X. R x y)) 
          (λ x: X. λ (Hxy : ∀ y1: X. R x y1). exIntro X (λ x: X. R x y) x (Hxy y))
in example
