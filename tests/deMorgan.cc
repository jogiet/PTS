-- De Morgan Law
-- Comme on n'est dans une logique intuitionniste on ne peut prouver qu'un sens : 
-- ∀ A B, ~(A \/ B) -> (~A /λ ~ B)
let false = ∀P: *. P in

-- la négation d'une propriété est définie comme P-> Faux
let neg = λ P: *. P -> false in

-- le type de «ou» est son destructeur
let or = λ A B: *. ∀ P: *. (A -> P) -> (B -> P) -> P in
let orLeft: ∀ (A B: *). A -> (or A B)
  = λ A B: *. λ Ha: A. 
    λ (Q: *). λ(fa: A -> Q). λ(fb: B -> Q). fa Ha in 
let orRight: ∀ (A B: *). B -> (or A B)
  = λ A B: *. λ Hb: B. 
    λ (Q: *). λ(fa: A -> Q). λ(fb: B -> Q). fb Hb in 

-- idem pour le «et»
let and = λ A B: *. ∀ P: *. (A -> B -> P) -> P in
let andIntro : ∀ (A B: *). A -> B -> (and A B)
  = λ A B: *. λ Ha: A. λ Hb: B.
    λ (Q: *). λ(f: A -> B -> Q). f Ha Hb in
let andLeft: ∀ A B: *. (and A B) -> A 
	= λ A B: *. λ Hab: and A B. 
			Hab A (λ Ha: A. λ Hb: B. Ha) in
let andRight: ∀ A B: *. (and A B) -> B 
	= λ A B: *. λ Hab: and A B. 
			Hab B (λ Ha: A. λ Hb: B. Hb) in
 

let demorgan1: ∀ A B: * . (neg (or A B)) -> (and (neg A) (neg B))
  = λ (A B: *). λ Hnab : (neg (or A B)).
       andIntro (neg A) (neg B)
         (λ Ha: A. Hnab (orLeft A B Ha))
         (λ Hb: B. Hnab (orRight A B Hb))

in
let demorgan2: ∀ A B: * . (and (neg A) (neg B)) -> (neg (or A B)) 
  = λ (A B: *). λ Hnanb : (and (neg A) (neg B)). λ Hab:	(or A B).
			Hab false 
				(andLeft (neg A) (neg B) Hnanb)
				(andRight (neg A) (neg B) Hnanb)

in neg
