
let bool = ∀ (α : *). α → α → α in
let true: bool = λ (α : *). λ (x y: α). x in
let false: bool = λ (α : *). λ (x y: α). y in
let and = λ (b1 b2: bool). b1 bool b2 false in
let or = λ (b1 b2: bool). b1 bool true b2 in
     or (and true false) (or true false)
