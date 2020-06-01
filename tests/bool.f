
let bool = ∀ (α : *). α → α → α in
let true = λ (α : *). λ (x: α). λ (y: α). x in
let false = λ (α : *). λ (x: α). λ (y: α). y in
let and = λ (b1: bool). λ (b2: bool). b1 bool b2 false in
let or = λ (b1: bool). λ (b2: bool). b1 bool true b2 in
      or (and true false) (or true false)
