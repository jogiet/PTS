open Ast

let gen_type_id t = 
  Prod (
    t,
    Var "*",
    Prod (
      "-",
      Var t,
      Var t
    )
  )

let type_id = gen_type_id "α"

let id =
  Lam ("α", Var "*", 
    Lam ("x", Var "α", Var "x")
  )

let id_id = 
  App (
    Lam ("x", type_id, Var "x"), 
    id
  )

let lx_xx =
  Lam ("x", type_id,
    App (
      App (Var "x", gen_type_id "β"),
      Var "x"
    )
  )

let lx_xx_lx_x = App (lx_xx, id)
    
