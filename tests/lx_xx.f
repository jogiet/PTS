-- This example shows that we can type the term \lambx.xx in system F.
-- it is the identity function of the identity function
\lamb (x: (\P (α : *). \P (s: α). α)). x (\P (α: *).\P (x : α).  α) x
