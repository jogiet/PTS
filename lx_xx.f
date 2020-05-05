-- This example shows that we can type the term \lambx.xx in system F.
-- it is the identity function of the identity function
\lamb (x: (\P (alpha : *). \P (s: alpha). alpha)). x (\P (a: *).\P (_ : a).  a) x
