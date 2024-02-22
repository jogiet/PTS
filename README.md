PTS : une Perte de Temps Supreme
===============================

This project is an interpretor for Pure Type Systems, a family a typed
λ-calculi.

**You can try it online on the [github pages](https://jogiet.github.io/PTS)**

Installation
------------

0. `opam install . -y --deps-only`
1. `dune build`

Description
-----------

This interpretor :

- check if the file is well typed (with the option `type_debug`, it prints on
	stdout the typing judgments used, and with the option `--get-proof`, it prints
	the proof on a `.tex` file)
- If the file is well typed, then it computes its normal form

Supported extension :

- `.stlc` for simply typed λ-calculus
- `.f` for System F (simply-typed λ-calulus with polymorphism)
- `.fw` for System Fω (System F with type constructor)
- `.cc` for the Calculus of Constructions
- `.u` for the System U (beware that it's an inconsistent system)

![](https://upload.wikimedia.org/wikipedia/commons/c/cd/Lambda_Cube_img.svg)

Type `./pts.exe -help` to see the list of options.

TODO:

- adding axioms (e.g. for λ0)

