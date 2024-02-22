(lang dune 2.5)
(name pts)
(using menhir 2.1)

(maintainers "Josselin Giet <josselin.giet@ens.fr>")
(authors "Josselin Giet <josselin.giet@ens.fr>")

(source (github jogiet/PTS))

(generate_opam_files true)

(license MIT)

(package
 (name pts)
 (synopsis "A Pure Type System interpreter")
 (description "A toy parser/typer/interpreter for pure type systems")
 (depends (ocaml (>= 4.08)) js_of_ocaml menhir)
)