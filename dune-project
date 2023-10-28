(lang dune 3.10)

(name ocaml99)

(generate_opam_files true)

(source
 (github EthanOlpin/ocaml99))

(authors "Ethan Olpin")

(maintainers "Ethan Olpin")

(license LICENSE)

(package
 (name ocaml99)
 (synopsis "99 Problems solved in OCaml")
 (description "https://v2.ocaml.org/learn/tutorials/99problems.html")
 (depends ocaml dune)
 (tags (ocaml99)))
