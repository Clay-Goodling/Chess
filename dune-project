(lang dune 3.6)

(name chess)

(generate_opam_files true)

(source
 (github Clay-Goodling/Chess))

(authors "Clay Goodling")

(maintainers "Clay Goodling")

(license MIT)

(documentation https:goodling.dev/Chess)

(package
 (name chess)
 (synopsis "An OCaml chess engine")
 (description "An OCaml chess engine")
 (depends ocaml dune odoc ANSITerminal)
 (tags
  ("chess")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
