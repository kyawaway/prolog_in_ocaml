(lang dune 3.2)

(name prolog_in_ocaml)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name prolog_in_ocaml)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune)
 (tags
  (topics "to describe" your project)))

(using menhir 2.1)
; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
