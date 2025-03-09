(lang dune 3.17)

(name myocaml)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name <author@example.com>")

(maintainers "Maintainer Name <maintainer@example.com>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name myocaml)
 (depends
  ocaml
  dune
  (ocaml-lsp-server :dev)
  (odoc :dev)
  (ocamlformat :dev)
  (utop :dev)
  base
  core
  core_unix
  core_bench))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
