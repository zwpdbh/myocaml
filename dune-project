(lang dune 3.17)

(name myocaml)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name <author@example.com>")

(maintainers "Maintainer Name <maintainer@example.com>")

(license MIT)

(documentation https://url/to/documentation)

(package
 (name myocaml)
 (synopsis "A collection of OCaml examples and exercises")
 ; install them by opam install . --deps-only
 (depends
  ocaml
  dune
  ocaml-lsp-server
  odoc
  ocamlformat
  utop
  base
  core
  core_unix
  core_bench
  uri
  yojson
  ppx_deriving_yojson
  cohttp
  cohttp-async
  async_ssl
  textwrap
  ppx_jane))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
