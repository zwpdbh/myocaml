# How to Start 

## Create switch 

Each switch is an isolated OCaml environment with its own compiler version and set of packages.

```sh 
# create switch 
opam switch create myocaml 5.3.0
opam install ocamlformat ocaml-lsp-server base core core_bench
eval $(opam env)


# remove installed switch 
opam switch list 
opam switch remove 
```


## How to quickly load addition libraries into utop 

Edit `~/.ocamlinit` to be:

```ocaml
#use "topfind";;
#directory "_build/default";;
#require "base";;
#require "core";;
open Base;;
open Core;;
#thread;;
```

This will load the `base` and `core` libraries whenever you start UTOP.

## How to load file in utop 

```ocaml 
#use "real_world_ocaml/ch02/ch02.ml";;
```

The path is relative to your project root, which is indicated by `dune-project` file.

## How to use libraries created in current project?
