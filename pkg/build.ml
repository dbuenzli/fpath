#!/usr/bin/env ocaml
#directory "pkg";;
#use "topkg.ml";;

let () =
  Pkg.describe "fpath" ~builder:(`OCamlbuild []) [
    Pkg.lib "pkg/META";
    Pkg.lib ~exts:Exts.module_library "src/fpath";
    Pkg.lib ~exts:Exts.library "src/fpath_top";
    Pkg.doc "README.md";
    Pkg.doc "CHANGES.md"; ]
