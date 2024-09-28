open B0_kit.V000

(* OCaml library names *)

let astring = B0_ocaml.libname "astring"
let b0_std = B0_ocaml.libname "b0.std"
let compiler_libs_toplevel = B0_ocaml.libname "compiler-libs.toplevel"

let fpath = B0_ocaml.libname "fpath"
let fpath_top = B0_ocaml.libname "fpath.top"

(* Libraries *)

let fpath_lib =
  let srcs = [`Dir ~/"src/"; `X ~/"src/fpath_top_init.ml"] in
  B0_ocaml.lib fpath ~srcs ~requires:[astring]

let fpath_top_lib =
  let srcs = [ `Dir ~/"src/top" ] in
  B0_ocaml.lib fpath_top ~srcs ~requires:[fpath; compiler_libs_toplevel]

(* Tests *)

let test_fpath =
  let srcs = [ `Dir ~/"test" ] in
  B0_ocaml.test ~/"test/test.ml" ~srcs ~requires:[b0_std; fpath]

(* Packs *)

let default =
  let meta =
    B0_meta.empty
    |> ~~ B0_meta.authors ["The fpath programmers"]
    |> ~~ B0_meta.maintainers ["Daniel Bünzli <daniel.buenzl i@erratique.ch>"]
    |> ~~ B0_meta.homepage "https://erratique.ch/software/fpath"
    |> ~~ B0_meta.online_doc "https://erratique.ch/software/fpath/doc/"
    |> ~~ B0_meta.licenses ["ISC" ]
    |> ~~ B0_meta.repo "git+https://erratique.ch/repos/fpath.git"
    |> ~~ B0_meta.issues "https://github.com/dbuenzli/fpath/issues"
    |> ~~ B0_meta.description_tags
      ["file"; "system"; "path"; "org:erratique"]
    |> ~~ B0_opam.depends
      [ "ocaml", {|>= "4.08.0"|};
        "ocamlfind", {|build|};
        "ocamlbuild", {|build|};
        "topkg", {|build & >= "1.0.3"|};
      ]
    |> ~~ B0_opam.build
      {|[["ocaml" "pkg/pkg.ml" "build" "--dev-pkg" "%{dev}%"]]|}
    |> B0_meta.tag B0_opam.tag
  in
  B0_pack.make "default" ~doc:"fpath package" ~meta ~locked:true @@
  B0_unit.list ()
