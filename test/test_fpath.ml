(*---------------------------------------------------------------------------
   Copyright (c) 2015 The fpath programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_testing

let windows = Sys.os_type = "Win32"

let eq = Test.eq (module Fpath)
let v = Fpath.v

let of_string () =
  Test.test "Fpath.{v,of_string}" @@ fun () ->
  let eq = Test.eq (Test.Eq.result ~ok:(module Fpath) ~error:Test.Eq.true') in
  let ok s = Ok (v s) in
  let error = Error (`Msg "any") in
  eq (Fpath.of_string "/\x00") error ~__POS__;
  eq (Fpath.of_string "/") (ok "/") ~__POS__;
  Test.bool (Fpath.equal (v "/") (v "/ ")) false ~__POS__;
  eq (Fpath.of_string "//") (if windows then error else ok "//") ~__POS__;
  eq (Fpath.of_string "/a/b/c") (ok "/a/b/c") ~__POS__;
  Test.bool (Fpath.equal (v "/a/b/c/") (v "/a/b/c")) false ~__POS__;
  eq (Fpath.of_string "") error ~__POS__; (* no empty path *)
  eq (Fpath.of_string "a///b///////c///") (ok "a/b/c/") ~__POS__;
  eq (Fpath.of_string "a///b///////c") (ok "a/b/c") ~__POS__; (* seg collapse *)
  if windows then begin
    eq (Fpath.of_string "C:\x00") error ~__POS__;
    eq (Fpath.of_string "C:") error ~__POS__; (* no empty path *)
    eq (Fpath.of_string "C:\\") (ok "C:\\") ~__POS__;
    eq (Fpath.of_string "C:rel") (ok "C:rel") ~__POS__;
    eq (Fpath.of_string "\\\\") error ~__POS__;
    eq (Fpath.of_string "\\\\server") error ~__POS__;
    eq (Fpath.of_string "\\\\server\\") error ~__POS__;
    eq (Fpath.of_string "\\\\server\\share")
      (ok "\\\\server\\share\\") (* root add *) ~__POS__;
    eq (Fpath.of_string "\\\\?") error ~__POS__;
    eq (Fpath.of_string "\\\\?\\") error ~__POS__;
    eq (Fpath.of_string "\\\\?\\a") error ~__POS__;
    eq (Fpath.of_string "\\\\?\\a:") (ok "\\\\?\\a:\\") ~__POS__; (* root add *)
    eq (Fpath.of_string "\\\\?\\a:\\") (ok "\\\\?\\a:\\") ~__POS__;
    eq (Fpath.of_string "\\\\?\\a:\\c") (ok "\\\\?\\a:\\c") ~__POS__;
    eq (Fpath.of_string "\\\\?\\server\\") error ~__POS__;
    eq (Fpath.of_string "\\\\?\\server\\\\") error ~__POS__;
    eq (Fpath.of_string "\\\\?\\server\\share")
      (ok "\\\\?\\server\\share\\") ~__POS__; (* root add *)
    eq (Fpath.of_string "\\\\?\\server\\\\share")
      (ok "\\\\?\\server\\share\\") ~__POS__; (* seg collapse and root add *)
    eq (Fpath.of_string "\\\\?\\server\\share\\")
      (ok "\\\\?\\server\\share\\") ~__POS__;
    eq (Fpath.of_string "\\\\?\\server\\share\\a")
      (ok "\\\\?\\server\\share\\a") ~__POS__;
    eq (Fpath.of_string "\\\\?\\UNC") error ~__POS__;
    eq (Fpath.of_string "\\\\?\\UNC\\") error ~__POS__;
    eq (Fpath.of_string "\\\\?\\UNC\\server") error ~__POS__;
    eq (Fpath.of_string "\\\\?\\UNC\\server\\") error ~__POS__;
    eq (Fpath.of_string "\\\\?\\UNC\\server\\\\") error ~__POS__;
    eq (Fpath.of_string "\\\\?\\UNC\\server\\share")
      (ok "\\\\?\\UNC\\server\\share\\") ~__POS__; (* root add *)
    eq (Fpath.of_string "\\\\?\\UNC\\server\\share\\")
      (ok "\\\\?\\UNC\\server\\share\\") ~__POS__;
    eq (Fpath.of_string "\\\\?\\UNC\\server\\share\\a")
      (ok "\\\\?\\UNC\\server\\share\\a") ~__POS__;
    eq (Fpath.of_string "\\\\.") error ~__POS__;
    eq (Fpath.of_string "\\\\.\\") error ~__POS__;
    eq (Fpath.of_string "\\\\.\\device")
      (ok "\\\\.\\device\\")(* root add *) ~__POS__;
    eq (Fpath.of_string "\\\\.\\device\\") (ok "\\\\.\\device\\") ~__POS__;
    eq (Fpath.of_string "\\\\.\\device\\a") (ok "\\\\.\\device\\a") ~__POS__;
  end;
  ()

let dir_sep () =
  Test.test "Fpath.dir_sep" @@ fun () ->
  Test.string Fpath.dir_sep (if windows then "\\" else "/") ~__POS__;
  ()

let is_seg () =
  Test.test "Fpath.is_seg" @@ fun () ->
  Test.bool (Fpath.is_seg "abc") true ~__POS__;
  Test.bool (Fpath.is_seg "ab/c") false ~__POS__;
  Test.bool (Fpath.is_seg "ab\x00c") false ~__POS__;
  if windows then Test.bool (Fpath.is_seg "ab\\c") false ~__POS__;
  ()

let add_seg () =
  Test.test "Fpath.add_seg" @@ fun () ->
  Test.invalid_arg ~__POS__  (fun () -> Fpath.add_seg (v "a/b/c") "a\x00o");
  Test.invalid_arg ~__POS__  (fun () -> Fpath.add_seg (v "a/b/c") "a/o");
  if windows then
    Test.invalid_arg ~__POS__ (fun () -> Fpath.add_seg (v "a/b/c") "a\\o");
  eq (Fpath.add_seg (v "/a") "b") (v "/a/b") ~__POS__;
  eq (Fpath.add_seg (v "/a/") "b") (v "/a/b") ~__POS__;
  eq (Fpath.add_seg (v "a/b") "") (v "a/b/") ~__POS__;
  eq (Fpath.add_seg (v "a/b/") "") (v "a/b/") ~__POS__;
  eq (Fpath.add_seg (v "/a/b") "") (v "/a/b/") ~__POS__;
  eq (Fpath.add_seg (v "/a/b/") "") (v "/a/b/") ~__POS__;
  eq (Fpath.add_seg (v "/a/b/") "e") (v "/a/b/e") ~__POS__;
  eq (Fpath.add_seg (v "/a/b") "e") (v "/a/b/e") ~__POS__;
  eq (Fpath.add_seg (v "/") "") (v "/") ~__POS__;
  eq (Fpath.add_seg (v "/") "a") (v "/a") ~__POS__;
  eq (Fpath.add_seg (v ".") "a") (v "./a") ~__POS__;
  eq (Fpath.add_seg (v ".") "") (v "./") ~__POS__;
  eq (Fpath.add_seg (v "..") "a") (v "../a") ~__POS__;
  eq (Fpath.add_seg (v "..") "") (v "../") ~__POS__;
  ()

let append () =
  Test.test "Fpath.append" @@ fun () ->
  eq (Fpath.append (v "/a/b/") (v "e/f")) (v "/a/b/e/f") ~__POS__;
  eq (Fpath.append (v "/a/b") (v "e/f")) (v "/a/b/e/f") ~__POS__;
  eq (Fpath.append (v "/a/b/") (v "/e/f")) (v "/e/f") ~__POS__;
  eq (Fpath.append (v "a/b/") (v "e/f")) (v "a/b/e/f") ~__POS__;
  eq (Fpath.append (v "bla") (v "/bli")) (v "/bli") ~__POS__;
  if not windows
  then eq (Fpath.append (v "bla") (v "//bli")) (v "//bli") ~__POS__;
  if windows then begin
    eq (Fpath.append (v "a/b") (v "C:e")) (v "C:e") ~__POS__;
    eq (Fpath.append (v "C:bla") (v "blu")) (v "C:bla/blu") ~__POS__;
    eq (Fpath.append (v "C:\\bla") (v "blu")) (v "C:\\bla\\blu") ~__POS__;
    eq (Fpath.append (v "C:\\bla") (v "\\blu")) (v "\\blu") ~__POS__;
    eq (Fpath.append (v "\\\\srv\\share\\a") (v "b"))
      (v "\\\\srv\\share\\a\\b") ~__POS__;
    eq (Fpath.append (v "\\\\srv\\share\\a\\") (v "b"))
      (v "\\\\srv\\share\\a\\b") ~__POS__;
  end;
  ()

let split_volume () =
  Test.test "Fpath.split_volume" @@ fun () ->
  let eq_split ?__POS__:pos p vol q =
    Test.block ?__POS__:pos @@ fun () ->
    let p = v p in
    let vol', q' = Fpath.split_volume p in
    Test.string vol vol' ~__POS__;
    eq (v q) q' ~__POS__;
    eq (v (vol' ^ (Fpath.to_string q'))) p ~__POS__;
  in
  eq_split "/bla" "" "/bla" ~__POS__;
  eq_split "bla" "" "bla" ~__POS__;
  eq_split "bla/a" "" "bla/a" ~__POS__;
  eq_split "bla/a/" "" "bla/a/" ~__POS__;
  if not windows then begin
    eq_split "//" "/" "/" ~__POS__;
    eq_split "//a/b/c" "/" "/a/b/c" ~__POS__;
    eq_split "//a/b/c/" "/" "/a/b/c/" ~__POS__;
  end;
  if windows then begin
    eq_split "C:." "C:" "." ~__POS__;
    eq_split "C:\\" "C:" "\\" ~__POS__;
    eq_split "C:\\a" "C:" "\\a" ~__POS__;
    eq_split "C:rel" "C:" "rel" ~__POS__;
    eq_split "\\\\server\\share\\" "\\\\server\\share" "\\" ~__POS__;
    eq_split "\\\\server\\share\\a" "\\\\server\\share" "\\a" ~__POS__;
    eq_split "\\\\?\\a:\\" "\\\\?\\a:" "\\" ~__POS__;
    eq_split "\\\\?\\a:\\c" "\\\\?\\a:" "\\c" ~__POS__;
    eq_split "\\\\?\\server\\share\\" "\\\\?\\server\\share" "\\" ~__POS__;
    eq_split "\\\\?\\server\\share\\a" "\\\\?\\server\\share" "\\a" ~__POS__;
    eq_split "\\\\?\\UNC\\server\\share\\" "\\\\?\\UNC\\server\\share" "\\"
      ~__POS__;
    eq_split "\\\\?\\UNC\\server\\share\\a" "\\\\?\\UNC\\server\\share" "\\a"
      ~__POS__;
    eq_split "\\\\.\\device\\" "\\\\.\\device" "\\" ~__POS__;
    eq_split "\\\\.\\device\\a" "\\\\.\\device" "\\a" ~__POS__;
  end;
  ()

let segs () =
  Test.test "Fpath.segs" @@ fun () ->
  let eq = Test.eq (Test.Eq.(list string)) in
  eq (Fpath.segs @@ v "/a/b/") [""; "a"; "b"; ""] ~__POS__;
  eq (Fpath.segs @@ v "/a/b") [""; "a"; "b"] ~__POS__;
  eq (Fpath.segs @@ v "a/b/") ["a"; "b"; ""] ~__POS__;
  eq (Fpath.segs @@ v "a/b") ["a"; "b"] ~__POS__;
  eq (Fpath.segs @@ v "a") ["a"] ~__POS__;
  eq (Fpath.segs @@ v "/") [""; ""] ~__POS__;
  eq (Fpath.segs @@ v "/a/b/c") [""; "a"; "b"; "c"] ~__POS__;
  eq (Fpath.segs @@ v "/a/b/c/") [""; "a"; "b"; "c"; ""] ~__POS__;
  eq (Fpath.segs @@ v "a/b/c") ["a"; "b"; "c";] ~__POS__;
  eq (Fpath.segs @@ v "a/b/c/") ["a"; "b"; "c"; ""] ~__POS__;
  if not windows then begin
    eq (Fpath.segs @@ v "//") [""; ""] ~__POS__;
    eq (Fpath.segs @@ v "//a/b") [""; "a"; "b"] ~__POS__;
  end;
  if windows then begin
    eq (Fpath.segs @@ v "C:\\bla") [""; "bla"] ~__POS__;
    eq (Fpath.segs @@ v "C:bla") ["bla"] ~__POS__;
    eq (Fpath.segs @@ v "\\\\Server\\share\\bla") [""; "bla"] ~__POS__;
    eq (Fpath.segs @@ v "\\\\?\\C:\\bla") ["";"bla"] ~__POS__;
    eq (Fpath.segs @@ v "\\\\?\\Server\\share\\bla") [""; "bla"] ~__POS__;
    eq (Fpath.segs @@ v "\\\\?\\UNC\\Server\\share\\bla") [""; "bla"] ~__POS__;
    eq (Fpath.segs @@ v "\\\\.\\dev\\bla") [""; "bla"] ~__POS__;
    eq (Fpath.segs @@ v "\\a") [""; "a"] ~__POS__;
    eq (Fpath.segs @@ v "\\a\\b") [""; "a"; "b"] ~__POS__;
    eq (Fpath.segs @@ v "\\a\\b\\") [""; "a"; "b"; ""] ~__POS__;
    eq (Fpath.segs @@ v "C:.") ["."] ~__POS__;
    eq (Fpath.segs @@ v "C:\\") ["";""] ~__POS__;
    eq (Fpath.segs @@ v "C:\\a") ["";"a"] ~__POS__;
    eq (Fpath.segs @@ v "C:rel") ["rel";] ~__POS__;
    eq (Fpath.segs @@ v "\\\\server\\share\\") [""; ""] ~__POS__;
    eq (Fpath.segs @@ v "\\\\server\\share\\a") [""; "a"] ~__POS__;
    eq (Fpath.segs @@ v "\\\\?\\a:\\") [""; ""] ~__POS__;
    eq (Fpath.segs @@ v "\\\\?\\a:\\c") [""; "c"] ~__POS__;
    eq (Fpath.segs @@ v "\\\\?\\server\\share\\") [""; ""] ~__POS__;
    eq (Fpath.segs @@ v "\\\\?\\server\\share\\a") [""; "a"] ~__POS__;
    eq (Fpath.segs @@ v "\\\\?\\UNC\\server\\share\\") [""; ""] ~__POS__;
    eq (Fpath.segs @@ v "\\\\?\\UNC\\server\\share\\a") [""; "a"] ~__POS__;
    eq (Fpath.segs @@ v "\\\\.\\device\\") ["";""] ~__POS__;
    eq (Fpath.segs @@ v "\\\\.\\device\\a") ["";"a"] ~__POS__;
    eq (Fpath.segs @@ v "\\\\server\\share\\a") ["";"a"] ~__POS__;
    eq (Fpath.segs @@ v "C:a") ["a"] ~__POS__;
    eq (Fpath.segs @@ v "C:\\a") ["";"a"] ~__POS__;
  end;
  ()

let is_dir_path () =
  Test.test "Fpath.is_dir_path" @@ fun () ->
  Test.bool (Fpath.is_dir_path (v ".")) true ~__POS__;
  Test.bool (Fpath.is_dir_path (v "..")) true ~__POS__;
  Test.bool (Fpath.is_dir_path (v "../")) true ~__POS__;
  Test.bool (Fpath.is_dir_path (v "/a/b/")) true ~__POS__;
  Test.bool (Fpath.is_dir_path (v "/a/b")) false ~__POS__;
  Test.bool (Fpath.is_dir_path (v "a/")) true ~__POS__;
  Test.bool (Fpath.is_dir_path (v "a")) false ~__POS__;
  Test.bool (Fpath.is_dir_path (v "a/.")) true ~__POS__;
  Test.bool (Fpath.is_dir_path (v "a/..")) true ~__POS__;
  Test.bool (Fpath.is_dir_path (v "a/..b")) false ~__POS__;
  Test.bool (Fpath.is_dir_path (v "/")) true ~__POS__;
  if windows then begin
    Test.bool (Fpath.is_dir_path (v "C:\\")) true ~__POS__;
    Test.bool (Fpath.is_dir_path (v "C:a")) false ~__POS__;
  end;
  ()

let is_file_path () =
  Test.test "Fpath.is_file_path" @@ fun () ->
  Test.bool (Fpath.is_file_path (v ".")) false ~__POS__;
  Test.bool (Fpath.is_file_path (v "..")) false ~__POS__;
  Test.bool (Fpath.is_file_path (v "../")) false ~__POS__;
  Test.bool (Fpath.is_file_path (v "/a/b/")) false ~__POS__;
  Test.bool (Fpath.is_file_path (v "/a/b")) true ~__POS__;
  Test.bool (Fpath.is_file_path (v "a/")) false ~__POS__;
  Test.bool (Fpath.is_file_path (v "a")) true ~__POS__;
  Test.bool (Fpath.is_file_path (v "a/.")) false ~__POS__;
  Test.bool (Fpath.is_file_path (v "a/..")) false ~__POS__;
  Test.bool (Fpath.is_file_path (v "a/..b")) true ~__POS__;
  Test.bool (Fpath.is_file_path (v "/")) false ~__POS__;
  if windows then begin
    Test.bool (Fpath.is_file_path (v "C:\\")) false ~__POS__;
    Test.bool (Fpath.is_file_path (v "C:a")) true ~__POS__;
  end;
  ()

let to_dir_path () =
  Test.test "Fpath.to_dir_path" @@ fun () ->
  eq (Fpath.to_dir_path @@ v ".") (v "./") ~__POS__;
  eq (Fpath.to_dir_path @@ v "..") (v "../") ~__POS__;
  eq (Fpath.to_dir_path @@ v "../") (v "../") ~__POS__;
  eq (Fpath.to_dir_path @@ v "/a/b/") (v "/a/b/") ~__POS__;
  eq (Fpath.to_dir_path @@ v "/a/b") (v "/a/b/") ~__POS__;
  eq (Fpath.to_dir_path @@ v "a/") (v "a/") ~__POS__;
  eq (Fpath.to_dir_path @@ v "a") (v "a/") ~__POS__;
  eq (Fpath.to_dir_path @@ v "a/.") (v "a/./") ~__POS__;
  eq (Fpath.to_dir_path @@ v "a/..") (v "a/../") ~__POS__;
  eq (Fpath.to_dir_path @@ v "a/..b") (v "a/..b/") ~__POS__;
  eq (Fpath.to_dir_path @@ v "/") (v "/") ~__POS__;
  if not windows then begin
    eq (Fpath.to_dir_path @@ v "//") (v "//") ~__POS__;
    eq (Fpath.to_dir_path @@ v "//a") (v "//a/") ~__POS__;
  end;
  if windows then begin
    eq (Fpath.to_dir_path @@
         v "\\\\server\\share\\") (v "\\\\server\\share\\") ~__POS__;
    eq (Fpath.to_dir_path @@ v "C:a") (v "C:a/") ~__POS__;
    eq (Fpath.to_dir_path @@ v "C:\\") (v "C:\\") ~__POS__;
  end;
  ()

let filename () =
  Test.test "Fpath.filename" @@ fun () ->
  Test.string (Fpath.filename @@ v ".") "" ~__POS__;
  Test.string (Fpath.filename @@ v "./") "" ~__POS__;
  Test.string (Fpath.filename @@ v "..") "" ~__POS__;
  Test.string (Fpath.filename @@ v "../") "" ~__POS__;
  Test.string (Fpath.filename @@ v "../..") "" ~__POS__;
  Test.string (Fpath.filename @@ v "../../") "" ~__POS__;
  Test.string (Fpath.filename @@ v "/a/b/") "" ~__POS__;
  Test.string (Fpath.filename @@ v "/a/b") "b" ~__POS__;
  Test.string (Fpath.filename @@ v "a/") "" ~__POS__;
  Test.string (Fpath.filename @@ v "a") "a" ~__POS__;
  Test.string (Fpath.filename @@ v "a/.") "" ~__POS__;
  Test.string (Fpath.filename @@ v "a/..") "" ~__POS__;
  Test.string (Fpath.filename @@ v "a/..b") "..b" ~__POS__;
  Test.string (Fpath.filename @@ v "/") "" ~__POS__;
  Test.string (Fpath.filename @@ v "/a/b/") "" ~__POS__;
  Test.string (Fpath.filename @@ v "/a/b") "b" ~__POS__;
  Test.string (Fpath.filename @@ v "a") "a" ~__POS__;
  Test.string (Fpath.filename @@ v "a/") "" ~__POS__;
  Test.string (Fpath.filename @@ v "/") "" ~__POS__;
  if not windows then begin
    Test.string (Fpath.filename @@ v "//") "" ~__POS__;
    Test.string (Fpath.filename @@ v "//..") "" ~__POS__;
    Test.string (Fpath.filename @@ v "//a/b") "b" ~__POS__;
    Test.string (Fpath.filename @@ v "//a/b/") "" ~__POS__;
  end;
  if windows then begin
    Test.string (Fpath.filename @@ v "\\\\server\\share\\a") "a" ~__POS__;
    Test.string (Fpath.filename @@ v "\\\\.\\device\\") "" ~__POS__;
    Test.string (Fpath.filename @@ v "\\\\.\\device\\a") "a" ~__POS__;
    Test.string (Fpath.filename @@ v "C:\\") "" ~__POS__;
    Test.string (Fpath.filename @@ v "C:a") "a" ~__POS__;
  end;
  ()

let split_base () =
  Test.test "Fpath.split_base" @@ fun () ->
  let eq_split ?__POS__:pos p (d, b) =
    Test.block ?__POS__:pos @@ fun () ->
    let d', b' = Fpath.split_base (v p) in
    eq (v d) d' ~__POS__;
    eq (v b) b' ~__POS__;
  in
  eq_split "." ("./", ".") ~__POS__;
  eq_split "./" ("./", "./") ~__POS__;
  eq_split ".." ("./", "..") ~__POS__;
  eq_split "../" ("./", "../") ~__POS__;
  eq_split "../../" ("../", "../") ~__POS__;
  eq_split ".././" ("../", "./") ~__POS__;
  eq_split "../../../" ("../../", "../") ~__POS__;
  eq_split "/" ("/", "./") ~__POS__;
  eq_split "/a/b/" ("/a/", "b/") ~__POS__;
  eq_split "/a/b" ("/a/", "b") ~__POS__;
  eq_split "a/" ("./", "a/") ~__POS__;
  eq_split "a" ("./", "a") ~__POS__;
  eq_split "a/b" ("a/", "b") ~__POS__;
  eq_split "a/b/" ("a/", "b/") ~__POS__;
  eq_split "a/." ("a/", ".") ~__POS__;
  eq_split "a/.." ("a/", "..") ~__POS__;
  eq_split "a/../.." ("a/../", "..") ~__POS__;
  eq_split "a/..b" ("a/", "..b") ~__POS__;
  eq_split "./a" ("./", "a") ~__POS__;
  eq_split "./a/" ("./", "a/") ~__POS__;
  eq_split "../a" ("../", "a") ~__POS__;
  eq_split "../a/" ("../", "a/") ~__POS__;
  if not windows then begin
    eq_split "//" ("//", "./") ~__POS__;
    eq_split "//a/b" ("//a/", "b") ~__POS__;
    eq_split "//a/b/" ("//a/", "b/") ~__POS__;
    eq_split "//a" ("//", "a") ~__POS__;
    eq_split "//a/" ("//", "a/") ~__POS__;
    eq_split "//a/." ("//a/", ".") ~__POS__;
    eq_split "//a/./" ("//a/", "./") ~__POS__;
  end;
  if windows then begin
    eq_split "\\\\server\\share\\a" ("\\\\server\\share\\", "a") ~__POS__;
    eq_split "\\\\.\\device\\" ("\\\\.\\device\\", ".\\") ~__POS__;
    eq_split "\\\\.\\device\\a" ("\\\\.\\device\\", "a") ~__POS__;
    eq_split "\\\\.\\device\\a\\" ("\\\\.\\device\\", "a\\") ~__POS__;
    eq_split "C:\\" ("C:\\", ".\\") ~__POS__;
    eq_split "C:a" ("C:.\\", "a") ~__POS__;
  end;
  ()

let base () =
  Test.test "Fpath.base" @@ fun () ->
  eq (Fpath.base @@ v ".") (v ".") ~__POS__;
  eq (Fpath.base @@ v "./") (v "./") ~__POS__;
  eq (Fpath.base @@ v "..") (v "..") ~__POS__;
  eq (Fpath.base @@ v "../") (v "../") ~__POS__;
  eq (Fpath.base @@ v "../../") (v "../") ~__POS__;
  eq (Fpath.base @@ v ".././") (v "./") ~__POS__;
  eq (Fpath.base @@ v "../../../") (v "../") ~__POS__;
  eq (Fpath.base @@ v "/") (v "./") ~__POS__;
  eq (Fpath.base @@ v "/a/b/") (v "b/") ~__POS__;
  eq (Fpath.base @@ v "/a/b") (v "b") ~__POS__;
  eq (Fpath.base @@ v "a/") (v "a/") ~__POS__;
  eq (Fpath.base @@ v "a") (v "a") ~__POS__;
  eq (Fpath.base @@ v "a/b") (v "b") ~__POS__;
  eq (Fpath.base @@ v "a/b/") (v "b/") ~__POS__;
  eq (Fpath.base @@ v "a/.") (v ".") ~__POS__;
  eq (Fpath.base @@ v "a/..") (v "..") ~__POS__;
  eq (Fpath.base @@ v "a/../..") (v "..") ~__POS__;
  eq (Fpath.base @@ v "a/..b") (v "..b") ~__POS__;
  eq (Fpath.base @@ v "./a") (v "a") ~__POS__;
  eq (Fpath.base @@ v "./a/") (v "a/") ~__POS__;
  eq (Fpath.base @@ v "../a") (v "a") ~__POS__;
  eq (Fpath.base @@ v "../a/") (v "a/") ~__POS__;
  if not windows then begin
    eq (Fpath.base @@ v "//") (v "./") ~__POS__;
    eq (Fpath.base @@ v "//a/b") (v "b") ~__POS__;
    eq (Fpath.base @@ v "//a/b/") (v "b/") ~__POS__;
    eq (Fpath.base @@ v "//a") (v "a") ~__POS__;
    eq (Fpath.base @@ v "//a/") (v "a/") ~__POS__;
    eq (Fpath.base @@ v "//a/.") (v ".") ~__POS__;
    eq (Fpath.base @@ v "//a/./") (v "./") ~__POS__;
  end;
  if windows then begin
    eq (Fpath.base @@ v "\\\\server\\share\\a") (v "a") ~__POS__;
    eq (Fpath.base @@ v "\\\\.\\device\\") (v ".\\") ~__POS__;
    eq (Fpath.base @@ v "\\\\.\\device\\a") (v "a") ~__POS__;
    eq (Fpath.base @@ v "\\\\.\\device\\a\\") (v "a\\") ~__POS__;
    eq (Fpath.base @@ v "C:\\") (v ".\\") ~__POS__;
    eq (Fpath.base @@ v "C:a") (v "a") ~__POS__;
  end;
  ()

let basename () =
  Test.test "Fpath.basename" @@ fun () ->
  Test.string (Fpath.basename @@ v ".") "" ~__POS__;
  Test.string (Fpath.basename @@ v "..") "" ~__POS__;
  Test.string (Fpath.basename @@ v "../") "" ~__POS__;
  Test.string (Fpath.basename @@ v "../../") "" ~__POS__;
  Test.string (Fpath.basename @@ v "/") "" ~__POS__;
  Test.string (Fpath.basename @@ v "/a/b/") "b" ~__POS__;
  Test.string (Fpath.basename @@ v "/a/b") "b" ~__POS__;
  Test.string (Fpath.basename @@ v "a/") "a" ~__POS__;
  Test.string (Fpath.basename @@ v "a") "a" ~__POS__;
  Test.string (Fpath.basename @@ v "a/.") "" ~__POS__;
  Test.string (Fpath.basename @@ v "a/./") "" ~__POS__;
  Test.string (Fpath.basename @@ v "a/..") "" ~__POS__;
  Test.string (Fpath.basename @@ v "a/..b") "..b" ~__POS__;
  Test.string (Fpath.basename @@ v "./a") "a" ~__POS__;
  Test.string (Fpath.basename @@ v "../a") "a" ~__POS__;
  if not windows then begin
    Test.string (Fpath.basename @@ v "//") "" ~__POS__;
    Test.string (Fpath.basename @@ v "//a/b") "b" ~__POS__;
    Test.string (Fpath.basename @@ v "//a/b/") "b" ~__POS__;
  end;
  if windows then begin
    Test.string (Fpath.basename @@ v "\\\\server\\share\\a") "a" ~__POS__;
    Test.string (Fpath.basename @@ v "\\\\server\\share\\a\\") "a" ~__POS__;
    Test.string (Fpath.basename @@ v "\\\\.\\device\\") "" ~__POS__;
    Test.string (Fpath.basename @@ v "\\\\.\\device\\a") "a" ~__POS__;
    Test.string (Fpath.basename @@ v "C:\\") "" ~__POS__;
    Test.string (Fpath.basename @@ v "C:a") "a" ~__POS__;
  end;
  ()

let parent () =
  Test.test "Fpath.parent" @@ fun () ->
  eq (Fpath.parent @@ v ".") (v "./../") ~__POS__;
  eq (Fpath.parent @@ v "..") (v "../../") ~__POS__;
  eq (Fpath.parent @@ v "../") (v "../../") ~__POS__;
  eq (Fpath.parent @@ v "../../") (v "../../../") ~__POS__;
  eq (Fpath.parent @@ v "/") (v "/") ~__POS__;
  eq (Fpath.parent @@ v "/a/b/") (v "/a/") ~__POS__;
  eq (Fpath.parent @@ v "/a/b") (v "/a/") ~__POS__;
  eq (Fpath.parent @@ v "a/") (v "./") ~__POS__;
  eq (Fpath.parent @@ v "a") (v "./") ~__POS__;
  eq (Fpath.parent @@ v "a/.") (v "a/./../") ~__POS__;
  eq (Fpath.parent @@ v "a/./") (v "a/./../") ~__POS__;
  eq (Fpath.parent @@ v "a/..") (v "a/../../") ~__POS__;
  eq (Fpath.parent @@ v "a/../") (v "a/../../") ~__POS__;
  eq (Fpath.parent @@ v "a/..b") (v "a/") ~__POS__;
  eq (Fpath.parent @@ v "./a") (v "./") ~__POS__;
  eq (Fpath.parent @@ v "../a") (v "../") ~__POS__;
  eq (Fpath.parent @@ v "../../a") (v "../../") ~__POS__;
  if not windows then begin
    eq (Fpath.parent @@ v "//") (v "//") ~__POS__;
    eq (Fpath.parent @@ v "//.") (v "//./../") ~__POS__;
    eq (Fpath.parent @@ v "//a/b") (v "//a/") ~__POS__;
    eq (Fpath.parent @@ v "//a/b/") (v "//a/") ~__POS__;
    eq (Fpath.parent @@ v "//a/b/..") (v "//a/b/../../") ~__POS__;
    eq (Fpath.parent @@ v "//a/b/../") (v "//a/b/../../") ~__POS__;
    eq (Fpath.parent @@ v "//a") (v "//") ~__POS__;
    eq (Fpath.parent @@ v "//abcd") (v "//") ~__POS__;
  end;
  if windows then begin
    eq (Fpath.parent @@ v "\\\\server\\share\\") (v "\\\\server\\share\\")
      ~__POS__;
    eq (Fpath.parent @@ v "C:a") (v "C:.\\") ~__POS__;
    eq (Fpath.parent @@ v "C:\\") (v "C:\\") ~__POS__;
    eq (Fpath.parent @@ v "C:\\a\\b\\") (v "C:\\a\\") ~__POS__;
    eq (Fpath.parent @@ v "C:\\a\\b") (v "C:\\a\\") ~__POS__;
    eq (Fpath.parent @@ v "C:a\\b\\") (v "C:a\\") ~__POS__;
    eq (Fpath.parent @@ v "C:a\\b") (v "C:a\\") ~__POS__;
    eq (Fpath.parent @@ v "C:a\\..") (v "C:a\\..\\..\\") ~__POS__;
  end;
  ()

let rem_empty_seg () =
  Test.test "Fpath.rem_empty_seg" @@ fun () ->
  eq (Fpath.rem_empty_seg @@ v ".") (v ".") ~__POS__;
  eq (Fpath.rem_empty_seg @@ v "..") (v "..") ~__POS__;
  eq (Fpath.rem_empty_seg @@ v "../") (v "..") ~__POS__;
  eq (Fpath.rem_empty_seg @@ v "../../") (v "../..") ~__POS__;
  eq (Fpath.rem_empty_seg @@ v "/") (v "/") ~__POS__;
  eq (Fpath.rem_empty_seg @@ v "/a/b/") (v "/a/b") ~__POS__;
  eq (Fpath.rem_empty_seg @@ v "/a/b") (v "/a/b") ~__POS__;
  eq (Fpath.rem_empty_seg @@ v "a/") (v "a") ~__POS__;
  eq (Fpath.rem_empty_seg @@ v "a") (v "a") ~__POS__;
  eq (Fpath.rem_empty_seg @@ v "a/.") (v "a/.") ~__POS__;
  eq (Fpath.rem_empty_seg @@ v "a/./") (v "a/.") ~__POS__;
  eq (Fpath.rem_empty_seg @@ v "a/..") (v "a/..") ~__POS__;
  eq (Fpath.rem_empty_seg @@ v "a/../") (v "a/..") ~__POS__;
  eq (Fpath.rem_empty_seg @@ v "a/..b") (v "a/..b") ~__POS__;
  eq (Fpath.rem_empty_seg @@ v "./a") (v "./a") ~__POS__;
  eq (Fpath.rem_empty_seg @@ v "../a") (v "../a") ~__POS__;
  eq (Fpath.rem_empty_seg @@ v "../../a") (v "../../a") ~__POS__;
  if not windows then begin
    eq (Fpath.rem_empty_seg @@ v "//") (v "//") ~__POS__;
    eq (Fpath.rem_empty_seg @@ v "//a") (v "//a") ~__POS__;
    eq (Fpath.rem_empty_seg @@ v "//a/") (v "//a") ~__POS__;
  end;
  if windows then begin
    eq (Fpath.rem_empty_seg @@ v "\\\\server\\share\\")
      (v "\\\\server\\share\\") ~__POS__;
    eq (Fpath.rem_empty_seg @@ v "\\\\server\\share\\a\\")
      (v "\\\\server\\share\\a") ~__POS__;
    eq (Fpath.rem_empty_seg @@ v "C:a") (v "C:a") ~__POS__;
    eq (Fpath.rem_empty_seg @@ v "C:a\\") (v "C:a") ~__POS__;
    eq (Fpath.rem_empty_seg @@ v "C:\\") (v "C:\\") ~__POS__;
  end;
  ()

let normalize () =
  Test.test "Fpath.normalize" @@ fun () ->
  eq (Fpath.normalize @@ v ".") (v "./") ~__POS__;
  eq (Fpath.normalize @@ v "..") (v "../") ~__POS__;
  eq (Fpath.normalize @@ v "../") (v "../") ~__POS__;
  eq (Fpath.normalize @@ v "../..") (v "../../") ~__POS__;
  eq (Fpath.normalize @@ v "../../") (v "../../") ~__POS__;
  eq (Fpath.normalize @@ v "/") (v "/") ~__POS__;
  eq (Fpath.normalize @@ v "/a/b/") (v "/a/b/") ~__POS__;
  eq (Fpath.normalize @@ v "/a/b") (v "/a/b") ~__POS__;
  eq (Fpath.normalize @@ v "a/") (v "a/") ~__POS__;
  eq (Fpath.normalize @@ v "a") (v "a") ~__POS__;
  eq (Fpath.normalize @@ v "a/.") (v "a/") ~__POS__;
  eq (Fpath.normalize @@ v "a/./") (v "a/") ~__POS__;
  eq (Fpath.normalize @@ v "a/..") (v "./") ~__POS__;
  eq (Fpath.normalize @@ v "a/../") (v "./") ~__POS__;
  eq (Fpath.normalize @@ v "a/..b") (v "a/..b") ~__POS__;
  eq (Fpath.normalize @@ v "./a") (v "a") ~__POS__;
  eq (Fpath.normalize @@ v "../a") (v "../a") ~__POS__;
  eq (Fpath.normalize @@ v "a/..") (v "./") ~__POS__;
  eq (Fpath.normalize @@ v "../../a") (v "../../a") ~__POS__;
  eq (Fpath.normalize @@ v "./a/..") (v "./") ~__POS__;
  eq (Fpath.normalize @@ v "/a/b/./..") (v "/a/") ~__POS__;
  eq (Fpath.normalize @@ v "/../..") (v "/") ~__POS__;
  eq (Fpath.normalize @@ v "/a/../..") (v "/") ~__POS__;
  eq (Fpath.normalize @@ v "./../..") (v "../../") ~__POS__;
  eq (Fpath.normalize @@ v "../../a/") (v "../../a/") ~__POS__;
  eq (Fpath.normalize @@ v "a/../a/") (v "a/") ~__POS__;
  eq (Fpath.normalize @@ v "a/../a/../..") (v "../") ~__POS__;
  eq (Fpath.normalize @@ v "/a/../a/../..") (v "/") ~__POS__;
  eq (Fpath.normalize @@ v "/a/b/c/./../../g") (v "/a/g") ~__POS__;
  eq (Fpath.normalize @@ v "/a/b/c/./../../g/") (v "/a/g/") ~__POS__;
  eq (Fpath.normalize @@ v "a/b/c/./../../g") (v "a/g") ~__POS__;
  eq (Fpath.normalize @@ v "a/b/c/./../../g/") (v "a/g/") ~__POS__;
  eq (Fpath.normalize @@ v "././.") (v "./") ~__POS__;
  eq (Fpath.normalize @@ v "./././") (v "./") ~__POS__;
  eq (Fpath.normalize @@ v "./a/..") (v "./") ~__POS__;
  eq (Fpath.normalize @@ v "./a/../") (v "./") ~__POS__;
  eq (Fpath.normalize @@ v "..") (v "../") ~__POS__;
  eq (Fpath.normalize @@ v "../../../a") (v "../../../a") ~__POS__;
  eq (Fpath.normalize @@ v "../../../a/") (v "../../../a/") ~__POS__;
  eq (Fpath.normalize @@ v "/") (v "/") ~__POS__;
  eq (Fpath.normalize @@ v "/.") (v "/") ~__POS__;
  eq (Fpath.normalize @@ v "/..") (v "/") ~__POS__;
  eq (Fpath.normalize @@ v "/./../../.") (v "/") ~__POS__;
  eq (Fpath.normalize @@ v "/./../../.") (v "/") ~__POS__;
  eq (Fpath.normalize @@ v "../../a/..") (v "../../") ~__POS__;
  eq (Fpath.normalize @@ v "../../a/../.") (v "../../") ~__POS__;
  eq (Fpath.normalize @@ v "../../a/.././..") (v "../../../") ~__POS__;
  eq (Fpath.normalize @@ v "../../a/../..") (v "../../../") ~__POS__;
  eq (Fpath.normalize @@ v "/a/b/c/./../../g") (v "/a/g") ~__POS__;
  eq (Fpath.normalize @@ v "./a/b/c/./../../g") (v "a/g") ~__POS__;
  eq (Fpath.normalize @@ v "./a/b/c/./../../g/") (v "a/g/") ~__POS__;
  if not windows then begin
    eq (Fpath.normalize @@ v "//a/b/c/./../../g") (v "//a/g") ~__POS__;
    eq (Fpath.normalize @@ v "//a/b/c/./../../g/") (v "//a/g/") ~__POS__;
  end;
  if windows then begin
    eq (Fpath.normalize @@ v "C:/a/b/c/./../../g") (v "C:/a/g") ~__POS__;
    eq (Fpath.normalize @@ v "C:/a/b/c/./../../g/") (v "C:/a/g/") ~__POS__;
    eq (Fpath.normalize @@ v "\\\\?\\UNC\\server\\share\\..")
      (v "\\\\?\\UNC\\server\\share\\") ~__POS__;
  end;
  ()

let is_prefix () =
  Test.test "Fpath.is_prefix" @@ fun () ->
  Test.bool (Fpath.is_prefix (v "/a/b") (v "/a/b")) true ~__POS__;
  Test.bool (Fpath.is_prefix (v "/a/b") (v "/a/bc")) false ~__POS__;
  Test.bool (Fpath.is_prefix (v "/a/b") (v "/a/b/")) true ~__POS__;
  Test.bool (Fpath.is_prefix (v "a/b/") (v "a/b")) false ~__POS__;
  Test.bool (Fpath.is_prefix (v "a/b/") (v "a/b/")) true ~__POS__;
  Test.bool (Fpath.is_prefix (v "a/b/") (v "a/b/c")) true ~__POS__;
  Test.bool (Fpath.is_prefix (v ".") (v "./")) true ~__POS__;
  Test.bool (Fpath.is_prefix (v "..") (v ".")) false ~__POS__;
  Test.bool (Fpath.is_prefix (v ".") (v "..")) false ~__POS__;
  Test.bool (Fpath.is_prefix (v "/a/b") (v "/a/b/c")) true ~__POS__;
  Test.bool (Fpath.is_prefix (v "/a/b/") (v "/a/b/c")) true ~__POS__;
  Test.bool (Fpath.is_prefix (v "/a/b/") (v "/a/b")) false ~__POS__;
  Test.bool (Fpath.is_prefix (v "/a/b/") (v "/a/b")) false ~__POS__;
  Test.bool (Fpath.is_prefix (v "a/b") (v "/a/b")) false ~__POS__;
  Test.bool (Fpath.is_prefix (v "abcd/") (v "abcd")) false ~__POS__;
  Test.bool (Fpath.is_prefix (v "abcd") (v "abcd/bla")) true ~__POS__;
  if not windows then begin
    Test.bool (Fpath.is_prefix (v "//a/b") (v "/a/b")) false ~__POS__;
  end;
  if windows then begin
    Test.bool (Fpath.is_prefix (v "C:a") (v "a")) false ~__POS__;
  end;
  ()

let find_prefix () =
  Test.test "Fpath.find_prefix" @@ fun () ->
  let eq = Test.eq Test.Eq.(option (module Fpath)) in
  let find_prefix ?__POS__:pos p0 p1 r =
    Test.block ?__POS__:pos @@ fun () ->
    eq (Fpath.find_prefix p0 p1) r ~__POS__;
    eq (Fpath.find_prefix p1 p0) r ~__POS__;
  in
  find_prefix (v "a/b/c") (v "a/b/d") (Some (v "a/b/")) ~__POS__;
  find_prefix (v "a/b/c") (v "a/b/cd") (Some (v "a/b/")) ~__POS__;
  find_prefix (v "a/b") (v "a/b") (Some (v "a/b")) ~__POS__;
  find_prefix (v "a/b") (v "a/b/") (Some (v "a/b")) ~__POS__;
  find_prefix (v "a/b") (v "e/f") None ~__POS__;
  find_prefix (v "/a/b") (v "/e/f") (Some (v "/")) ~__POS__;
  find_prefix (v "/a/b") (v "e/f") None ~__POS__;
  find_prefix (v "/a/b/c") (v "/a/b/d") (Some (v "/a/b/")) ~__POS__;
  find_prefix (v "ab") (v "abc") None ~__POS__;
  find_prefix (v "ab") (v "ab") (Some (v "ab")) ~__POS__;
  find_prefix (v "/") (v "/") (Some (v "/")) ~__POS__;
  find_prefix (v "a/") (v "a") (Some (v "a")) ~__POS__;
  find_prefix (v "abc/") (v "abc") (Some (v "abc")) ~__POS__;
  find_prefix (v "abcd/") (v "abc") None ~__POS__;
  find_prefix (v "a/") (v "a/a") (Some (v "a/")) ~__POS__;
  if not windows then begin
    find_prefix (v "//") (v "/") None ~__POS__;
    find_prefix (v "/") (v "//") None ~__POS__;
    find_prefix (v "//") (v "/a/b") None ~__POS__;
    find_prefix (v "//a/b/c") (v "/") None ~__POS__;
    find_prefix (v "//a/b/c") (v "//") (Some (v "//")) ~__POS__;
    find_prefix (v "//a/b") (v "/a/b") None ~__POS__;
    find_prefix (v "//a/c") (v "/a/b") None ~__POS__;
    find_prefix (v "//a/c") (v "a/b") None ~__POS__;
  end;
  if windows then begin
    find_prefix (v "C:\\a") (v "\\a") None ~__POS__;
    find_prefix (v "C:\\a") (v "C:\\a") (Some (v "C:\\a")) ~__POS__;
    find_prefix (v "C:a") (v "C:a") (Some (v "C:a")) ~__POS__;
    find_prefix (v "C:a") (v "C:b") None ~__POS__;
    find_prefix (v "C:a") (v "C:b/c") None ~__POS__;
    find_prefix (v "C:a/f") (v "C:b/c") None ~__POS__;
    find_prefix (v "C:a/f") (v "C:/b/c") None ~__POS__;
    find_prefix (v "C:\\") (v "C:\\") (Some (v "C:\\")) ~__POS__;
    find_prefix (v "\\\\server\\share\\") (v "\\\\server\\share\\")
      (Some (v "\\\\server\\share\\")) ~__POS__;
    find_prefix (v "\\\\server\\share\\") (v "\\\\server\\share\\a")
      (Some (v "\\\\server\\share\\")) ~__POS__;
    find_prefix (v "\\\\server\\share\\a") (v "\\\\server\\share\\a")
      (Some (v "\\\\server\\share\\a")) ~__POS__;
    find_prefix (v "\\\\server\\share\\a") (v "\\\\server\\share\\b")
      (Some (v "\\\\server\\share\\")) ~__POS__;
  end;
  ()

let rem_prefix () =
  Test.test "Fpath.rem_prefix" @@ fun () ->
  let eq = Test.eq Test.Eq.(option (module Fpath)) in
  eq (Fpath.rem_prefix (v "a/b/") (v "a/b")) None ~__POS__;
  eq (Fpath.rem_prefix (v "a/b/") (v "a/b/")) None ~__POS__;
  eq (Fpath.rem_prefix (v "a/b") (v "a/b")) None ~__POS__;
  eq (Fpath.rem_prefix (v "a/b") (v "a/b/")) (Some (v "./")) ~__POS__;
  eq (Fpath.rem_prefix (v "a/b") (v "a/b/c")) (Some (v "c")) ~__POS__;
  eq (Fpath.rem_prefix (v "a/b") (v "a/b/c/")) (Some (v "c/")) ~__POS__;
  eq (Fpath.rem_prefix (v "a/b/") (v "a/b/c")) (Some (v "c")) ~__POS__;
  eq (Fpath.rem_prefix (v "a/b/") (v "a/b/c/")) (Some (v "c/")) ~__POS__;
  eq (Fpath.rem_prefix (v "a/b") (v "a/b")) None ~__POS__;
  eq (Fpath.rem_prefix (v "/a/b/") (v "/a/b")) None ~__POS__;
  eq (Fpath.rem_prefix (v "/a/b/") (v "/a/b/")) None ~__POS__;
  eq (Fpath.rem_prefix (v "/a/b") (v "/a/bc")) None ~__POS__;
  eq (Fpath.rem_prefix (v "/a/b") (v "/a/b")) None ~__POS__;
  eq (Fpath.rem_prefix (v "/a/b/") (v "/a/b")) None ~__POS__;
  eq (Fpath.rem_prefix (v "/a/b") (v "/a/b/")) (Some (v "./")) ~__POS__;
  eq (Fpath.rem_prefix (v "/a/b/") (v "/a/b/")) None ~__POS__;
  eq (Fpath.rem_prefix (v "/a/b") (v "/a/b/c")) (Some (v "c")) ~__POS__;
  eq (Fpath.rem_prefix (v "/a/b/") (v "/a/b/c")) (Some (v "c")) ~__POS__;
  eq (Fpath.rem_prefix (v "a") (v "a/b/c")) (Some (v "b/c")) ~__POS__;
  if windows then begin
    eq (Fpath.rem_prefix (v "C:\\a") (v "C:\\a\\b")) (Some (v "b")) ~__POS__;
  end;
  ()

let relativize () =
  Test.test "Fpath.relativize" @@ fun () ->
  let eq_opt = Test.eq Test.Eq.(option (module Fpath)) in
  let relativize ?__POS__:pos root p result =
    Test.block ?__POS__:pos @@ fun () ->
    match Fpath.relativize ~root p with
    | None -> eq_opt None result ~__POS__
    | Some rel as r ->
        eq_opt r result ~__POS__;
        eq (Fpath.normalize (Fpath.append root rel)) (Fpath.normalize p)
          ~__POS__;
  in
  relativize (v "/a/") (v "/a") (Some (v "../a")) ~__POS__;
  relativize (v "/a/") (v "/a/") (Some (v "./")) ~__POS__;
  relativize (v "/a/") (v "/") (Some (v "../")) ~__POS__;
  relativize (v "/a/") (v "/../") (Some (v "../")) ~__POS__;
  relativize (v "/a/") (v "/../c/d") (Some (v "../c/d")) ~__POS__;
  relativize (v "/a/") (v "/../c/d/") (Some (v "../c/d/")) ~__POS__;
  relativize (v "/") (v "/../c/d/") (Some (v "c/d/")) ~__POS__;
  relativize (v "/") (v "/../c/d") (Some (v "c/d")) ~__POS__;
  relativize (v "/") (v "/") (Some (v "./")) ~__POS__;
  relativize (v "/") (v "/a") (Some (v "a")) ~__POS__;
  relativize (v "/") (v "/a/../b") (Some (v "b")) ~__POS__;
  relativize (v "/") (v "/a/../b/") (Some (v "b/")) ~__POS__;
  relativize (v "/a/b/") (v "c") None ~__POS__;
  relativize (v "/a/b/") (v "./") None ~__POS__;
  relativize (v "/a/b/") (v "../") None ~__POS__;
  relativize (v "/a/b/") (v "/c") (Some (v "../../c")) ~__POS__;
  relativize (v "/a/b/") (v "/c/") (Some (v "../../c/")) ~__POS__;
  relativize (v "/a/b/") (v "/c/d/e") (Some (v "../../c/d/e")) ~__POS__;
  relativize (v "/a/b/") (v "/c/d/e/../../f") (Some (v "../../c/f")) ~__POS__;
  relativize (v "/a/b/") (v "/c/d/e/../../f/") (Some (v "../../c/f/")) ~__POS__;
  relativize (v "/a/b/") (v "/./c/d/e/../../f/") (Some (v "../../c/f/"))
    ~__POS__;
  relativize (v "/a/b/") (v "/a/b/c") (Some (v "c")) ~__POS__;
  relativize (v "/a/b/") (v "/a/b") (Some (v "../b")) ~__POS__;
  relativize (v "/a/b/") (v "/a/b/") (Some (v "./")) ~__POS__;
  relativize (v "/a/b/c") (v "/d/e/f") (Some (v "../../../d/e/f")) ~__POS__;
  relativize (v "/a/b/c") (v "/a/b/d") (Some (v "../d")) ~__POS__;
  relativize (v "a/b") (v "/c") None ~__POS__;
  relativize (v "a/b") (v "c") (Some (v "../../c")) ~__POS__;
  relativize (v "a/b") (v "../c") (Some (v "../../../c")) ~__POS__;
  relativize (v "a/b") (v "../c/") (Some (v "../../../c/")) ~__POS__;
  relativize (v "a/b") (v "c/") (Some (v "../../c/")) ~__POS__;
  relativize (v "a/b") (v "a/b/c") (Some (v "c")) ~__POS__;
  relativize (v "a/b") (v "a") (Some (v "../../a")) ~__POS__;
  relativize (v "a/b") (v "b") (Some (v "../../b")) ~__POS__;
  relativize (v "a/b") (v "c") (Some (v "../../c")) ~__POS__;
  relativize (v "a/b/c/") (v "a/d") (Some (v "../../d")) ~__POS__;
  relativize (v "a/b/c/") (v "a/b") (Some (v "../../b")) ~__POS__;
  relativize (v "a/b/c/") (v "a/b/../../../") (Some (v "../../../../"))
    ~__POS__;
  relativize (v "a/b/c/") (v "a/b/../../../a") (Some (v "../../../../a"))
    ~__POS__;
  relativize (v "a/b") (v "a/b/") (Some (v "./")) ~__POS__;
  relativize (v "../") (v "./") None ~__POS__;
  relativize (v "../a") (v "b") None ~__POS__;
  relativize (v "../../a") (v "../b") None ~__POS__;
  relativize (v "../a") (v "../b/c") (Some (v "../b/c")) ~__POS__;
  relativize (v "../a") (v "../../b") (Some (v "../../b")) ~__POS__;
  relativize (v "a") (v "../../b") (Some (v "../../../b")) ~__POS__;
  relativize (v "a/c") (v "../../b") (Some (v "../../../../b")) ~__POS__;
  if windows then begin
    relativize (v "C:a\\c") (v "C:..\\..\\b") (Some (v "..\\..\\..\\..\\b"))
      ~__POS__;
    relativize (v "C:a\\c") (v "..\\..\\b") None ~__POS__;
    relativize (v "\\\\?\\UNC\\server\\share\\a\\b\\c")
      (v "\\\\?\\UNC\\server\\share\\d\\e\\f") (Some (v "../../../d/e/f"))
      ~__POS__;
  end;
  ()

let is_rooted () =
  Test.test "Fpath.is_rooted" @@ fun () ->
  Test.bool (Fpath.is_rooted ~root:(v "a/b") (v "a/b")) false ~__POS__;
  Test.bool (Fpath.is_rooted ~root:(v "a/b") (v "a/b/")) true ~__POS__;
  Test.bool (Fpath.is_rooted ~root:(v "a/b/") (v "a/b")) false ~__POS__;
  Test.bool (Fpath.is_rooted ~root:(v "a/b/") (v "a/b/")) true ~__POS__;
  Test.bool (Fpath.is_rooted ~root:(v "./") (v "a")) true ~__POS__;
  Test.bool (Fpath.is_rooted ~root:(v "./") (v "a/")) true ~__POS__;
  Test.bool (Fpath.is_rooted ~root:(v "./") (v "a/../")) true ~__POS__;
  Test.bool (Fpath.is_rooted ~root:(v "./") (v "..")) false ~__POS__;
  Test.bool (Fpath.is_rooted ~root:(v "../") (v "./")) false ~__POS__;
  Test.bool (Fpath.is_rooted ~root:(v "../") (v "a")) false ~__POS__;
  Test.bool (Fpath.is_rooted ~root:(v "../") (v "../a")) true ~__POS__;
  Test.bool (Fpath.is_rooted ~root:(v "../a") (v "./")) false ~__POS__;
  Test.bool (Fpath.is_rooted ~root:(v "/a") (v "/a/..")) false ~__POS__;
  Test.bool (Fpath.is_rooted ~root:(v "/a") (v "/a/../a/")) true ~__POS__;
  Test.bool (Fpath.is_rooted ~root:(v "/a") (v "/a/../a")) false ~__POS__;
  Test.bool (Fpath.is_rooted ~root:(v "/") (v "/..")) true ~__POS__;
  ()

let is_abs_rel () =
  Test.test "Fpath.is_abs_rel" @@ fun () ->
  let is_abs ?__POS__:pos  bool p =
    Test.block ?__POS__:pos @@ fun () ->
    let p = v p in
    Test.bool (Fpath.is_abs p) bool ~__POS__;
    Test.bool (Fpath.is_rel p) (not bool) ~__POS__;
  in
  is_abs true "/a/b/c" ~__POS__;
  if not windows then is_abs true "//a/b/c" ~__POS__;
  is_abs false "." ~__POS__;
  is_abs false ".." ~__POS__;
  is_abs false "../" ~__POS__;
  is_abs false "a" ~__POS__;
  is_abs false "a/b" ~__POS__;
  is_abs true "/" ~__POS__;
  if windows then begin
    is_abs false "C:." ~__POS__;
    is_abs true "C:\\" ~__POS__;
    is_abs true "C:/" ~__POS__;
    is_abs false "C:bli/bla" ~__POS__;
    is_abs false "C:bli/bla" ~__POS__;
    is_abs false  "C:rel" ~__POS__;
    is_abs true "\\\\server\\share\\" ~__POS__;
    is_abs true "\\\\?\\a:\\" ~__POS__;
    is_abs true "\\\\?\\a:\\c" ~__POS__;
    is_abs true "\\\\?\\server\\share\\" ~__POS__;
    is_abs true "\\\\?\\server\\share\\a" ~__POS__;
    is_abs true "\\\\?\\UNC\\server\\share\\" ~__POS__;
    is_abs true "\\\\?\\UNC\\server\\share\\a" ~__POS__;
    is_abs true "\\\\.\\device\\" ~__POS__;
    is_abs true "\\\\.\\device\\a" ~__POS__;
  end;
  ()

let is_root () =
  Test.test "Fpath.is_root" @@ fun () ->
  Test.bool (Fpath.is_root (v "/")) true ~__POS__;
  Test.bool (Fpath.is_root (v "/..")) false ~__POS__;
  Test.bool (Fpath.is_root (v "/.")) false ~__POS__;
  Test.bool (Fpath.is_root (v "/a")) false ~__POS__;
  Test.bool (Fpath.is_root (v "/a/..")) false ~__POS__;
  Test.bool (Fpath.is_root (v "a")) false ~__POS__;
  Test.bool (Fpath.is_root (v ".")) false ~__POS__;
  Test.bool (Fpath.is_root (v "..")) false ~__POS__;
  if not windows then (Test.bool (Fpath.is_root (v "//")) true ~__POS__);
  if windows then begin
    Test.bool (Fpath.is_root (v "\\\\.\\dev\\")) true ~__POS__;
    Test.bool (Fpath.is_root (v "\\\\.\\dev\\..")) false ~__POS__;
    Test.bool (Fpath.is_root (v "\\\\.\\dev\\a")) false ~__POS__;
    Test.bool (Fpath.is_root (v "\\\\server\\share\\")) true ~__POS__;
    Test.bool (Fpath.is_root (v "\\\\server\\share\\a")) false ~__POS__;
    Test.bool (Fpath.is_root (v "C:\\")) true ~__POS__;
    Test.bool (Fpath.is_root (v "C:a")) false ~__POS__;
    Test.bool (Fpath.is_root (v "C:\\a")) false ~__POS__;
  end;
  ()

let is_current_dir () =
  Test.test "Fpath.is_current_dir" @@ fun () ->
  Test.bool (Fpath.is_current_dir (v ".")) true ~__POS__;
  Test.bool (Fpath.is_current_dir ~prefix:true (v ".")) true ~__POS__;
  Test.bool (Fpath.is_current_dir (v "./")) true ~__POS__;
  Test.bool (Fpath.is_current_dir ~prefix:true (v "./")) true ~__POS__;
  Test.bool (Fpath.is_current_dir (v "./a/..")) false ~__POS__;
  Test.bool (Fpath.is_current_dir ~prefix:true (v "./a/..")) true ~__POS__;
  Test.bool (Fpath.is_current_dir (v "/.")) false ~__POS__;
  if windows then begin
    Test.bool (Fpath.is_current_dir (v "\\\\.\\dev\\.")) false ~__POS__;
    Test.bool (Fpath.is_current_dir ~prefix:true (v "\\\\.\\dev\\.")) false
      ~__POS__;
    Test.bool (Fpath.is_current_dir (v "\\\\.\\dev\\.\\")) false ~__POS__;
    Test.bool (Fpath.is_current_dir (v "\\\\server\\share\\.")) false ~__POS__;
    Test.bool (Fpath.is_current_dir (v "\\\\server\\share\\.\\")) false
      ~__POS__;
    Test.bool (Fpath.is_current_dir (v "C:.")) true ~__POS__;
    Test.bool (Fpath.is_current_dir ~prefix:true (v "C:.")) true ~__POS__;
    Test.bool (Fpath.is_current_dir (v "C:./")) true ~__POS__;
    Test.bool (Fpath.is_current_dir ~prefix:true (v "C:./")) true ~__POS__;
    Test.bool (Fpath.is_current_dir (v "C:./a/..")) false ~__POS__;
    Test.bool (Fpath.is_current_dir ~prefix:true (v "C:./a/..")) true ~__POS__;
  end;
  ()

let is_parent_dir () =
  Test.test "Fpath.is_parent_dir" @@ fun () ->
  Test.bool (Fpath.is_parent_dir (v ".")) false ~__POS__;
  Test.bool (Fpath.is_parent_dir (v "./")) false ~__POS__;
  Test.bool (Fpath.is_parent_dir (v "..")) true ~__POS__;
  Test.bool (Fpath.is_parent_dir ~prefix:true (v "..")) true ~__POS__;
  Test.bool (Fpath.is_parent_dir (v "../")) true ~__POS__;
  Test.bool (Fpath.is_parent_dir ~prefix:true (v "../")) true ~__POS__;
  Test.bool (Fpath.is_parent_dir (v "./a/../..")) false ~__POS__;
  Test.bool (Fpath.is_parent_dir ~prefix:true (v "../a/../..")) true ~__POS__;
  Test.bool (Fpath.is_parent_dir (v "../..")) false ~__POS__;
  Test.bool (Fpath.is_parent_dir (v "/..")) false ~__POS__;
  if windows then begin
    Test.bool (Fpath.is_parent_dir (v "\\\\.\\dev\\.")) false ~__POS__;
    Test.bool (Fpath.is_parent_dir (v "\\\\.\\dev\\.\\")) false ~__POS__;
    Test.bool (Fpath.is_parent_dir (v "\\\\server\\share\\.")) false ~__POS__;
    Test.bool (Fpath.is_parent_dir (v "\\\\server\\share\\.\\")) false ~__POS__;
    Test.bool (Fpath.is_parent_dir (v "C:..")) true ~__POS__;
    Test.bool (Fpath.is_parent_dir (v "C:../")) true ~__POS__;
    Test.bool (Fpath.is_parent_dir (v "C:../a/..")) false ~__POS__;
    Test.bool (Fpath.is_parent_dir ~prefix:true (v "C:../a/..")) true ~__POS__;
  end;
  ()

let is_dotfile () =
  Test.test "Fpath.is_dotfile" @@ fun () ->
  Test.bool (Fpath.is_dotfile (v ".")) false ~__POS__;
  Test.bool (Fpath.is_dotfile (v "..")) false ~__POS__;
  Test.bool (Fpath.is_dotfile (v "a/.")) false ~__POS__;
  Test.bool (Fpath.is_dotfile (v "a/..")) false ~__POS__;
  Test.bool (Fpath.is_dotfile (v "/a/.")) false ~__POS__;
  Test.bool (Fpath.is_dotfile (v "/a/..")) false ~__POS__;
  Test.bool (Fpath.is_dotfile (v "...")) true ~__POS__;
  Test.bool (Fpath.is_dotfile (v ".../")) true ~__POS__;
  Test.bool (Fpath.is_dotfile (v "a/...")) true ~__POS__;
  Test.bool (Fpath.is_dotfile (v "a/.../")) true ~__POS__;
  Test.bool (Fpath.is_dotfile (v "/a/...")) true ~__POS__;
  Test.bool (Fpath.is_dotfile (v "/a/.../")) true ~__POS__;
  Test.bool (Fpath.is_dotfile (v "/a/.../a")) false ~__POS__;
  if windows then begin
    Test.bool (Fpath.is_dotfile (v "\\\\.\\dev\\.")) false ~__POS__;
    Test.bool (Fpath.is_dotfile (v "\\\\.\\dev\\.\\")) false ~__POS__;
    Test.bool (Fpath.is_dotfile (v "\\\\server\\share\\.")) false ~__POS__;
    Test.bool (Fpath.is_dotfile (v "\\\\server\\share\\.\\")) false ~__POS__;
    Test.bool (Fpath.is_dotfile (v "C:.")) false ~__POS__;
    Test.bool (Fpath.is_dotfile (v "C:./")) false ~__POS__;
    Test.bool (Fpath.is_dotfile (v "C:./a/..")) false ~__POS__;
    Test.bool (Fpath.is_dotfile (v "C:..")) false ~__POS__;
    Test.bool (Fpath.is_dotfile (v "C:../")) false ~__POS__;
    Test.bool (Fpath.is_dotfile (v "C:../a/..")) false ~__POS__;
    Test.bool (Fpath.is_dotfile (v "C:../a/...")) true ~__POS__;
    Test.bool (Fpath.is_dotfile (v "C:...")) true ~__POS__;
  end;
  ()

let get_ext () =
  Test.test "Fpath.get_ext" @@ fun () ->
  let eq_ext ?__POS__:pos ?multi p e =
    Test.block ?__POS__:pos @@ fun () ->
    let p = Fpath.v p in
    Test.string (Fpath.get_ext ?multi p) e ~__POS__;
    Test.string Fpath.(get_ext ?multi (to_dir_path p)) e ~__POS__;
  in
  eq_ext "/" "" ~__POS__;
  eq_ext "a/b" "" ~__POS__;
  eq_ext "a/b.mli/.." "" ~__POS__;
  eq_ext "a/b.mli/..." "" ~__POS__;
  eq_ext "a/b." "." ~__POS__;
  eq_ext "a/b.mli" ".mli" ~__POS__;
  eq_ext ~multi:true "a/b.mli" ".mli" ~__POS__;
  eq_ext "a/b.mli/" ".mli" ~__POS__;
  eq_ext "a/.ocamlinit" "" ~__POS__;
  eq_ext "a.tar.gz" ".gz" ~__POS__;
  eq_ext ~multi:true "a.tar.gz" ".tar.gz" ~__POS__;
  eq_ext "a/.emacs.d" ".d" ~__POS__;
  eq_ext "a/.emacs.d/" ".d" ~__POS__;
  eq_ext ~multi:true "a/.emacs.d" ".d" ~__POS__;
  eq_ext "." "" ~__POS__;
  eq_ext ".." "" ~__POS__;
  eq_ext "..." "" ~__POS__;
  eq_ext "...." "" ~__POS__;
  eq_ext "....." "" ~__POS__;
  eq_ext ".a" "" ~__POS__;
  eq_ext ".a." "." ~__POS__;
  eq_ext ".a.." "." ~__POS__;
  eq_ext ".a..." "." ~__POS__;
  eq_ext ".a...." "." ~__POS__;
  eq_ext "a/..." "" ~__POS__;
  eq_ext "a.mli/." "" ~__POS__;
  eq_ext "a.mli/.." "" ~__POS__;
  eq_ext "a/.a" "" ~__POS__;
  eq_ext "a/..b" "" ~__POS__;
  eq_ext "a/..b.a" ".a" ~__POS__;
  eq_ext "a/..b..ac" ".ac" ~__POS__;
  eq_ext "/a/b" "" ~__POS__;
  eq_ext "/a/b." "." ~__POS__;
  eq_ext "./a." "." ~__POS__;
  eq_ext "./a.." "." ~__POS__;
  eq_ext "./.a." "." ~__POS__;
  eq_ext ~multi:true "." "" ~__POS__;
  eq_ext ~multi:true ".." "" ~__POS__;
  eq_ext ~multi:true "..." "" ~__POS__;
  eq_ext ~multi:true "...." "" ~__POS__;
  eq_ext ~multi:true "....." "" ~__POS__;
  eq_ext ~multi:true ".a" "" ~__POS__;
  eq_ext ~multi:true ".a." "." ~__POS__;
  eq_ext ~multi:true ".a.." ".." ~__POS__;
  eq_ext ~multi:true ".a..." "..." ~__POS__;
  eq_ext ~multi:true ".a...." "...." ~__POS__;
  eq_ext ~multi:true "a/..." "" ~__POS__;
  eq_ext ~multi:true "a/.a" "" ~__POS__;
  eq_ext ~multi:true "a/.." "" ~__POS__;
  eq_ext ~multi:true "a/..b" "" ~__POS__;
  eq_ext ~multi:true "a/..b.a" ".a" ~__POS__;
  eq_ext ~multi:true "a/..b..ac" "..ac" ~__POS__;
  eq_ext ~multi:true "a/.emacs.d" ".d" ~__POS__;
  eq_ext ~multi:true "/a/b.mli" ".mli" ~__POS__;
  eq_ext ~multi:true "a.tar.gz" ".tar.gz" ~__POS__;
  eq_ext ~multi:true "./a." "." ~__POS__;
  eq_ext ~multi:true "./a.." ".." ~__POS__;
  eq_ext ~multi:true "./.a." "." ~__POS__;
  eq_ext ~multi:true "./.a.." ".." ~__POS__;
  ()

let has_ext () =
  Test.test "Fpath.has_ext" @@ fun () ->
  let has_ext ?__POS__:pos e p bool =
    Test.block ?__POS__:pos @@ fun () ->
    let p = Fpath.v p in
    Test.bool (Fpath.has_ext e p) bool ~__POS__;
    Test.bool (Fpath.has_ext e (Fpath.to_dir_path p)) bool ~__POS__;
  in
  has_ext "mli" "a/b.mli" true ~__POS__;
  has_ext ".mli" "a/b.mli" true ~__POS__;
  has_ext ".mli" "a/b.mli/" true ~__POS__;
  has_ext ".mli" "a/bmli" false ~__POS__;
  has_ext ".tar.gz" "a/f.tar.gz" true ~__POS__;
  has_ext "tar.gz" "a/f.tar.gz" true ~__POS__;
  has_ext ".gz" "a/f.tar.gz" true ~__POS__;
  has_ext ".tar" "a/f.tar.gz" false ~__POS__;
  has_ext ".cache" "a/.cache" false ~__POS__;
  has_ext "" "a/b" false ~__POS__;
  has_ext "" "a/b." true ~__POS__;
  has_ext "." "a/b." true ~__POS__;
  has_ext "." "." false ~__POS__;
  has_ext "." ".." false ~__POS__;
  has_ext "." "..." false ~__POS__;
  has_ext "." "...a" false ~__POS__;
  has_ext "." "...a." true ~__POS__;
  has_ext "." "...a.." true ~__POS__;
  has_ext ".." "...a.." true ~__POS__;
  has_ext ".." "...a.." true ~__POS__;
  has_ext "" "." false ~__POS__;
  has_ext "" ".." false ~__POS__;
  has_ext "" "..." false ~__POS__;
  has_ext "" "...a" false ~__POS__;
  has_ext "" "...a." true ~__POS__;
  has_ext "" "...a.." true ~__POS__;
  has_ext ".." "." false ~__POS__;
  has_ext ".." ".." false ~__POS__;
  has_ext ".." "..a." false ~__POS__;
  has_ext ".." "..a.." true ~__POS__;
  has_ext ".." "..." false ~__POS__;
  has_ext ".." "...a." false ~__POS__;
  has_ext ".." "...a.." true ~__POS__;
  has_ext "..." ".." false ~__POS__;
  has_ext "..." "..." false ~__POS__;
  has_ext "..." "...." false ~__POS__;
  has_ext "..." ".a..." true ~__POS__;
  has_ext "tar.gz" "a/ftar.gz" false ~__POS__;
  has_ext "tar.gz" "a/tar.gz" false ~__POS__;
  has_ext "tar.gz" "a/.tar.gz" false ~__POS__;
  has_ext ".tar" "a/f.tar.gz" false ~__POS__;
  has_ext ".ocamlinit" ".ocamlinit" false ~__POS__;
  has_ext ".ocamlinit/" ".ocamlinit" false ~__POS__;
  has_ext ".ocamlinit" "..ocamlinit" false ~__POS__;
  has_ext "..ocamlinit" "...ocamlinit" false ~__POS__;
  has_ext "..ocamlinit" ".a..ocamlinit" true ~__POS__;
  has_ext "..a" ".." false ~__POS__;
  ()

let exists_ext () =
  Test.test "Fpath.exists_ext" @@ fun () ->
  let exists_ext ?__POS__:pos ?multi p bool =
    Test.block ?__POS__:pos @@ fun () ->
    let p = Fpath.v p in
    Test.bool (Fpath.exists_ext ?multi p) bool ~__POS__;
    Test.bool (Fpath.exists_ext ?multi (Fpath.to_dir_path p)) bool ~__POS__;
  in
  exists_ext "a/f" false ~__POS__;
  exists_ext "a/f." true ~__POS__;
  exists_ext "a/f.gz" true ~__POS__;
  exists_ext ~multi:true "a/f.gz" false ~__POS__;
  exists_ext "a/f.tar.gz" true ~__POS__;
  exists_ext ~multi:true "a/f.tar.gz" true ~__POS__;
  exists_ext "a/f.tar.gz/" true ~__POS__;
  exists_ext ".emacs.d" true ~__POS__;
  exists_ext ".emacs.d/" true ~__POS__;
  exists_ext ~multi:true ".emacs.d/" false ~__POS__;
  exists_ext ~multi:true "..emacs.d/" false ~__POS__;
  exists_ext ~multi:true "..emacs..d/" true ~__POS__;
  exists_ext ".ocamlinit" false ~__POS__;
  exists_ext ~multi:true "a/.a.." true ~__POS__;
  exists_ext "a/.a." true ~__POS__;
  exists_ext "a/..." false ~__POS__;
  exists_ext "a/.." false ~__POS__;
  exists_ext "a/." false ~__POS__;
  ()

let add_ext () =
  Test.test "Fpath.add_ext" @@ fun () ->
  Test.invalid_arg (fun () -> Fpath.add_ext "/" (v "a/b/c"));
  let eq_add_ext ?__POS__:pos ext p p' =
    Test.block ?__POS__:pos @@ fun () ->
    let p, p' = Fpath.v p, Fpath.v p' in
    eq (Fpath.add_ext ext p) p' ~__POS__;
    eq
      (Fpath.add_ext ext (Fpath.to_dir_path p)) (Fpath.to_dir_path p') ~__POS__;
  in
  eq_add_ext "mli" "a/b" "a/b.mli" ~__POS__;
  eq_add_ext ".mli" "a/b" "a/b.mli" ~__POS__;
  eq_add_ext ".mli" "a/b/" "a/b.mli/" ~__POS__;
  eq_add_ext ".mli" "/" "/" ~__POS__;
  eq_add_ext ".mli" "a/b/.." "a/b/.." ~__POS__;
  eq_add_ext "." "a/b" "a/b." ~__POS__;
  eq_add_ext "" "a/b" "a/b" ~__POS__;
  eq_add_ext "tar.gz" "a/f" "a/f.tar.gz" ~__POS__;
  eq_add_ext ".tar.gz" "a/f" "a/f.tar.gz" ~__POS__;
  eq_add_ext "gz" "a/f.tar" "a/f.tar.gz" ~__POS__;
  eq_add_ext ".gz" "a/f.tar" "a/f.tar.gz" ~__POS__;
  eq_add_ext "" "/" "/" ~__POS__;
  eq_add_ext "a" "/" "/" ~__POS__;
  eq_add_ext ".a" "/" "/" ~__POS__;
  ()

let rem_ext () =
  Test.test "Fpath.rem_ext" @@ fun () ->
  let eq_rem_ext ?__POS__:pos ?multi p p' =
    Test.block ?__POS__:pos @@ fun () ->
    let p, p' = Fpath.v p, Fpath.v p' in
    eq (Fpath.rem_ext ?multi p) p' ~__POS__;
    eq (Fpath.rem_ext ?multi (Fpath.to_dir_path p)) (Fpath.to_dir_path p')
      ~__POS__;
  in
  eq_rem_ext "/" "/" ~__POS__;
  eq_rem_ext "/a/b" "/a/b" ~__POS__;
  eq_rem_ext "/a/b.mli" "/a/b" ~__POS__;
  eq_rem_ext "/a/b.mli/" "/a/b/" ~__POS__;
  eq_rem_ext "/a/b.mli/.." "/a/b.mli/.." ~__POS__;
  eq_rem_ext "/a/b.mli/." "/a/b.mli/." ~__POS__;
  eq_rem_ext "a/.ocamlinit" "a/.ocamlinit" ~__POS__;
  eq_rem_ext ~multi:true "a/.ocamlinit" "a/.ocamlinit" ~__POS__;
  eq_rem_ext "a/.emacs.d" "a/.emacs" ~__POS__;
  eq_rem_ext "f.tar.gz" "f.tar" ~__POS__;
  eq_rem_ext ~multi:true "f.tar.gz" "f" ~__POS__;
  eq_rem_ext ~multi:true "f.tar.gz/" "f/" ~__POS__;
  eq_rem_ext "a/..." "a/..." ~__POS__;
  eq_rem_ext "a/..a." "a/..a" ~__POS__;
  eq_rem_ext "a/..a.." "a/..a." ~__POS__;
  eq_rem_ext ~multi:true "a/..a.." "a/..a" ~__POS__;
  eq_rem_ext ".tar.gz" ".tar" ~__POS__;
  eq_rem_ext ~multi:true "a/.tar.gz" "a/.tar" ~__POS__;
  eq_rem_ext ~multi:true ".tar" ".tar" ~__POS__;
  eq_rem_ext ~multi:true "/.tar" "/.tar" ~__POS__;
  ()

let set_ext () =
  Test.test "Fpath.set_ext" @@ fun () ->
  Test.invalid_arg ~__POS__ (fun () -> (Fpath.set_ext "/") (v "a/b/c"));
  let eq_set_ext ?__POS__:pos ?multi ext p p' =
    Test.block ?__POS__:pos @@ fun () ->
    let p, p' = Fpath.v p, Fpath.v p' in
    eq (Fpath.set_ext ?multi ext p) p' ~__POS__;
    eq (Fpath.set_ext ?multi ext (Fpath.to_dir_path p)) (Fpath.to_dir_path p')
      ~__POS__;
  in
  eq_set_ext ".bla" "/a/b" "/a/b.bla" ~__POS__;
  eq_set_ext "bla" "/a/b" "/a/b.bla" ~__POS__;
  eq_set_ext ".bla" "/a/b.mli" "/a/b.bla" ~__POS__;
  eq_set_ext "bla" "/a/b.mli" "/a/b.bla" ~__POS__;
  eq_set_ext "bla" "a/.ocamlinit" "a/.ocamlinit.bla" ~__POS__;
  eq_set_ext "bla" "a/.emacs.d" "a/.emacs.bla" ~__POS__;
  eq_set_ext "bla" "f.tar.gz" "f.tar.bla" ~__POS__;
  eq_set_ext ~multi:true "bla" "f.tar.gz" "f.bla" ~__POS__;
  eq_set_ext ~multi:true "" "f.tar.gz" "f" ~__POS__;
  ()

let split_ext () =
  Test.test "Fpath.split_ext" @@ fun () ->
  let eq_split ?__POS__:pos ?multi p q ext =
    Test.block ?__POS__:pos @@ fun () ->
    let p, q = Fpath.v p, Fpath.v q in
    let check ?__POS__:pos p q =
      Test.block ?__POS__:pos @@ fun () ->
      let q', ext' = Fpath.split_ext ?multi p in
      Test.string ext ext' ~__POS__;
      eq q q' ~__POS__;
      eq p (Fpath.add_ext ext q') ~__POS__;
    in
    check p q ~__POS__;
    check (Fpath.to_dir_path p) (Fpath.to_dir_path q) ~__POS__;
  in
  eq_split "/a/b" "/a/b" "" ~__POS__;
  eq_split "/a/b.mli" "/a/b" ".mli" ~__POS__;
  eq_split "a/.ocamlinit" "a/.ocamlinit" "" ~__POS__;
  eq_split "f.tar.gz" "f.tar" ".gz" ~__POS__;
  eq_split ~multi:true "f.tar.gz" "f" ".tar.gz" ~__POS__;
  eq_split ~multi:true ".tar" ".tar" "" ~__POS__;
  eq_split ~multi:true "/.tar" "/.tar" "" ~__POS__;
  eq_split ~multi:true "/.tar.gz" "/.tar" ".gz" ~__POS__;
  eq_split ~multi:true "/.tar.gz/.." "/.tar.gz/.." "" ~__POS__;
  ()

let main () =
  Test.main @@ fun () ->
  of_string ();
  dir_sep ();
  is_seg ();
  add_seg ();
  append ();
  split_volume ();
  segs ();
  is_dir_path ();
  is_file_path ();
  to_dir_path ();
  filename ();
  split_base ();
  base ();
  basename ();
  parent ();
  rem_empty_seg ();
  normalize ();
  is_prefix ();
  find_prefix ();
  rem_prefix ();
  relativize ();
  is_rooted ();
  is_abs_rel ();
  is_root ();
  is_current_dir ();
  is_parent_dir ();
  is_dotfile ();
  get_ext ();
  has_ext ();
  exists_ext ();
  add_ext ();
  rem_ext ();
  set_ext ();
  split_ext ();
  ()

let () = if !Sys.interactive then () else exit (main ())
