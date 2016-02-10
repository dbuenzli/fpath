(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open Testing

let windows = Sys.os_type = "Win32"

let eqp = eq ~eq:Fpath.equal ~pp:Fpath.pp
let v = Fpath.v

let of_string = test "Fpath.{v,of_string}" @@ fun () ->
  let eq = eq_option ~eq:Fpath.equal ~pp:Fpath.pp in
  let some s = (Some (v s)) in
  eq (Fpath.of_string "/\x00") None;
  eq (Fpath.of_string "/") (Some (Fpath.v "/"));
  eq_bool (Fpath.equal (v "/") (v "/ ")) false;
  eq (Fpath.of_string "//") (if windows then None else some "//");
  eq (Fpath.of_string "/a/b/c") (some "/a/b/c");
  eq_bool (Fpath.equal (v "/a/b/c/") (v "/a/b/c")) false;
  eq (Fpath.of_string "") (some "."); (* no empty path *)
  eq (Fpath.of_string "a///b///////c///") (some "a/b/c/"); (* seg collapse *)
  eq (Fpath.of_string "a///b///////c") (some "a/b/c"); (* seg collapse *)
  if windows then begin
    eq (Fpath.of_string "C:\x00") None;
    eq (Fpath.of_string "C:") (some "C:."); (* no empty path *)
    eq (Fpath.of_string "C:\\") (some "C:\\");
    eq (Fpath.of_string "C:rel") (some "C:rel");
    eq (Fpath.of_string "\\\\") None;
    eq (Fpath.of_string "\\\\server") None;
    eq (Fpath.of_string "\\\\server\\") None;
    eq (Fpath.of_string "\\\\server\\share")
      (some "\\\\server\\share\\") (* root add *);
    eq (Fpath.of_string "\\\\?") None;
    eq (Fpath.of_string "\\\\?\\") None;
    eq (Fpath.of_string "\\\\?\\a") None;
    eq (Fpath.of_string "\\\\?\\a:") (some "\\\\?\\a:\\"); (* root add *)
    eq (Fpath.of_string "\\\\?\\a:\\") (some "\\\\?\\a:\\");
    eq (Fpath.of_string "\\\\?\\a:\\c") (some "\\\\?\\a:\\c");
    eq (Fpath.of_string "\\\\?\\server\\") None;
    eq (Fpath.of_string "\\\\?\\server\\\\") None;
    eq (Fpath.of_string "\\\\?\\server\\share")
      (some "\\\\?\\server\\share\\"); (* root add *)
    eq (Fpath.of_string "\\\\?\\server\\\\share")
      (some "\\\\?\\server\\share\\"); (* seg collapse and root add *)
    eq (Fpath.of_string "\\\\?\\server\\share\\")
      (some "\\\\?\\server\\share\\");
    eq (Fpath.of_string "\\\\?\\server\\share\\a")
      (some "\\\\?\\server\\share\\a");
    eq (Fpath.of_string "\\\\?\\UNC") None;
    eq (Fpath.of_string "\\\\?\\UNC\\") None;
    eq (Fpath.of_string "\\\\?\\UNC\\server") None;
    eq (Fpath.of_string "\\\\?\\UNC\\server\\") None;
    eq (Fpath.of_string "\\\\?\\UNC\\server\\\\") None;
    eq (Fpath.of_string "\\\\?\\UNC\\server\\share")
      (some "\\\\?\\UNC\\server\\share\\"); (* root add *)
    eq (Fpath.of_string "\\\\?\\UNC\\server\\share\\")
      (some "\\\\?\\UNC\\server\\share\\");
    eq (Fpath.of_string "\\\\?\\UNC\\server\\share\\a")
      (some "\\\\?\\UNC\\server\\share\\a");
    eq (Fpath.of_string "\\\\.") None;
    eq (Fpath.of_string "\\\\.\\") None;
    eq (Fpath.of_string "\\\\.\\device") (some "\\\\.\\device\\") (* root add *);
    eq (Fpath.of_string "\\\\.\\device\\") (some "\\\\.\\device\\");
    eq (Fpath.of_string "\\\\.\\device\\a") (some "\\\\.\\device\\a");
  end;
  ()

let add_seg = test "Fpath.add_seg" @@ fun () ->
  app_raises ~pp:Fpath.pp (Fpath.add_seg (v "a/b/c")) "a\x00o";
  app_raises ~pp:Fpath.pp (Fpath.add_seg (v "a/b/c")) "a/o";
  if windows then app_raises ~pp:Fpath.pp (Fpath.add_seg (v "a/b/c")) "a\\o";
  eqp (Fpath.add_seg (v "/a") "b") (v "/a/b");
  eqp (Fpath.add_seg (v "/a/") "b") (v "/a/b");
  eqp (Fpath.add_seg (v "a/b") "") (v "a/b/");
  eqp (Fpath.add_seg (v "a/b/") "") (v "a/b/");
  eqp (Fpath.add_seg (v "/a/b") "") (v "/a/b/");
  eqp (Fpath.add_seg (v "/a/b/") "") (v "/a/b/");
  eqp (Fpath.add_seg (v "/a/b/") "e") (v "/a/b/e");
  eqp (Fpath.add_seg (v "/a/b") "e") (v "/a/b/e");
  eqp (Fpath.add_seg (v "/") "") (v "/");
  eqp (Fpath.add_seg (v "/") "a") (v "/a");
  eqp (Fpath.add_seg (v ".") "a") (v "./a");
  eqp (Fpath.add_seg (v ".") "") (v "./");
  eqp (Fpath.add_seg (v "..") "a") (v "../a");
  eqp (Fpath.add_seg (v "..") "") (v "../");
  ()

let append = test "Fpath.append" @@ fun () ->
  eqp (Fpath.append (v "/a/b/") (v "e/f")) (v "/a/b/e/f");
  eqp (Fpath.append (v "/a/b") (v "e/f")) (v "/a/b/e/f");
  eqp (Fpath.append (v "/a/b/") (v "/e/f")) (v "/e/f");
  eqp (Fpath.append (v "a/b/") (v "e/f")) (v "a/b/e/f");
  eqp (Fpath.append (v "bla") (v "/bli")) (v "/bli");
  if not windows then eqp (Fpath.append (v "bla") (v "//bli")) (v "//bli");
  if windows then begin
    eqp (Fpath.append (v "a/b") (v "C:e")) (v "C:e");
    eqp (Fpath.append (v "C:") (v "blu")) (v "C:.\\blu");
    eqp (Fpath.append (v "C:bla") (v "blu")) (v "C:bla/blu");
    eqp (Fpath.append (v "C:\\bla") (v "blu")) (v "C:\\bla\\blu");
    eqp (Fpath.append (v "C:\\bla") (v "\\blu")) (v "\\blu");
    eqp (Fpath.append (v "\\\\srv\\share\\a") (v "b"))
      (v "\\\\srv\\share\\a\\b");
    eqp (Fpath.append (v "\\\\srv\\share\\a\\") (v "b"))
      (v "\\\\srv\\share\\a\\b");
  end;
  ()

let constants = test "Constants" @@ fun () ->
  eq_str Fpath.dir_sep (if windows then "\\" else "/");
  ()

let is_seg_valid = test "Fpath.is_seg_valid" @@ fun () ->
  eq_bool (Fpath.is_seg_valid "abc") true;
  eq_bool (Fpath.is_seg_valid "ab/c") false;
  eq_bool (Fpath.is_seg_valid "ab\x00c") false;
  if windows then eq_bool (Fpath.is_seg_valid "ab\\c") false;
  ()

let is_abs_rel = test "Fpath.is_abs_rel" @@ fun () ->
  let is_abs bool p =
    let p = v p in
    eq_bool (Fpath.is_abs p) bool;
    eq_bool (Fpath.is_rel p) (not bool);
  in
  is_abs true "/a/b/c";
  if not windows then is_abs true "//a/b/c";
  is_abs false ".";
  is_abs false "..";
  is_abs false "../";
  is_abs false "a";
  is_abs false "a/b";
  is_abs true "/";
  if windows then begin
    is_abs false "C:.";
    is_abs true "C:\\";
    is_abs true "C:/";
    is_abs false "C:bli/bla";
    is_abs false "C:bli/bla";
    is_abs false  "C:rel";
    is_abs true "\\\\server\\share\\";
    is_abs true "\\\\?\\a:\\";
    is_abs true "\\\\?\\a:\\c";
    is_abs true "\\\\?\\server\\share\\";
    is_abs true "\\\\?\\server\\share\\a";
    is_abs true "\\\\?\\UNC\\server\\share\\";
    is_abs true "\\\\?\\UNC\\server\\share\\a";
    is_abs true "\\\\.\\device\\";
    is_abs true "\\\\.\\device\\a";
  end;
  ()

let is_dotfile = test "Fpath.is_dotfile" @@ fun () ->
  eq_bool (Fpath.is_dotfile (v ".")) false;
  eq_bool (Fpath.is_dotfile (v "..")) false;
  eq_bool (Fpath.is_dotfile (v "a/.")) false;
  eq_bool (Fpath.is_dotfile (v "a/..")) false;
  eq_bool (Fpath.is_dotfile (v "/a/.")) false;
  eq_bool (Fpath.is_dotfile (v "/a/..")) false;
  eq_bool (Fpath.is_dotfile (v "...")) true;
  eq_bool (Fpath.is_dotfile (v ".../")) true;
  eq_bool (Fpath.is_dotfile (v "a/...")) true;
  eq_bool (Fpath.is_dotfile (v "a/.../")) true;
  eq_bool (Fpath.is_dotfile (v "/a/...")) true;
  eq_bool (Fpath.is_dotfile (v "/a/.../")) true;
  eq_bool (Fpath.is_dotfile (v "/a/.../a")) false;
  ()

let is_root = test "Fpath.is_root" @@ fun () ->
  if not windows then (eq_bool (Fpath.is_root (v "//")) true);
  eq_bool (Fpath.is_root (v "/")) true;
  eq_bool (Fpath.is_root (v "/a")) false;
  eq_bool (Fpath.is_root (v "a")) false;
  eq_bool (Fpath.is_root (v ".")) false;
  eq_bool (Fpath.is_root (v "..")) false;
  if windows then begin
    eq_bool (Fpath.is_root (v "\\\\.\\dev\\")) true;
    eq_bool (Fpath.is_root (v "\\\\.\\dev\\a")) false;
    eq_bool (Fpath.is_root (v "\\\\server\\share\\")) true;
    eq_bool (Fpath.is_root (v "\\\\server\\share\\a")) false;
    eq_bool (Fpath.is_root (v "C:\\")) true;
    eq_bool (Fpath.is_root (v "C:a")) false;
    eq_bool (Fpath.is_root (v "C:\\a")) false;
  end;
  ()

let is_prefix = test "Fpath.is_prefix" @@ fun () ->
  eq_bool (Fpath.is_prefix (v "/a/b") (v "/a/b")) true;
  eq_bool (Fpath.is_prefix (v "/a/b") (v "/a/b/")) true;
  eq_bool (Fpath.is_prefix (v "/a/b") (v "/a/bc")) false;
  eq_bool (Fpath.is_prefix (v "/a/b") (v "/a/b/c")) true;
  eq_bool (Fpath.is_prefix (v "/a/b/") (v "/a/b/c")) true;
  eq_bool (Fpath.is_prefix (v "/a/b/") (v "/a/b")) false;
  eq_bool (Fpath.is_prefix (v "/a/b/") (v "/a/b")) false;
  eq_bool (Fpath.is_prefix (v "a/b") (v "/a/b")) false;
  eq_bool (Fpath.is_prefix (v "abcd/") (v "abcd")) false;
  eq_bool (Fpath.is_prefix (v "abcd") (v "abcd/bla")) true;
  if not windows then begin
    eq_bool (Fpath.is_prefix (v "//a/b") (v "/a/b")) false
  end;
  if windows then begin
    eq_bool (Fpath.is_prefix (v "C:a") (v "a")) false;
  end;
  ()

let split_volume = test "Fpath.split_volume" @@ fun () ->
  let eq_split p vol q =
    let p = v p in
    let vol', q' = Fpath.split_volume p in
    eq_str vol vol';
    eqp (v q) q';
    eqp (v (vol' ^ (Fpath.to_string q'))) p
  in
  eq_split "/bla" "" "/bla";
  eq_split "bla" "" "bla";
  eq_split "bla/a" "" "bla/a";
  eq_split "bla/a/" "" "bla/a/";
  if not windows then begin
    eq_split "//" "/" "/";
    eq_split "//a/b/c" "/" "/a/b/c";
    eq_split "//a/b/c/" "/" "/a/b/c/";
  end;
  if windows then begin
    eq_split "C:." "C:" ".";
    eq_split "C:\\" "C:" "\\";
    eq_split "C:\\a" "C:" "\\a";
    eq_split "C:rel" "C:" "rel";
    eq_split "\\\\server\\share\\" "\\\\server\\share" "\\";
    eq_split "\\\\server\\share\\a" "\\\\server\\share" "\\a";
    eq_split "\\\\?\\a:\\" "\\\\?\\a:" "\\";
    eq_split "\\\\?\\a:\\c" "\\\\?\\a:" "\\c";
    eq_split "\\\\?\\server\\share\\" "\\\\?\\server\\share" "\\";
    eq_split "\\\\?\\server\\share\\a" "\\\\?\\server\\share" "\\a";
    eq_split "\\\\?\\UNC\\server\\share\\" "\\\\?\\UNC\\server\\share" "\\";
    eq_split "\\\\?\\UNC\\server\\share\\a" "\\\\?\\UNC\\server\\share" "\\a";
    eq_split "\\\\.\\device\\" "\\\\.\\device" "\\";
    eq_split "\\\\.\\device\\a" "\\\\.\\device" "\\a";
  end;
  ()

let segs = test "Fpath.segs" @@ fun () ->
  let eq = eq_list ~eq:(=) ~pp:pp_str in
  eq (Fpath.segs @@ v "/a/b/") [""; "a"; "b"; ""];
  eq (Fpath.segs @@ v "/a/b") [""; "a"; "b"];
  eq (Fpath.segs @@ v "a/b/") ["a"; "b"; ""];
  eq (Fpath.segs @@ v "a/b") ["a"; "b"];
  eq (Fpath.segs @@ v "a") ["a"];
  eq (Fpath.segs @@ v "/") [""; ""];
  eq (Fpath.segs @@ v "/a/b/c") [""; "a"; "b"; "c"];
  eq (Fpath.segs @@ v "/a/b/c/") [""; "a"; "b"; "c"; ""];
  eq (Fpath.segs @@ v "a/b/c") ["a"; "b"; "c";];
  eq (Fpath.segs @@ v "a/b/c/") ["a"; "b"; "c"; ""];
  if not windows then begin
    eq (Fpath.segs @@ v "//") [""; ""];
    eq (Fpath.segs @@ v "//a/b") [""; "a"; "b"];
  end;
  if windows then begin
    eq (Fpath.segs @@ v "C:\\bla") [""; "bla"];
    eq (Fpath.segs @@ v "C:bla") ["bla"];
    eq (Fpath.segs @@ v "\\\\Server\\share\\bla") [""; "bla"];
    eq (Fpath.segs @@ v "\\\\?\\C:\\bla") ["";"bla"];
    eq (Fpath.segs @@ v "\\\\?\\Server\\share\\bla") [""; "bla"];
    eq (Fpath.segs @@ v "\\\\?\\UNC\\Server\\share\\bla") [""; "bla"];
    eq (Fpath.segs @@ v "\\\\.\\dev\\bla") [""; "bla"];
    eq (Fpath.segs @@ v "\\a") [""; "a"];
    eq (Fpath.segs @@ v "\\a\\b") [""; "a"; "b"];
    eq (Fpath.segs @@ v "\\a\\b\\") [""; "a"; "b"; ""];
    eq (Fpath.segs @@ v "C:.") ["."];
    eq (Fpath.segs @@ v "C:\\") ["";""];
    eq (Fpath.segs @@ v "C:\\a") ["";"a"];
    eq (Fpath.segs @@ v "C:rel") ["rel";];
    eq (Fpath.segs @@ v "\\\\server\\share\\") [""; ""];
    eq (Fpath.segs @@ v "\\\\server\\share\\a") [""; "a"];
    eq (Fpath.segs @@ v "\\\\?\\a:\\") [""; ""];
    eq (Fpath.segs @@ v "\\\\?\\a:\\c") [""; "c"];
    eq (Fpath.segs @@ v "\\\\?\\server\\share\\") [""; ""];
    eq (Fpath.segs @@ v "\\\\?\\server\\share\\a") [""; "a"];
    eq (Fpath.segs @@ v "\\\\?\\UNC\\server\\share\\") [""; ""];
    eq (Fpath.segs @@ v "\\\\?\\UNC\\server\\share\\a") [""; "a"];
    eq (Fpath.segs @@ v "\\\\.\\device\\") ["";""];
    eq (Fpath.segs @@ v "\\\\.\\device\\a") ["";"a"];
    eq (Fpath.segs @@ v "\\\\server\\share\\a") ["";"a"];
    eq (Fpath.segs @@ v "C:a") ["a"];
    eq (Fpath.segs @@ v "C:\\a") ["";"a"];
  end;
  ()

let name = test "Fpath.name" @@ fun () ->
  eq_str (Fpath.name @@ v "/a/b/") "b";
  eq_str (Fpath.name @@ v "/a/b") "b";
  eq_str (Fpath.name @@ v "a") "a";
  eq_str (Fpath.name @@ v "a/") "a";
  eq_str (Fpath.name @@ v "/") "";
  if not windows then begin
    eq_str (Fpath.name @@ v "//") "";
    eq_str (Fpath.name @@ v "//a/b") "b";
    eq_str (Fpath.name @@ v "//a/b/") "b";
  end;
  if windows then begin
    eq_str (Fpath.name @@ v "\\\\server\\share\\a") "a";
    eq_str (Fpath.name @@ v "\\\\server\\share\\a\\") "a";
    eq_str (Fpath.name @@ v "\\\\.\\device\\") "";
    eq_str (Fpath.name @@ v "\\\\.\\device\\a") "a";
    eq_str (Fpath.name @@ v "C:\\") "";
    eq_str (Fpath.name @@ v "C:a") "a";
  end;
  ()

let filename = test "Fpath.filename" @@ fun () ->
  eq_str (Fpath.filename @@ v "/a/b/") "";
  eq_str (Fpath.filename @@ v "/a/b") "b";
  eq_str (Fpath.filename @@ v "a") "a";
  eq_str (Fpath.filename @@ v "a/") "";
  eq_str (Fpath.filename @@ v "/") "";
  if not windows then begin
    eq_str (Fpath.filename @@ v "//") "";
    eq_str (Fpath.filename @@ v "//a/b") "b";
    eq_str (Fpath.filename @@ v "//a/b/") "";
  end;
  if windows then begin
    eq_str (Fpath.filename @@ v "\\\\server\\share\\a") "a";
    eq_str (Fpath.filename @@ v "\\\\.\\device\\") "";
    eq_str (Fpath.filename @@ v "\\\\.\\device\\a") "a";
    eq_str (Fpath.filename @@ v "C:\\") "";
    eq_str (Fpath.filename @@ v "C:a") "a";
  end;
  ()

let base = test "Fpath.base" @@ fun () ->
  eqp (Fpath.base @@ v "/a/b/") (v "b");
  eqp (Fpath.base @@ v "/a/b") (v "b");
  eqp (Fpath.base @@ v "a") (v "a");
  eqp (Fpath.base @@ v "a/") (v "a");
  eqp (Fpath.base @@ v "ab") (v "ab");
  eqp (Fpath.base @@ v "ab/") (v "ab");
  eqp (Fpath.base @@ v ".") (v ".");
  eqp (Fpath.base @@ v "..") (v "..");
  eqp (Fpath.base @@ v "/") (v "/");
  if not windows then begin
    eqp (Fpath.base @@ v "//") (v "//");
    eqp (Fpath.base @@ v "//a/b") (v "b");
    eqp (Fpath.base @@ v "//a/b/") (v "b");
  end;
  if windows then begin
    eqp (Fpath.base @@ v "\\\\server\\share\\") (v "\\\\server\\share\\");
    eqp (Fpath.base @@ v "\\\\server\\share\\a") (v "a");
    eqp (Fpath.base @@ v "\\\\server\\share\\a\\") (v "a");
    eqp (Fpath.base @@ v "C:\\") (v "C:\\");
    eqp (Fpath.base @@ v "C:\\") (v "C:\\");
    eqp (Fpath.base @@ v "C:\\a\\") (v "a");
    eqp (Fpath.base @@ v "C:\\a") (v "a");
    eqp (Fpath.base @@ v "C:a\\") (v "a");
  end;
  ()

let parent = test "Fpath.parent" @@ fun () ->
  eqp (Fpath.parent @@ v "/a/b") (v "/a");
  eqp (Fpath.parent @@ v "/a/b/") (v "/a");
  eqp (Fpath.parent @@ v "/a") (v "/");
  eqp (Fpath.parent @@ v "/a/") (v "/");
  eqp (Fpath.parent @@ v "a/b/") (v "a");
  eqp (Fpath.parent @@ v "a/b") (v "a");
  eqp (Fpath.parent @@ v "a") (v ".");
  eqp (Fpath.parent @@ v "a/") (v ".");
  eqp (Fpath.parent @@ v ".") (v ".");
  eqp (Fpath.parent @@ v "..") (v ".");
  eqp (Fpath.parent @@ v "/") (v "/");
  eqp (Fpath.parent @@ v "/aab") (v "/");
  if not windows then begin
    eqp (Fpath.parent @@ v "//") (v "//");
    eqp (Fpath.parent @@ v "//a/b") (v "//a");
    eqp (Fpath.parent @@ v "//a/b/") (v "//a");
    eqp (Fpath.parent @@ v "//a") (v "//");
    eqp (Fpath.parent @@ v "//abcd") (v "//");
  end;
  if windows then begin
    eqp (Fpath.parent @@ v "\\\\server\\share\\") (v "\\\\server\\share\\");
    eqp (Fpath.parent @@ v "C:a") (v "C:.");
    eqp (Fpath.parent @@ v "C:\\") (v "C:\\");
  end;
  ()

let file_to_dir = test "Fpath.file_to_dir" @@ fun () ->
  eqp (Fpath.file_to_dir @@ v "/a/b") (v "/a/b/");
  eqp (Fpath.file_to_dir @@ v "/a/b/") (v "/a/b/");
  eqp (Fpath.file_to_dir @@ v "a") (v "a/");
  eqp (Fpath.file_to_dir @@ v "a/") (v "a/");
  eqp (Fpath.file_to_dir @@ v "/") (v "/");
  if not windows then begin
    eqp (Fpath.file_to_dir @@ v "//") (v "//");
    eqp (Fpath.file_to_dir @@ v "//a") (v "//a/");
  end;
  if windows then begin
    eqp (Fpath.file_to_dir @@ v "\\\\server\\share\\") (v "\\\\server\\share\\");
    eqp (Fpath.file_to_dir @@ v "C:a") (v "C:a/");
    eqp (Fpath.file_to_dir @@ v "C:\\") (v "C:\\");
  end;
  ()

let dir_to_file = test "Fpath.dir_to_file" @@ fun () ->
  eqp (Fpath.dir_to_file @@ v "/a/b") (v "/a/b");
  eqp (Fpath.dir_to_file @@ v "/a/b/") (v "/a/b");
  eqp (Fpath.dir_to_file @@ v "a") (v "a");
  eqp (Fpath.dir_to_file @@ v "a/") (v "a");
  eqp (Fpath.dir_to_file @@ v "/") (v "/");
  if not windows then begin
    eqp (Fpath.dir_to_file @@ v "//") (v "//");
    eqp (Fpath.dir_to_file @@ v "//a") (v "//a");
    eqp (Fpath.dir_to_file @@ v "//a/") (v "//a");
  end;
  if windows then begin
    eqp (Fpath.dir_to_file @@ v "\\\\server\\share\\") (v "\\\\server\\share\\");
    eqp (Fpath.dir_to_file @@ v "\\\\server\\share\\a\\")
      (v "\\\\server\\share\\a");
    eqp (Fpath.dir_to_file @@ v "C:a") (v "C:a");
    eqp (Fpath.dir_to_file @@ v "C:a/") (v "C:a");
    eqp (Fpath.dir_to_file @@ v "C:\\") (v "C:\\");
  end;
  ()

let find_prefix = test "Fpath.find_prefix" @@ fun () ->
  let eq = eq_option ~eq:Fpath.equal ~pp:Fpath.pp in
  eq (Fpath.find_prefix (v "a/b/c") (v "a/b/d")) (Some (v "a/b/"));
  eq (Fpath.find_prefix (v "a/b/c") (v "a/b/cd")) (Some (v "a/b/"));
  eq (Fpath.find_prefix (v "/a/b/c") (v "/a/b/d")) (Some (v "/a/b/"));
  eq (Fpath.find_prefix (v "a/b") (v "e/f")) (Some (v "."));
  eq (Fpath.find_prefix (v "/a/b") (v "/e/f")) (Some (v "/"));
  eq (Fpath.find_prefix (v "/a/b") (v "e/f")) None;
  eq (Fpath.find_prefix (v "a/b") (v "/e/f")) None;
  eq (Fpath.find_prefix (v "ab") (v "abc")) (Some (v "."));
  eq (Fpath.find_prefix (v "ab") (v "ab")) (Some (v "ab"));
  eq (Fpath.find_prefix (v "/") (v "/")) (Some (v "/"));
  eq (Fpath.find_prefix (v "a/") (v "a")) (Some (v "a"));
  eq (Fpath.find_prefix (v "abc/") (v "abc")) (Some (v "abc"));
  eq (Fpath.find_prefix (v "abcd/") (v "abc")) (Some (v "."));
  eq (Fpath.find_prefix (v "a/") (v "a/a")) (Some (v "a/"));
  if not windows then begin
    eq (Fpath.find_prefix (v "//") (v "/a/b")) None;
    eq (Fpath.find_prefix (v "//a/b/c") (v "/")) None;
    eq (Fpath.find_prefix (v "//a/b/c") (v "//")) (Some (v "//"));
    eq (Fpath.find_prefix (v "//a/b") (v "/a/b")) None;
    eq (Fpath.find_prefix (v "//") (v "/")) None;
    eq (Fpath.find_prefix (v "//a/c") (v "/a/b")) None;
    eq (Fpath.find_prefix (v "//a/c") (v "a/b")) None;
  end;
  if windows then begin
    eq (Fpath.find_prefix (v "C:\\a") (v "\\a")) None;
    eq (Fpath.find_prefix (v "C:\\a") (v "C:\\a")) (Some (v "C:\\a"));
    eq (Fpath.find_prefix (v "C:a") (v "C:a")) (Some (v "C:a"));
    eq (Fpath.find_prefix (v "C:a") (v "C:b")) (Some (v "C:."));
    eq (Fpath.find_prefix (v "C:a") (v "C:b/c")) (Some (v "C:."));
    eq (Fpath.find_prefix (v "C:a/f") (v "C:b/c")) (Some (v "C:."));
    eq (Fpath.find_prefix (v "C:a/f") (v "C:/b/c")) None;
    eq (Fpath.find_prefix (v "C:\\") (v "C:\\")) (Some (v "C:\\"));
    eq (Fpath.find_prefix (v "\\\\server\\share\\") (v "\\\\server\\share\\"))
      (Some (v "\\\\server\\share\\"));
    eq (Fpath.find_prefix (v "\\\\server\\share\\") (v "\\\\server\\share\\a"))
      (Some (v "\\\\server\\share\\"));
    eq (Fpath.find_prefix (v "\\\\server\\share\\a") (v "\\\\server\\share\\a"))
      (Some (v "\\\\server\\share\\a"));
    eq (Fpath.find_prefix (v "\\\\server\\share\\a") (v "\\\\server\\share\\b"))
      (Some (v "\\\\server\\share\\"));
  end;
  ()

let rem_prefix = test "Fpath.rem_prefix" @@ fun () ->
  let eq = eq_option ~eq:Fpath.equal ~pp:Fpath.pp in
  eq (Fpath.rem_prefix (v "/a/b") (v "/a/bc")) None;
  eq (Fpath.rem_prefix (v "/a/b") (v "/a/b")) (Some (v "."));
  eq (Fpath.rem_prefix (v "/a/b/") (v "/a/b")) None;
  eq (Fpath.rem_prefix (v "/a/b") (v "/a/b/")) (Some (v "."));
  eq (Fpath.rem_prefix (v "/a/b/") (v "/a/b/")) (Some (v "."));
  eq (Fpath.rem_prefix (v "/a/b") (v "/a/b/c")) (Some (v "c"));
  eq (Fpath.rem_prefix (v "/a/b/") (v "/a/b/c")) (Some (v "c"));
  eq (Fpath.rem_prefix (v "a") (v "a/b/c")) (Some (v "b/c"));
  ()

let normalize = test "Fpath.normalize" @@ fun () ->
  eqp (Fpath.normalize (v ".")) (v ".");
  eqp (Fpath.normalize (v "././.")) (v ".");
  eqp (Fpath.normalize (v "./././")) (v ".");
  eqp (Fpath.normalize (v "./a/..")) (v ".");
  eqp (Fpath.normalize (v "./a/../")) (v ".");
  eqp (Fpath.normalize (v "..")) (v "..");
  eqp (Fpath.normalize (v "../../../a")) (v "../../../a");
  eqp (Fpath.normalize (v "../../../a/")) (v "../../../a");
  eqp (Fpath.normalize (v "/")) (v "/");
  eqp (Fpath.normalize (v "/.")) (v "/");
  eqp (Fpath.normalize (v "/..")) (v "/");
  eqp (Fpath.normalize (v "/./../../.")) (v "/");
  eqp (Fpath.normalize (v "/./../../.")) (v "/");
  eqp (Fpath.normalize (v "../../a/..")) (v "../..");
  eqp (Fpath.normalize (v "../../a/../.")) (v "../..");
  eqp (Fpath.normalize (v "../../a/.././..")) (v "../../..");
  eqp (Fpath.normalize (v "../../a/../..")) (v "../../..");
  eqp (Fpath.normalize (v "/a/b/c/./../../g")) (v "/a/g");
  eqp (Fpath.normalize (v "./a/b/c/./../../g")) (v "a/g");
  eqp (Fpath.normalize (v "./a/b/c/./../../g/")) (v "a/g");
  eqp (Fpath.normalize (v "a/b/c/./../../g")) (v "a/g");
  eqp (Fpath.normalize (v "a/b/c/./../../g/")) (v "a/g");
  if not windows then begin
    eqp (Fpath.normalize (v "//a/b/c/./../../g")) (v "//a/g");
  end;
  if windows then begin
    eqp (Fpath.normalize (v "C:/a/b/c/./../../g")) (v "C:/a/g");
    eqp (Fpath.normalize (v "C:/a/b/c/./../../g")) (v "C:/a/g");
    eqp (Fpath.normalize (v "\\\\?\\UNC\\server\\share\\.."))
           (v "\\\\?\\UNC\\server\\share\\");
  end;
  ()

let rooted = test "Fpath.rooted" @@ fun () ->
  let eq = eq_option ~eq:Fpath.equal ~pp:Fpath.pp in
  eq (Fpath.rooted (v "/a/b") (v "c")) (Some (v "/a/b/c"));
  eq (Fpath.rooted (v "/a/b") (v "/a/b/c")) (Some (v "/a/b/c"));
  eq (Fpath.rooted (v "/a/b") (v "/a/b/c/")) (Some (v "/a/b/c"));
  eq (Fpath.rooted (v "/a/b") (v "/a/b/c/.")) (Some (v "/a/b/c"));
  eq (Fpath.rooted (v "/a/b") (v "../c")) None;
  eq (Fpath.rooted (v "a/b") (v "c")) (Some (v "a/b/c"));
  eq (Fpath.rooted (v "a/b") (v "/c")) None;
  eq (Fpath.rooted (v "a/b") (v "../c")) None;
  eq (Fpath.rooted (v "a/b") (v "c/..")) (Some (v "a/b"));
  eq (Fpath.rooted (v "a/b") (v "c/../..")) None;
  eq (Fpath.rooted (v "a/b") (v "c/d/../..")) (Some (v "a/b"));
  eq (Fpath.rooted (v "../../a") (v "a")) (Some (v "../../a/a"));
  eq (Fpath.rooted (v "../../a") (v "a/..")) (Some (v "../../a"));
  eq (Fpath.rooted (v "../../a") (v "../../b")) None;
  eq (Fpath.rooted (v "../../a") (v "../../a")) (None);
  ()

let relativize = test "Fpath.relativize" @@ fun () ->
  let eq_opt = eq_option ~eq:Fpath.equal ~pp:Fpath.pp in
  let relativize root p result = match Fpath.relativize root p with
  | None -> eq_opt None result
  | Some rp as r ->
      eq_opt r result;
      eqp (Fpath.normalize (Fpath.append root rp)) (Fpath.normalize p);
  in
  relativize (v "/a/b") (v "c") None;
  relativize (v "/a/b") (v "/c") (Some (v "../../c"));
  relativize (v "/a/b") (v "/c/") (Some (v "../../c"));
  relativize (v "/a/b") (v "/a/b/c") (Some (v "c"));
  relativize (v "/a/b") (v "/a/b") (Some (v "."));
  relativize (v "/a/b") (v "/a/b/") (Some (v "."));
  relativize (v "/a/b/c") (v "/d/e/f") (Some (v "../../../d/e/f"));
  relativize (v "/a/b/c") (v "/a/b/d") (Some (v "../d"));
  relativize (v "a/b") (v "/c") None;
  relativize (v "a/b") (v "c") (Some (v "../../c"));
  relativize (v "a/b") (v "c/") (Some (v "../../c"));
  relativize (v "a/b") (v "a/b/c") (Some (v "c"));
  relativize (v "a/b") (v "a/b") (Some (v "."));
  relativize (v "a/b") (v "a/b/") (Some (v "."));
  relativize (v "../a") (v "b") None;
  relativize (v "../../a") (v "../b") None;
  relativize (v "../a") (v "../../b") (Some (v "../../b"));
  relativize (v "a") (v "../../b") (Some (v "../../../b"));
  relativize (v "a/c") (v "../../b") (Some (v "../../../../b"));
  ()

let ext = test "Fpath.ext" @@ fun () ->
  eq_str (Fpath.ext @@ v ".") "";
  eq_str (Fpath.ext @@ v "..") "";
  eq_str (Fpath.ext @@ v "...") "";
  eq_str (Fpath.ext @@ v "....") "";
  eq_str (Fpath.ext @@ v ".....") "";
  eq_str (Fpath.ext @@ v ".a") "";
  eq_str (Fpath.ext @@ v ".a.") ".";
  eq_str (Fpath.ext @@ v ".a..") ".";
  eq_str (Fpath.ext @@ v ".a...") ".";
  eq_str (Fpath.ext @@ v ".a....") ".";
  eq_str (Fpath.ext @@ v "a/...") "";
  eq_str (Fpath.ext @@ v "a/.") "";
  eq_str (Fpath.ext @@ v "a/..") "";
  eq_str (Fpath.ext @@ v "a/.a") "";
  eq_str (Fpath.ext @@ v "a/..b") "";
  eq_str (Fpath.ext @@ v "a/..b.a") ".a";
  eq_str (Fpath.ext @@ v "a/..b..ac") ".ac";
  eq_str (Fpath.ext @@ v "/a/b") "";
  eq_str (Fpath.ext @@ v "/a/b.") ".";
  eq_str (Fpath.ext @@ v "a/.ocamlinit") "";
  eq_str (Fpath.ext @@ v "a/.emacs.d") ".d";
  eq_str (Fpath.ext @@ v "/a/b.mli") ".mli";
  eq_str (Fpath.ext @@ v "a.tar.gz") ".gz";
  eq_str (Fpath.ext @@ v "./a.") ".";
  eq_str (Fpath.ext @@ v "./a..") ".";
  eq_str (Fpath.ext @@ v "./.a.") ".";
  eq_str (Fpath.ext @@ v "./.a..") ".";
  eq_str (Fpath.ext ~multi:true @@ v ".") "";
  eq_str (Fpath.ext ~multi:true @@ v "..") "";
  eq_str (Fpath.ext ~multi:true @@ v "...") "";
  eq_str (Fpath.ext ~multi:true @@ v "....") "";
  eq_str (Fpath.ext ~multi:true @@ v ".....") "";
  eq_str (Fpath.ext ~multi:true @@ v ".a") "";
  eq_str (Fpath.ext ~multi:true @@ v ".a.") ".";
  eq_str (Fpath.ext ~multi:true @@ v ".a..") "..";
  eq_str (Fpath.ext ~multi:true @@ v ".a...") "...";
  eq_str (Fpath.ext ~multi:true @@ v ".a....") "....";
  eq_str (Fpath.ext ~multi:true @@ v "a/...") "";
  eq_str (Fpath.ext ~multi:true @@ v "a/.a") "";
  eq_str (Fpath.ext ~multi:true @@ v "a/..") "";
  eq_str (Fpath.ext ~multi:true @@ v "a/..b") "";
  eq_str (Fpath.ext ~multi:true @@ v "a/..b.a") ".a";
  eq_str (Fpath.ext ~multi:true @@ v "a/..b..ac") "..ac";
  eq_str (Fpath.ext ~multi:true @@ v "a/.emacs.d") ".d";
  eq_str (Fpath.ext ~multi:true @@ v "/a/b.mli") ".mli";
  eq_str (Fpath.ext ~multi:true @@ v "a.tar.gz") ".tar.gz";
  eq_str (Fpath.ext ~multi:true @@ v "./a.") ".";
  eq_str (Fpath.ext ~multi:true @@ v "./a..") "..";
  eq_str (Fpath.ext ~multi:true @@ v "./.a.") ".";
  eq_str (Fpath.ext ~multi:true @@ v "./.a..") "..";
  ()

let has_ext = test "Fpath.has_ext" @@ fun () ->
  eq_bool (Fpath.has_ext "." @@ v ".") false;
  eq_bool (Fpath.has_ext "." @@ v "..") false;
  eq_bool (Fpath.has_ext "." @@ v "...") false;
  eq_bool (Fpath.has_ext "." @@ v "...a") false;
  eq_bool (Fpath.has_ext "." @@ v "...a.") true;
  eq_bool (Fpath.has_ext "." @@ v "...a..") true;
  eq_bool (Fpath.has_ext "" @@ v ".") false;
  eq_bool (Fpath.has_ext "" @@ v "..") false;
  eq_bool (Fpath.has_ext "" @@ v "...") false;
  eq_bool (Fpath.has_ext "" @@ v "...a") false;
  eq_bool (Fpath.has_ext "" @@ v "...a.") true;
  eq_bool (Fpath.has_ext "" @@ v "...a..") true;
  eq_bool (Fpath.has_ext ".." @@ v ".") false;
  eq_bool (Fpath.has_ext ".." @@ v "..") false;
  eq_bool (Fpath.has_ext ".." @@ v "..a.") false;
  eq_bool (Fpath.has_ext ".." @@ v "..a..") true;
  eq_bool (Fpath.has_ext ".." @@ v "...") false;
  eq_bool (Fpath.has_ext ".." @@ v "...a.") false;
  eq_bool (Fpath.has_ext ".." @@ v "...a..") true;
  eq_bool (Fpath.has_ext "..." @@ v "..") false;
  eq_bool (Fpath.has_ext "..." @@ v "...") false;
  eq_bool (Fpath.has_ext "..." @@ v "....") false;
  eq_bool (Fpath.has_ext "..." @@ v ".a...") true;
  eq_bool (Fpath.has_ext ".mli" @@ v "a/b.mli") true;
  eq_bool (Fpath.has_ext "mli" @@ v "a/b.mli") true;
  eq_bool (Fpath.has_ext "mli" @@ v "a/bmli") false;
  eq_bool (Fpath.has_ext "mli" @@ v "a/.mli") false;
  eq_bool (Fpath.has_ext ".tar.gz" @@ v "a/f.tar.gz") true;
  eq_bool (Fpath.has_ext "tar.gz" @@ v "a/f.tar.gz") true;
  eq_bool (Fpath.has_ext "tar.gz" @@ v "a/ftar.gz") false;
  eq_bool (Fpath.has_ext "tar.gz" @@ v "a/tar.gz") false;
  eq_bool (Fpath.has_ext "tar.gz" @@ v "a/.tar.gz") false;
  eq_bool (Fpath.has_ext ".tar" @@ v "a/f.tar.gz") false;
  eq_bool (Fpath.has_ext ".ocamlinit" @@ v ".ocamlinit") false;
  eq_bool (Fpath.has_ext ".ocamlinit" @@ v "..ocamlinit") false;
  eq_bool (Fpath.has_ext "..ocamlinit" @@ v "...ocamlinit") false;
  eq_bool (Fpath.has_ext "..ocamlinit" @@ v ".a..ocamlinit") true;
  eq_bool (Fpath.has_ext "..a" @@ v "..") false;
  ()

let ext_exists = test "Fpath.ext_exists" @@ fun () ->
  eq_bool (Fpath.ext_exists @@ v "a/f") false;
  eq_bool (Fpath.ext_exists @@ v "a/f.") true;
  eq_bool (Fpath.ext_exists @@ v "a/f.gz") true;
  eq_bool (Fpath.ext_exists @@ v "a/f.tar.gz") true;
  eq_bool (Fpath.ext_exists ~multi:true @@ v "a/f") false;
  eq_bool (Fpath.ext_exists ~multi:true @@ v "a/f.") false;
  eq_bool (Fpath.ext_exists ~multi:true @@ v "a/f.gz") false;
  eq_bool (Fpath.ext_exists ~multi:true @@ v "a/f.tar.gz") true;
  eq_bool (Fpath.ext_exists ~multi:true @@ v "a/.a..") true;
  eq_bool (Fpath.ext_exists ~multi:true @@ v "a/.a.") false;
  ()

let add_ext = test "Fpath.add_ext" @@ fun () ->
  app_raises ~pp:Fpath.pp (Fpath.add_ext "/") (v "a/b/c");
  eqp (Fpath.add_ext ".mli" (v "a/b")) (v "a/b.mli");
  eqp (Fpath.add_ext "mli" (v "a/b")) (v "a/b.mli");
  eqp (Fpath.add_ext "" (v "a/b")) (v "a/b");
  eqp (Fpath.add_ext "." (v "a/b")) (v "a/b.");
  eqp (Fpath.add_ext ".tar.gz" (v "a/f")) (v "a/f.tar.gz");
  eqp (Fpath.add_ext "tar.gz" (v "a/f")) (v "a/f.tar.gz");
  eqp (Fpath.add_ext ".gz" (v "a/f.tar")) (v "a/f.tar.gz");
  eqp (Fpath.add_ext "gz" (v "a/f.tar")) (v "a/f.tar.gz");
  eqp (Fpath.(v "a/f.tar" + "gz")) (v "a/f.tar.gz");
  ()

let rem_ext = test "Fpath.rem_ext" @@ fun () ->
  eqp (Fpath.rem_ext @@ v "/a/b") (v "/a/b");
  eqp (Fpath.rem_ext @@ v "/a/b.mli") (v "/a/b");
  eqp (Fpath.rem_ext @@ v "a/.ocamlinit") (v "a/.ocamlinit");
  eqp (Fpath.rem_ext @@ v "f.tar.gz") (v "f.tar");
  eqp (Fpath.rem_ext ~multi:true @@ v "f.tar.gz") (v "f");
  ()

let set_ext = test "Fpath.set_ext" @@ fun () ->
  app_raises ~pp:Fpath.pp (Fpath.set_ext "/") (v "a/b/c");
  eqp (Fpath.set_ext ".bla" (v "/a/b")) (v "/a/b.bla");
  eqp (Fpath.set_ext "bla" (v "/a/b")) (v "/a/b.bla");
  eqp (Fpath.set_ext ".bla" (v "/a/b.mli")) (v "/a/b.bla");
  eqp (Fpath.set_ext "bla" (v "/a/b.mli")) (v "/a/b.bla");
  eqp (Fpath.set_ext "bla" (v "a/.ocamlinit")) (v "a/.ocamlinit.bla");
  eqp (Fpath.set_ext "bla" (v "f.tar.gz")) (v "f.tar.bla");
  eqp (Fpath.set_ext ~multi:true "bla" (v "f.tar.gz")) (v "f.bla");
  eqp (Fpath.set_ext ~multi:true "" (v "f.tar.gz")) (v "f.");
  ()

let split_ext = test "Fpath.split_ext" @@ fun () ->
  let eq_split ?multi p q ext =
    let p = v p in
    let q', ext' = Fpath.split_ext ?multi p in
    eq_str ext ext';
    eqp (v q) q';
    eqp (v (Fpath.to_string q' ^ ext')) p
  in
  eq_split "/a/b" "/a/b" "";
  eq_split "/a/b.mli" "/a/b" ".mli";
  eq_split "a/.ocamlinit" "a/.ocamlinit" "";
  eq_split "f.tar.gz" "f.tar" ".gz";
  eq_split ~multi:true "f.tar.gz" "f" ".tar.gz";
  ()


let suite = suite "Fpath module"
    [ of_string;
      add_seg;
      append;
      constants;
      is_seg_valid;
      is_abs_rel;
      is_dotfile;
      is_root;
      is_prefix;
      split_volume;
      segs;
      name;
      filename;
      base;
      parent;
      file_to_dir;
      dir_to_file;
      find_prefix;
      rem_prefix;
      normalize;
      rooted;
      relativize;
      ext;
      has_ext;
      ext_exists;
      add_ext;
      rem_ext;
      set_ext;
      split_ext; ]

(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli.
   All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions
   are met:

   1. Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

   2. Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

   3. Neither the name of Daniel C. Bünzli nor the names of
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
  ---------------------------------------------------------------------------*)
