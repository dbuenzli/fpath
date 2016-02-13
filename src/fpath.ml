(*---------------------------------------------------------------------------
   Copyright 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open Astring

(* Unsafe string and byte manipulations. If you don't believe the
   author's invariants, replacing with safe versions makes everything
   safe in the library. He won't be upset. *)

let bytes_unsafe_set = Bytes.unsafe_set
let string_unsafe_get = String.unsafe_get

(* Errors *)

let err_invalid_path s = strf "invalid path: %a" String.dump s
let err_invalid_seg s = strf "invalid segment: %a" String.dump s
let err_invalid_ext s = strf "invalid extension: %a" String.dump s

(* Preliminaries *)

let windows = Sys.os_type = "Win32"
let dir_sep_char = if windows then '\\' else '/'
let dir_sep = String.of_char dir_sep_char
let dir_sep_sub = String.sub dir_sep

let dot = "."
let dot_sub = String.sub dot
let dot_dir = dot ^ dir_sep
let dot_dir_sub = String.sub dot_dir
let dotdot = ".."
let dotdot_sub = String.sub dotdot
let dotdot_dir = dotdot ^ dir_sep
let dotdot_dir_sub = String.sub dotdot_dir

let validate_and_collapse_seps p =
  (* collapse non-initial sequences of [dir_sep] to a single one and checks
     no null byte *)
  let max_idx = String.length p - 1 in
  let rec with_buf b last_sep k i = (* k is the write index in b *)
    if i > max_idx then Some (Bytes.sub_string b 0 k) else
    let c = string_unsafe_get p i in
    if c = '\x00' then None else
    if c <> dir_sep_char
    then (bytes_unsafe_set b k c; with_buf b false (k + 1) (i + 1)) else
    if not last_sep
    then (bytes_unsafe_set b k c; with_buf b true (k + 1) (i + 1)) else
    with_buf b true k (i + 1)
  in
  let rec try_no_alloc last_sep i =
    if i > max_idx then Some p else
    let c = string_unsafe_get p i in
    if c = '\x00' then None else
    if c <> dir_sep_char then try_no_alloc false (i + 1) else
    if not last_sep then try_no_alloc true (i + 1) else
    let b = Bytes.of_string p in (* copy and overwrite starting from i *)
    with_buf b true i (i + 1)
  in
  let start = (* Allow initial double sep *)
    if max_idx > 0 then (if p.[0] = dir_sep_char then 1 else 0) else 0
  in
  try_no_alloc false start

let is_unc_path_windows p = String.is_prefix "\\\\" p
let windows_non_unc_path_start_index p =
  match String.find (Char.equal ':') p with
  | None -> 0
  | Some i -> i + 1 (* exists by construction *)

let parse_unc_windows s =
  (* parses an UNC path, the \\ prefix was already parsed, adds a root path
     if there's only a volume, UNC paths are always absolute. *)
  let p = String.sub ~start:2 s in
  let not_bslash c = c <> '\\' in
  let parse_seg p = String.Sub.span ~min:1 ~sat:not_bslash p in
  let ensure_root r = Some (if String.Sub.is_empty r then (s ^ "\\") else s) in
  match parse_seg p with
  | (seg1, _) when String.Sub.is_empty seg1 -> None (* \\ or \\\ *)
  | (seg1, rest) ->
      let seg1_len = String.Sub.length seg1 in
      match String.Sub.get_head ~rev:true seg1 with
      | '.' when seg1_len = 1 -> (* \\.\device\ *)
          begin match parse_seg (String.Sub.tail rest) with
          | (seg, _) when String.Sub.is_empty seg -> None
          | (_, rest) -> ensure_root rest
          end
      | '?' when seg1_len = 1 ->
          begin match parse_seg (String.Sub.tail rest) with
          | (seg2, _) when String.Sub.is_empty seg2 -> None
          | (seg2, rest) ->
              if (String.Sub.get_head ~rev:true seg2 = ':') (* \\?\drive:\ *)
              then (ensure_root rest) else
              if not (String.Sub.equal_bytes seg2 (String.sub "UNC"))
              then begin (* \\?\server\share\ *)
                match parse_seg (String.Sub.tail rest) with
                | (seg, _) when String.Sub.is_empty seg -> None
                | (_, rest) -> ensure_root rest
              end else begin (* \\?\UNC\server\share\ *)
                match parse_seg (String.Sub.tail rest) with
                | (seg, _) when String.Sub.is_empty seg -> None
                | (_, rest) ->
                    match parse_seg (String.Sub.tail rest) with
                    | (seg, _) when String.Sub.is_empty seg -> None
                    | (_, rest) -> ensure_root rest
              end
          end
      | _ -> (* \\server\share\ *)
          begin match parse_seg (String.Sub.tail rest) with
          | (seg, _) when String.Sub.is_empty seg -> None
          | (_, rest) -> ensure_root rest
          end

let sub_split_volume_windows p =
  (* splits a windows path into its volume (or drive) and actual file
     path. When called the path in [p] is guaranteed to be non empty
     and if [p] is an UNC path it is guaranteed to the be parseable by
     parse_unc_windows. *)
  let split_before i = String.sub p ~stop:i, String.sub p ~start:i in
  if not (is_unc_path_windows p) then
    begin match String.find (Char.equal ':') p with
    | None -> String.Sub.empty, String.sub p
    | Some i -> split_before (i + 1)
    end
  else
  let bslash ~start = match String.find ~start (Char.equal '\\') p with
  | None -> assert false | Some i -> i
  in
  let i = bslash ~start:2 in
  let j = bslash ~start:(i + 1) in
  match p.[i-1] with
  | '.' when i = 3 -> split_before j
  | '?' when i = 3 ->
      if p.[j-1] = ':' then split_before j else
      if (String.Sub.equal_bytes
            (String.sub p ~start:(i + 1) ~stop:j)
            (String.sub "UNC"))
      then split_before (bslash ~start:((bslash ~start:(j + 1)) + 1))
      else split_before (bslash ~start:(j + 1))
  | _ -> split_before j

let is_root_posix p = String.equal p dir_sep || String.equal p "//"
let is_root_windows p =
  let _, p = sub_split_volume_windows p in
  String.Sub.equal_bytes dir_sep_sub p

let sub_last_seg_windows p =
  let _, path = sub_split_volume_windows p in
  match String.Sub.find ~rev:true (Char.equal dir_sep_char) path with
  | None -> path
  | Some c -> String.Sub.(extend (stop c))

let sub_last_seg_posix p =
  let p = String.sub p in
  match String.Sub.find ~rev:true (Char.equal dir_sep_char) p with
  | None -> p
  | Some c -> String.Sub.(extend (stop c))

let sub_last_seg = if windows then sub_last_seg_windows else sub_last_seg_posix

let of_string_windows p =
  if p = "" then None else
  let p = String.map (fun c -> if c = '/' then '\\' else c) p in
  match validate_and_collapse_seps p with
  | None -> None
  | Some p as some ->
      if is_unc_path_windows p then parse_unc_windows p else
      match String.find (Char.equal ':') p with
      | None -> some
      | Some i -> if i = String.length p - 1 then None else (Some p)

let of_string_posix p = if p = "" then None else validate_and_collapse_seps p

let of_string = if windows then of_string_windows else of_string_posix

let to_string p = p
let pp ppf p = Format.pp_print_string ppf (to_string p)
let dump ppf p = String.dump ppf (to_string p)

let is_seg_windows s =
  let valid c = c <> '\x00' && c <> dir_sep_char && c <> '/' in
  String.for_all valid s

let is_seg_posix s =
  let valid c = c <> '\x00' && c <> dir_sep_char in
  String.for_all valid s

let is_seg = if windows then is_seg_windows else is_seg_posix

(* File paths *)

type t = string (* N.B. a path is never "" or something is wrooong. *)

let v s = match of_string s with
| None -> invalid_arg (err_invalid_path s)
| Some p -> p

let add_seg p seg =
  if not (is_seg seg) then invalid_arg (err_invalid_seg seg);
  let sep = if p.[String.length p - 1] = dir_sep_char then "" else dir_sep in
  String.concat [p; sep; seg]

let append_posix p0 p1 =
  if p1.[0] = dir_sep_char (* absolute *) then p1 else
  let sep = if p0.[String.length p0 - 1] = dir_sep_char then "" else dir_sep in
  String.concat [p0; sep; p1]

let append_windows p0 p1 =
  if is_unc_path_windows p1 then p1 else
  match String.find (Char.equal ':') p1 with
  | Some _ (* drive *) -> p1
  | None ->
      if p1.[0] = dir_sep_char then (* absolute *) p1 else
      let sep =
        if p0.[String.length p0 - 1] = dir_sep_char then "" else dir_sep
      in
      String.concat [p0; sep; p1]

let append = if windows then append_windows else append_posix

let ( / ) = add_seg
let ( // ) = append

let split_volume_windows p =
  let vol, path = sub_split_volume_windows p in
  String.Sub.to_string vol, String.Sub.to_string path

let split_volume_posix p =
  if String.is_prefix "//" p then dir_sep, String.with_range ~first:1 p else
  "", p

let split_volume = if windows then split_volume_windows else split_volume_posix

let segs_windows p =
  let _, path = sub_split_volume_windows p in
  let path = String.Sub.to_string path in
  String.cuts ~sep:dir_sep path

let segs_posix p =
  let segs = String.cuts ~sep:dir_sep p in
  if String.is_prefix "//" p then List.tl segs else segs

let segs = if windows then segs_windows else segs_posix

(* File and directory paths *)

let is_dir_path p =
  let seg = sub_last_seg p in
  String.Sub.is_empty seg ||
  String.Sub.equal_bytes seg dot_sub ||
  String.Sub.equal_bytes seg dotdot_sub

let is_file_path p = not (is_dir_path p)

let to_dir_path p = add_seg p ""

let filename p = match String.Sub.to_string (sub_last_seg p) with
| "" | "." | ".." -> ""
| filename -> filename

(* Base and parent paths *)

let not_dir_sep c = c <> dir_sep_char
let split_last_seg p = String.Sub.span ~rev:true ~sat:not_dir_sep p
let drop_last_seg p = String.Sub.drop ~rev:true ~sat:not_dir_sep p
let split_last_non_empty_seg p =
  let (dir, last_seg as r) = split_last_seg p in
  match String.Sub.is_empty last_seg with
  | false -> r, true
  | true -> split_last_seg (String.Sub.tail ~rev:true dir), false

let seg_is_rel = function "." | ".." -> true | _ -> false
let seg_is_rel_sub seg =
  String.Sub.(equal_bytes dot_sub seg || equal_bytes dotdot_sub seg)

let split_base_windows p =
  let vol, path = sub_split_volume_windows p in
  if String.Sub.equal_bytes dir_sep_sub path then (* root *) p, dot_dir else
  let dir, last_seg = split_last_seg path in
  match String.Sub.is_empty dir with
  | true -> (* single seg *)
      String.Sub.base_string (String.Sub.append vol dot_dir_sub),
      String.Sub.to_string path
  | false ->
      match String.Sub.is_empty last_seg with
      | false ->
          String.Sub.base_string (String.Sub.append vol dir),
          String.Sub.to_string last_seg
      | true ->
          let dir_file = String.Sub.tail ~rev:true dir in
          let dir, dir_last_seg = split_last_seg dir_file in
          match String.Sub.is_empty dir with
          | true ->
              String.Sub.base_string (String.Sub.append vol dot_dir_sub),
              String.Sub.to_string path
          | false ->
              String.Sub.base_string (String.Sub.append vol dir),
              String.Sub.to_string (String.Sub.extend dir_last_seg)

let split_base_posix p =
  if is_root_posix p then p, dot_dir else
  let dir, last_seg = split_last_seg (String.sub p) in
  match String.Sub.is_empty dir with
  | true -> (* single seg *) dot_dir, p
  | false ->
      match String.Sub.is_empty last_seg with
      | false -> String.Sub.to_string dir, String.Sub.to_string last_seg
      | true ->
          let dir_file = String.Sub.tail ~rev:true dir in
          let dir, dir_last_seg = split_last_seg dir_file in
          match String.Sub.is_empty dir with
          | true -> dot_dir, p
          | false ->
              String.Sub.to_string dir,
              String.Sub.to_string (String.Sub.extend dir_last_seg)

let split_base = if windows then split_base_windows else split_base_posix

let base p = snd (split_base p)

let basename_windows p =
  let vol, path = sub_split_volume_windows p in
  if String.Sub.equal_bytes dir_sep_sub path then (* root *) "" else
  let basename =
    let dir, last_seg = split_last_seg path in
    match String.Sub.is_empty dir with
    | true -> (* single seg *) String.Sub.to_string path
    | false ->
        match String.Sub.is_empty last_seg with
        | false -> String.Sub.to_string last_seg
        | true ->
            let dir_file = String.Sub.tail ~rev:true dir in
            let _, dir_last_seg = split_last_seg dir_file in
            String.Sub.to_string dir_last_seg
  in
  match basename with "." | ".." -> "" | basename -> basename

let basename_posix p =
  if p = dir_sep || p = "//" then (* root *) "" else
  let basename =
    let dir, last_seg = split_last_seg (String.sub p) in
    match String.Sub.is_empty dir with
    | true -> (* single seg *) p
    | false ->
        match String.Sub.is_empty last_seg with
        | false -> String.Sub.to_string last_seg
        | true ->
            let dir_file = String.Sub.tail ~rev:true dir in
            let _, dir_last_seg = split_last_seg dir_file in
            String.Sub.to_string dir_last_seg
  in
  match basename with "." | ".." -> "" | basename -> basename

let basename p = if windows then basename_windows p else basename_posix p

(* The parent algorithm is not very smart. It tries not to change the
   original path too much or deal with normalization.  We simply
   remove everyting after the last non-empty, non-relative, path segment
   and if the resulting path is empty we return "./". If the last non-empty
   segment is "." or ".." we then simply postfix "../" *)

let _parent p =
  let (dir, seg), is_last = split_last_non_empty_seg p in
  let dsep = if is_last then dir_sep_sub else String.Sub.empty in
  match String.Sub.is_empty dir with
  | true ->
      if seg_is_rel_sub seg then [p; dsep; dotdot_dir_sub] else [dot_dir_sub]
  | false ->
      if seg_is_rel_sub seg then [p; dsep; dotdot_dir_sub] else [dir]

let parent_windows p =
  let vol, path = sub_split_volume_windows p in
  if String.Sub.equal_bytes dir_sep_sub path then (* root *) p else
  String.Sub.(base_string @@ concat (vol :: _parent path))

let parent_posix p =
  if is_root_posix p then p else
  String.Sub.(base_string @@ concat (_parent (String.sub p)))

let parent = if windows then parent_windows else parent_posix

(* Normalization *)

let rem_empty_seg_windows p =
  let vol, path = sub_split_volume_windows p in
  if String.Sub.equal_bytes dir_sep_sub path then (* root *) p else
  let dir, last_seg = split_last_seg path in
  if not (String.Sub.is_empty last_seg) then p else
  let p = String.Sub.tail ~rev:true dir in
  String.Sub.(base_string @@ concat [vol; p])

let rem_empty_seg_posix p = match String.length p with
| 1 -> p
| 2 ->
    if p.[0] <> dir_sep_char && p.[1] = dir_sep_char
    then String.of_char p.[0]
    else p
| len ->
    let max = len - 1 in
    if p.[max] <> dir_sep_char then p else
    String.with_index_range p ~last:(max - 1)

let rem_empty_seg =
  if windows then rem_empty_seg_windows else rem_empty_seg_posix

let normalize_rel_segs_rev segs =
  let rec loop acc = function
  | "." :: [] -> ("" :: acc) (* final "." remove but preserve directoryness. *)
  | "." :: rest -> loop acc rest
  | ".." :: rest ->
      begin match acc with
      | ".." :: _ | [] -> loop (".." :: acc) rest
      | seg :: acc -> (* N.B. seg can't be "." *)
          match rest with
          | [] -> ("" :: acc) (* preserve directoryness *)
          | rest -> loop acc rest
      end
  | seg :: rest -> loop (seg :: acc) rest
  | [] ->
      match acc with
      | ".." :: _ -> ("" :: acc) (* normalize final .. to ../ *)
      | acc -> acc
  in
  loop [] segs

let normalize_segs = function
| "" :: segs -> (* absolute path *)
    let rec rem_dotdots = function
    | ".." :: segs -> rem_dotdots segs
    | [] -> [""]
    | segs -> segs
    in
    "" :: (rem_dotdots (List.rev (normalize_rel_segs_rev segs)))
| segs ->
    match List.rev (normalize_rel_segs_rev segs) with
    | [] | [""] -> ["."; ""]
    | segs -> segs

let normalize_windows p =
  let vol, path = sub_split_volume_windows p in
  let path = String.Sub.to_string path in
  let segs = normalize_segs (String.cuts ~sep:dir_sep path) in
  let path = String.concat ~sep:dir_sep segs in
  String.Sub.(to_string (concat [vol; String.sub path]))

let normalize_posix p =
  let segs = String.cuts ~sep:dir_sep p in
  let has_volume = String.is_prefix "//" p in
  let segs = normalize_segs (if has_volume then List.tl segs else segs) in
  let segs = if has_volume then "" :: segs else segs in
  String.concat ~sep:dir_sep segs

let normalize = if windows then normalize_windows else normalize_posix

(* Prefixes *)

let is_prefix prefix p =
  if not (String.is_prefix prefix p) then false else
  (* Further check the prefix is segment-based. If [prefix] ends with a
     dir_sep_char nothing more needs to be checked. If it doesn't we need
     to check that [p]'s remaining suffix is either empty or
     starts with a directory separator. *)
  let suff_start = String.length prefix in
  if prefix.[suff_start - 1] = dir_sep_char then true else
  if suff_start = String.length p then true else
  p.[suff_start] = dir_sep_char

let seg_prefix_last_index p0 p1 = (* Warning doesn't care about volumes *)
  let l0 = String.length p0 in
  let l1 = String.length p1 in
  let p0, p1, max = if l0 < l1 then p0, p1, l0 - 1 else p1, p0, l1 - 1 in
  let rec loop last_dir_sep i p0 p1 = match i > max || p0.[i] <> p1.[i] with
  | false ->
      let last_dir_sep = if p0.[i] = dir_sep_char then i else last_dir_sep in
      loop last_dir_sep (i + 1) p0 p1
  | true ->
      if i = 0 then None else
      let last = i - 1 in
      if last_dir_sep = last then Some last else
      match last = max with
      | true ->
          if l1 = l0 then Some last else
          if p1.[i] = dir_sep_char then Some last else
          if last_dir_sep <> -1 then Some last_dir_sep else None
      | false ->
          if last_dir_sep <> -1 then Some last_dir_sep else None
  in
  loop (-1) 0 p0 p1

let find_prefix_windows p0 p1 = match seg_prefix_last_index p0 p1 with
| None -> None
| Some i ->
    let v0_len = String.Sub.length (fst (sub_split_volume_windows p0)) in
    let v1_len = String.Sub.length (fst (sub_split_volume_windows p1)) in
    let vmax = if v0_len > v1_len then v0_len else v1_len in
    if i < vmax then None else
    Some (String.with_index_range p0 ~last:i)


let find_prefix_posix p0 p1 = match seg_prefix_last_index p0 p1 with
| None -> None
| Some 0 when String.is_prefix "//" p0 || String.is_prefix "//" p1 -> None
| Some i -> Some (String.with_index_range p0 ~last:i)

let find_prefix = if windows then find_prefix_windows else find_prefix_posix

let rem_prefix prefix p = match is_prefix prefix p with
| false -> None
| true ->
    match String.length prefix with
    | len when len = String.length p -> None
    | len ->
        let first = if p.[len] = dir_sep_char then len + 1 else len in
        match String.with_index_range p ~first with
        | "" -> Some dot_dir
        | q -> Some q

(* Roots and relativization *)

let rooted ~root p =
  let nroot = normalize root in
  let prooted = normalize (append root p) in
  if is_prefix nroot prooted then Some prooted else None

let relativize ~root p =
  let root = normalize root in
  let p = normalize p in
  match find_prefix root p with
  | None -> None
  | Some pre ->
      let rem_prefix p = match rem_prefix pre p with
      | None -> p | Some p -> p
      in
      match segs (rem_prefix root) with
      | ".." :: _ -> None
      | "." :: [] -> Some (rem_prefix p)
      | root ->
          let p = rem_prefix p in
          let rel = List.fold_left (fun acc _ -> dotdot :: acc) [p] root in
          (Some (String.concat ~sep:dir_sep rel))

(* Predicates and comparison *)

let is_rel_posix p = p.[0] <> dir_sep_char
let is_rel_windows p =
  if is_unc_path_windows p then false else
  p.[windows_non_unc_path_start_index p] <> dir_sep_char

let is_rel = if windows then is_rel_windows else is_rel_posix
let is_abs p = not (is_rel p)

let is_root = if windows then is_root_windows else is_root_posix

let is_current_dir_posix p = String.equal p dot || String.equal p dot_dir
let is_current_dir_windows p =
  if is_unc_path_windows p then false else
  let start = windows_non_unc_path_start_index p in
  match String.length p - start with
  | 1 -> p.[start] = '.'
  | 2 -> p.[start] = '.' && p.[start + 1] = dir_sep_char
  | _ -> false

let is_current_dir =
  if windows then is_current_dir_windows else is_current_dir_posix

let is_dotfile p = match basename p with
| "" -> false
| s -> s.[0] = '.'

let equal = String.equal
let compare = String.compare

(* File extensions *)

type ext = string

let ext_sep_char = '.'
let ext_sep = String.of_char ext_sep_char
let ext_sep_sub = String.Sub.of_char ext_sep_char
let eq_ext_sep c = c = ext_sep_char
let neq_ext_sep c = c <> ext_sep_char

let rec multi_ext_sub seg =
  let seg = String.Sub.drop ~sat:eq_ext_sep seg in
  String.Sub.drop ~sat:neq_ext_sep seg

let single_ext_sub seg =
  let seg = String.Sub.drop ~sat:eq_ext_sep seg in
  let name_dot, ext = String.Sub.span ~rev:true ~sat:neq_ext_sep seg in
  if String.Sub.length name_dot = 0 then String.Sub.empty else
  String.Sub.extend ~max:1 ~rev:true ext

let ext_sub ?(multi = false) seg =
  if multi then multi_ext_sub seg else single_ext_sub seg

let filename_sub_windows p =
  let _, path = sub_split_volume_windows p in
  match String.Sub.find ~rev:true (Char.equal dir_sep_char) path with
  | None -> path
  | Some c -> String.Sub.(extend (stop c))

let filename_sub_posix p =
  match String.find ~rev:true (Char.equal dir_sep_char) p with
  | None -> String.sub p
  | Some i -> String.sub p ~start:(i + 1)

let filename_sub = if windows then filename_sub_windows else filename_sub_posix

let get_ext ?multi p = String.Sub.to_string (ext_sub ?multi (filename_sub p))

let has_ext e p =
  let seg = String.Sub.drop ~sat:eq_ext_sep (filename_sub p) in
  if not (String.Sub.is_suffix (String.sub e) seg) then false else
  if not (String.is_empty e) && e.[0] = ext_sep_char then true else
  (* check there's a dot before the suffix in [seg] *)
  let dot_index = String.Sub.length seg - String.length e - 1 in
  if dot_index <= 0 then false else
  String.Sub.get seg dot_index = ext_sep_char

let ext_exists ?(multi = false) p =
  let ext = ext_sub ~multi (filename_sub p) in
  if not multi then not (String.Sub.is_empty ext) else
  if String.Sub.is_empty ext then false else
  match String.Sub.find ~rev:true eq_ext_sep ext with (* find another dot *)
  | Some c -> not (String.Sub.start_pos ext = String.Sub.start_pos c)
  | None -> assert false

let add_ext e p =
  if not (is_seg e) then invalid_arg (err_invalid_ext e) else
  let maybe_dot =
    if String.is_empty e then "" else
    if e.[0] <> ext_sep_char then ext_sep else ""
  in
  String.concat [p; maybe_dot; e]

let rem_ext ?multi p =
  let ext = ext_sub ?multi (filename_sub p) in
  if String.Sub.is_empty ext then p else
  String.with_index_range p ~last:(String.Sub.start_pos ext - 1)

let set_ext ?multi e p =
  if not (is_seg e) then invalid_arg (err_invalid_ext e) else
  let ext = ext_sub ?multi (filename_sub p) in
  let p =
    if String.Sub.is_empty ext then (String.sub p) else
    String.Sub.extend ~rev:true (String.Sub.start ext)
  in
  let maybe_dot =
    if String.is_empty e || e.[0] <> ext_sep_char then ext_sep_sub else
    String.Sub.empty
  in
  String.Sub.(to_string (concat [p; maybe_dot; String.sub e]))

let split_ext ?multi p =
  let ext = ext_sub ?multi (filename_sub p) in
  if String.Sub.is_empty ext then (p, "") else
  (String.with_index_range p ~last:(String.Sub.start_pos ext - 1),
   String.Sub.to_string ext)

let ( + ) p e = add_ext e p
let ( -+ ) p e = set_ext e p

(* Path sets and maps *)

type path = t

module Set = struct
  include Set.Make (String)

  let pp ?(sep = Format.pp_print_cut) pp_elt ppf ps =
    let pp_elt elt is_first =
      if is_first then () else Format.fprintf ppf "@ ";
      Format.fprintf ppf "%a" pp_elt elt; false
    in
    ignore (fold pp_elt ps true)

  let dump_path = dump
  let dump ppf ss =
    let pp_elt elt is_first =
      if is_first then () else Format.fprintf ppf "@ ";
      Format.fprintf ppf "%a" dump_path elt;
      false
    in
    Format.fprintf ppf "@[<1>{";
    ignore (fold pp_elt ss true);
    Format.fprintf ppf "}@]";
    ()

  let err_empty () = invalid_arg "empty set"
  let err_absent p ps =
    invalid_arg (strf "%a not in set %a" dump_path p dump ps)

  let get_min_elt ps = try min_elt ps with Not_found -> err_empty ()
  let min_elt ps = try Some (min_elt ps) with Not_found -> None

  let get_max_elt ps = try max_elt ps with Not_found -> err_empty ()
  let max_elt ps = try Some (max_elt ps) with Not_found -> None

  let get_any_elt ps = try choose ps with Not_found -> err_empty ()
  let choose ps = try Some (choose ps) with Not_found -> None

  let get p ps = try find p ps with Not_found -> err_absent p ps
  let find p ps = try Some (find p ps) with Not_found -> None

  let of_list = List.fold_left (fun acc s -> add s acc) empty
end

module Map = struct
  include Map.Make (String)

  let err_empty () = invalid_arg "empty map"
  let err_absent s = invalid_arg (strf "%s is not bound in map" s)

  let get_min_binding m = try min_binding m with Not_found -> err_empty ()
  let min_binding m = try Some (min_binding m) with Not_found -> None

  let get_max_binding m = try max_binding m with Not_found -> err_empty ()
  let max_binding m = try Some (max_binding m) with Not_found -> None

  let get_any_binding m = try choose m with Not_found -> err_empty ()
  let choose m = try Some (choose m) with Not_found -> None

  let get k s = try find k s with Not_found -> err_absent k
  let find k m = try Some (find k m) with Not_found -> None

  let dom m = fold (fun k _ acc -> Set.add k acc) m Set.empty

  let of_list bs = List.fold_left (fun m (k,v) -> add k v m) empty bs

  let pp ?sep:(pp_sep = Format.pp_print_cut) pp_binding ppf (m : 'a t) =
    let pp_binding k v is_first =
      if is_first then () else pp_sep ppf ();
      pp_binding ppf (k, v); false
    in
    ignore (fold pp_binding m true)

  let dump pp_v ppf m =
    let pp_binding k v is_first =
      if is_first then () else Format.fprintf ppf "@ ";
      Format.fprintf ppf "@[<1>(@[%a@],@ @[%a@])@]" dump k pp_v v;
      false
    in
    Format.fprintf ppf "@[<1>{";
    ignore (fold pp_binding m true);
    Format.fprintf ppf "}@]";
    ()
end

type set = Set.t
type 'a map = 'a Map.t

(*---------------------------------------------------------------------------
   Copyright 2015 Daniel C. Bünzli.
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
