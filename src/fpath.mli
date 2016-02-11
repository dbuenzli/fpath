(*---------------------------------------------------------------------------
   Copyright 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(** File system paths, file {{!file_exts}extensions}, path {{!Set}sets}
    and {{!Map}maps}.

    A (file system) {e path} specifies a file or a directory in a file
    system hierarchy. A path has three parts:
    {ol
    {- An optional, platform-dependent, {{!split_volume}volume}.}
    {- An optional root directory separator {!dir_sep} whose presence
        distinguishes {e absolute} paths (["/a"]) from {e relative}
        ones (["a"])}
    {- A list of {!dir_sep} separated segments. Segments are
       non empty strings except for maybe the last one. The latter
       distinguishes {e directory paths}
       (["a/b/"]) from {e file paths} (["a/b"]).}}

    [Fpath] operates on paths without accessing the operating
    system.

    Consult a few {{!tips}important tips}.

    {e v%%VERSION%% - {{:%%PKG_WWW%% }homepage}} *)

(** {1:paths Paths} *)

val dir_sep : string
(** [dir_sep] is the platform dependent directory separator.  This is
    ["/"] on POSIX and ["\\"] on Windows. *)

val is_seg_valid : string -> bool
(** [is_seg_valid s] is [true] iff [s] does not contain {!dir_sep} or a
    [0x00] byte. *)

type t
(** The type for paths. *)

val v : string -> t
(** [v s] is the string [s] as path, see {!of_string} for details.

    @raise Invalid_argument if {!of_string}[ p] is [None]. Use {!of_string}
    to deal with possibly invalid paths (e.g. on user input). *)

val add_seg : t -> string -> t
(** [add_seg p seg] adds [seg] at the end of [p]. If [seg] is [""]
    it is only added if [p] has no final empty segment. {{!ex_add_seg}Examples}.

    @raise Invalid_argument if {!is_seg_valid}[ seg] is [false]. *)

val ( / ) : t -> string -> t
(** [p / seg] is {!add_seg}[ p seg]. Left associative. *)

val append : t -> t -> t
(** [append p p'] appends [p'] to [p] as follows:
    {ul
    {- If [p'] is absolute or has a non-empty {{!split_volume}volume} then
       [p'] is returned.}
    {- Otherwise appends [p'] to [p] using a {!dir_sep} if needed.}}
    {{!ex_append}Examples}. *)

val ( // ) : t -> t -> t
(** [p // p'] is {!append}[ p p']. Left associative. *)

val split_volume : t -> string * t
(** [split_volume p] is the pair [(vol, q)] where [vol] is
    the platform dependent volume of [p] or the empty string
    if there is none and [q] the path [p] without its volume, that is
    its optional root {!dir_sep} and segments.

    On POSIX if [vol] is non-empty then it
    {{:http://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap03.html#tag_03_267}can} only be ["/"] (e.g. in [v "//a/b"]). On Windows [vol] may be
    one of the following prefixes parsed before an
    absolute root {!dir_sep}, except in the first case
    where a relative path can follow:
{[
$(drive):
\\$(server)\$(share)
\\?\$(drive):
\\?\$(server)\$(share)
\\?\UNC\$(server)\$(share)
\\.\$(device)
]}
    The following invariant holds:
    {ul
    {- [equal (v (vol ^ (to_string q))) p]}}
    {b Warning.} [equal (append (v vol) q) p] does not hold. *)

val segs : t -> string list
(** [segs p] is [p]'s {e non-empty} list of segments. Absolute paths have an
    initial empty string added, this allows to recover the path with
    {!String.concat}[ ~sep:dir_sep]. {{!ex_segs}Examples.}

    The following invariant holds:
    {ul
    {- [to_string (snd (split_volume p)) = (String.concat ~sep:dir_sep
       (segs p))]}} *)

(** {1:filedir File and directory paths} *)

val filename : t -> string
(** [filename p] is the file name of [p]. This is the last segment of
    [p]. If [p] is a directory path, this is the empty string. See
    also {!name}. {{!ex_filename}Examples}. *)

val file_to_dir : t -> t
(** [file_to_dir p] is {!add_seg}[ p ""]. It ensures the result has a
    trailing {!dir_sep}. {{!ex_file_to_dir}Examples}. *)

val dir_to_file : t -> t
(** [dir_to_file p] removes the last segment of [p] if it is empty.
    It ensures the result has no trailing {!dir_sep}.
    {{!ex_dir_to_file}Examples}. *)

(** {1:parentbase Base and parent paths} *)

val name : t -> string
(** [name p] is the name of [p]. This is the last {e non-empty} segment of
    [p] or the empty string if [p] is {!root}. See also {!filename} and
    {!base}. {{!ex_name}Examples}. *)

val base : t -> t
(** [base p] is the path made of the last {e non-empty} segment of
    [p] or [p] itself on root paths. See also {!name}. {{!ex_base}Examples}. *)

val parent : t -> t
(** [parent p] is the parent path of [p]. This is defined as [p] without
    its last {e non-empty} segment or [p] if there is no such segment
    or [.] for a single segment relative path.
    {{!ex_parent}Examples}. *)

(** {1:prefix Normalization and prefixes} *)

val is_prefix : root:t -> t -> bool
(** [is_prefix ~root p] is [true] if [root] is a prefix of [p].  This
    checks that [root] has the same optional volume as [p], the same
    optional root directory separator and that the list of segments
    of [root] is a prefix of the segments of [p]. {{!ex_is_prefix}Examples}. *)

val find_prefix : t -> t -> t option
(** [find_prefix p p'] is [Some root] if there exists [root] such that
    [root] is the longest path with
    [is_prefix root p && is_prefix root p' = true] and [None] otherwise.
    Note that if both [p] and [p'] are relative or absolute
    and have the same volume then a prefix exists.
    {{!ex_find_prefix}Examples}. *)

val rem_prefix : root:t -> t -> t option
(** [rem_prefix root p] is [Some q] if [root] is a
    {{!is_prefix}prefix} of [p]; [q] is [p] without the [root]
    prefix but interpreted as a {{!file_to_dir}directory} (hence [q]
    is always relative). {{!ex_rem_prefix}Examples}.

    {b Note.} If you {{!find_prefix}find} a prefix and this
    prefix is ["."], [rem_prefix] may return [None] on that
    prefix and the path where you found it. *)

val normalize : t -> t
(** [normalize p] normalizes [p] to a path referring to the same
    {{!dir_to_file}file} without consulting the filesystem. If [p]
    is absolute the resulting path has no [.] and [..]
    segments. If [p] is relative it has no [.] and may only
    have potential [..] as initial segments. Note that except
    if the path is a root, the path never has a trailing directory
    separator. {{!ex_normalize}Examples}. *)

val rooted : root:t -> t -> t option
(** [rooted ~root p] is:
    {ul
    {- [None] if
       [is_prefix (normalize root) (normalize @@ append root p) = false].}
    {- [Some (normalize @@ append root p)] otherwise.}}
       In other words it ensures that an absolute path [p] or a relative
       path [p] expressed w.r.t. [root] expresses a path that is
       within the [root] file hierarchy. {{!ex_rooted}Examples}. *)

val relativize : root:t -> t -> t option
(** [relativize ~root p] expresses [p] relative to [root] without
    consulting the file system. This is:
    {ul
    {- [None] if [find_prefix (normalize root) (normalize p)] is [None] or
       if the number of initial relative [..] segments is larger in
       [(normalize root)] than in [normalize p] (intuitively you can't
       come back from [root] to [p] without knowing the absolute path to
       the current working directory).}
    {- [Some q] otherwise with [q] such that
       [equal (normalize (append root q)) (normalize p) = true].}}
       {{!ex_relativize}Examples.} *)

(** {1:predicates Predicates and comparison} *)

val is_rel : t -> bool
(** [is_rel p] is [true] iff [p] is a relative path. *)

val is_abs : t -> bool
(** [is_abs p] is [true] iff [p] is an absolute path. *)

val is_root : t -> bool
(** [is_root p] is [true] if [p] is a root directory, that is
    an absolute path with a single empty segment.
    {{!ex_is_root}Examples}.

    {b Warning.} This is a structural test and will return [false]
    e.g. on ["/a/.."]. {!normalize} the path before testing to avoid
    this problem. *)

val is_current_dir : t -> bool
(** [is_current_dir p] is true iff [p] is the current relative directory,
    i.e. either ["."] or ["./"]. {{!ex_is_current_dir}Examples}.

    {b Warning.} This is a structural test and will return [false],
    e.g. on ["./a/.."]. {!normalize} the path before testing to avoid
    this problem. *)

val is_dotfile : t -> bool
(** [is_dotfile p] is [true] iff [p]'s {!name} is not
    ["."] or [".."] and starts with a [.]. *)

val equal : t -> t -> bool
(** [equal p p'] is [true] if [p] and [p'] have the same volume
    are both relative or absolute and have the same segments. This
    is a byte level comparison. *)

val compare : t  -> t -> int
(** [compare p p'] is a total order on paths compatible with {!equal}. *)

(** {1:conversions Conversions and pretty printing} *)

val to_string : t -> string
(** [to_string p] is the path [p] as a string. This path can
    be safely read back by {!v}. *)

val of_string : string -> t option
(** [of_string s] is the string [s] as a path. [None] is returned if
    there is a ['\x00'] byte in [s] or, on Windows, if this is an
    invalid UNC path (e.g. ["\\\\"] or ["\\\\a"]). The following
    transformations are performed:
    {ul
    {- On Windows any ['/'] occurence is converted to ['\\'] before
       any processing occurs.}
    {- Non-initial empty segments are suppressed;
       ["a//b"] becomes ["a/b"], ["//a////b//"] becomes ["//a/b/"], etc.}
    {- Empty relative paths are converted to [./]. For example
       [""] becomes ["./"], ["C:"] becomes ["C:./"], etc.}
    {- On Windows empty absolute UNC paths are completed to
       their root. For example ["\\\\server\\share"] becomes
       ["\\\\server\\share\\"],
       but incomplete UNC volumes like ["\\\\a"] return [None].}} *)

val pp : Format.formatter -> t -> unit
(** [pp ppf p] prints path [p] on [ppf] using {!to_string}. *)

val dump : Format.formatter -> t -> unit
(** [dump ppf p] prints path [p] on [ppf] using {!String.dump}. *)

(** {1:file_exts File extensions}

    The {e file extension} (resp. {e multiple file extension}) of a
    path segment is the suffix that starts at the last (resp. first)
    occurence of a ['.'] that is preceeded by at least one non ['.']
    character.  If there is no such occurence in the segment, the
    extension is empty.  With these definitions, ["."], [".."],
    ["..."] and dot files like [".ocamlinit"] or ["..ocamlinit"] have
    no extension, but [".emacs.d"] and ["..emacs.d"] do have one. *)

type ext = string
(** The type for file extensions. *)

val get_ext : ?multi:bool -> t -> ext
(** [get_ext p] is [p]'s last segment file extension or the empty
    string if there is no extension. If [multi] is [true] (defaults to
    [false]), returns the multiple file extension. {{!ex_get_ext}Examples}. *)

val has_ext : ext -> t -> bool
(** [has_ext e p] is [true] iff [ext p = e || ext ~multi:true p = e].
    If [e] doesn't start with a ['.'] one is prefixed before making
    the test. {{!ex_has_ext}Examples}. *)

val ext_exists : ?multi:bool -> t -> bool
(** [ext_exists ~multi p] is [true] iff [p]'s last segment has an
    extension.  If [multi] is [true] (default to [false]) returns
    [true] iff [p] has {e more than one} extension.
    {{!ex_ext_exists}Examples}. *)

val add_ext : ext -> t -> t
(** [add_ext ext p] is [p] with the string [ext] concatenated to [p]'s
    last segment. If [ext] doesn't start with a ['.'] one is prefixed to it
    before concatenation except if [ext] is [""]. {{!ex_add_ext}Examples}.

    @raise Invalid_argument if {!is_seg_valid}[ ext] is [false]. *)

val rem_ext : ?multi:bool -> t -> t
(** [rem_ext p] is [p] with the file extension of [p]'s last segment
    removed. If [multi] is [true] (default to [false]), the multiple
    file extension is removed. {{!ex_rem_ext}Examples}. *)

val set_ext : ?multi:bool -> ext -> t -> t
(** [set_ext ?multi p ext] is [add_ext ext (rem_ext ?multi p)].

    @raise Invalid_argument if {!is_seg_valid}[ ext] is [false]. *)

val split_ext : ?multi:bool -> t -> t * ext
(** [split_ext ?multi p] is [(rem_ext ?multi p, ext ?multi p)].

    Using [(p', ext)] for the resulting pair, the following invariant
    holds:
    {ul
    {- [equal (v (to_string p' ^ ext)) p]}} *)

val ( + ) : t -> ext -> t
(** [p + ext] is [add_ext ext p]. Left associative. *)

val ( -+ ) : t -> ext -> t
(** [p -+ ext] is [set_ext ext p]. Left associative. *)

(** {1:sets_maps Path sets and maps} *)

type path = t

type set
(** The type for path sets *)

(** Path sets. *)
module Set : sig

  (** {1 Path sets} *)

  include Set.S with type elt := path
                 and type t := set

  type t = set

  val min_elt : set -> path option
  (** Exception safe {!Set.S.min_elt}. *)

  val get_min_elt : set -> path
  (** [get_min_let] is like {!min_elt} but @raise Invalid_argument
        on the empty set. *)

  val max_elt : set -> path option
  (** Exception safe {!Set.S.max_elt}. *)

  val get_max_elt : set -> path
  (** [get_max_elt] is like {!max_elt} but @raise Invalid_argument
        on the empty set. *)

  val choose : set -> path option
  (** Exception safe {!Set.S.choose}. *)

  val get_any_elt : set -> path
  (** [get_any_elt] is like {!choose} but @raise Invalid_argument on the
        empty set. *)

  val find : path -> set -> path option
  (** Exception safe {!Set.S.find}. *)

  val get : path -> set -> path
  (** [get] is like {!Set.S.find} but @raise Invalid_argument if
        [elt] is not in [s]. *)

  val of_list : path list -> set
  (** [of_list ps] is a set from the list [ps]. *)

  val pp : ?sep:(Format.formatter -> unit -> unit) ->
    (Format.formatter -> string -> unit) ->
    Format.formatter -> set -> unit
  (** [pp ~sep pp_elt ppf ps] formats the elements of [ps] on
        [ppf]. Each element is formatted with [pp_elt] and elements
        are separated by [~sep] (defaults to
        {!Format.pp_print_cut}). If the set is empty leaves [ppf]
        untouched. *)

  val dump : Format.formatter -> set -> unit
  (** [dump ppf ps] prints an unspecified representation of [ps] on
        [ppf]. *)
end

type +'a map
(** The type for maps from paths to values of type ['a]. *)

(** Path maps. *)
module Map : sig

  (** {1 Path maps} *)

  include Map.S with type key := t
                 and type 'a t := 'a map

  type 'a t = 'a map

  val min_binding : 'a map -> (path * 'a) option
  (** Exception safe {!Map.S.min_binding}. *)

  val get_min_binding : 'a map -> (path * 'a)
  (** [get_min_binding] is like {!min_binding} but @raise Invalid_argument
      on the empty map. *)

  val max_binding : 'a map -> (path * 'a) option
  (** Exception safe {!Map.S.max_binding}. *)

  val get_max_binding : 'a map -> string * 'a
  (** [get_min_binding] is like {!max_binding} but @raise Invalid_argument
      on the empty map. *)

  val choose : 'a map -> (path * 'a) option
  (** Exception safe {!Map.S.choose}. *)

  val get_any_binding : 'a map -> (path * 'a)
  (** [get_any_binding] is like {!choose} but @raise Invalid_argument
      on the empty map. *)

  val find : path -> 'a map -> 'a option
  (** Exception safe {!Map.S.find}. *)

  val get : path -> 'a map -> 'a
  (** [get k m] is like {!Map.S.find} but raises [Invalid_argument] if
      [k] is not bound in [m]. *)

  val dom : 'a map -> set
  (** [dom m] is the domain of [m]. *)

  val of_list : (path * 'a) list -> 'a map
  (** [of_list bs] is [List.fold_left (fun m (k, v) -> add k v m) empty
      bs]. *)

  val pp : ?sep:(Format.formatter -> unit -> unit) ->
    (Format.formatter -> path * 'a -> unit) -> Format.formatter ->
    'a map -> unit
  (** [pp ~sep pp_binding ppf m] formats the bindings of [m] on
      [ppf]. Each binding is formatted with [pp_binding] and
      bindings are separated by [sep] (defaults to
      {!Format.pp_print_cut}). If the map is empty leaves [ppf]
      untouched. *)

  val dump : (Format.formatter -> 'a -> unit) -> Format.formatter ->
    'a map -> unit
  (** [dump pp_v ppf m] prints an unspecified representation of [m] on
        [ppf] using [pp_v] to print the map codomain elements. *)
end

(** {1:tips Tips}

    {ul
    {- Windows accepts both ['\\'] and ['/'] as directory
       separator.  However [Fpath] on Windows converts ['/'] to ['\\'] on
       the fly. Therefore you should either use ['/'] for defining
       constant paths you inject with {!v} or better, construct them
       directly with {!(/)}. {!to_string} will convert these paths
       to strings using the platform's specific directory
       separator {!dir_sep}.}
    {- Avoid platform specific {{!split_volume}volumes} or hard-coding file
       hierarchy conventions in your constants.}
    {- Do not assume there is a single root path and that it is
       [/]. On Windows each {{!split_volume}volume} can have a root path.
       Use {!is_root} to detect root paths.}
    {- Do not use {!to_string} to construct URIs, {!to_string} uses
       {!dir_sep} to separate segments, on Windows this is ['\\'] which
       is not what URIs expect. Access the path segments directly
       with {!segs}, note that you will need to percent encode these.}}

    {1:ex Examples}

    {2:ex_add_seg {!add_seg}}

    {ul
    {- [equal (add_seg (v "/a") "b") (v "/a/b")]}
    {- [equal (add_seg (v "/a/") "b") (v "/a/b")]}
    {- [equal (add_seg (v "/a/b") "") (v "/a/b/")]}
    {- [equal (add_seg (v "/a/b/") "") (v "/a/b/")]}
    {- [equal (add_seg (v "/") "") (v "/")]}
    {- [equal (add_seg (v "/") "a") (v "/a")]}
    {- [equal (add_seg (v ".") "") (v "./")]}
    {- [equal (add_seg (v ".") "a") (v "./a")]}
    {- [equal (add_seg (v "..") "") (v "../")]}
    {- [equal (add_seg (v "..") "a") (v "../a")]}}

    {2:ex_append {!append}}

    {ul
    {- [equal (append (v "/a/b/") (v "e/f")) (v "/a/b/e/f")]}
    {- [equal (append (v "/a/b") (v "e/f")) (v "/a/b/e/f")]}
    {- [equal (append (v "/a/b/") (v "/e/f")) (v "/e/f")]}
    {- [equal (append (v "a/b/") (v "e/f")) (v "a/b/e/f")]}
    {- [equal (append (v "a/b") (v "C:e")) (v "C:e")] (Windows)}}

    {2:ex_segs {!segs}}

    {ul
    {- [segs (v "/a/b/") = [""; "a"; "b"; ""]]}
    {- [segs (v "/a/b") = [""; "a"; "b"]]}
    {- [segs (v "a/b/") = ["a"; "b"; ""]]}
    {- [segs (v "a/b") = ["a"; "b"]]}
    {- [segs (v "a") = ["a"]]}
    {- [segs (v "") = ["."]]}
    {- [segs (v "/") = [""; ""]]}
    {- [segs (v "\\\\.\\dev\\") = ["";""]] (Windows)}
    {- [segs (v "\\\\server\\share\\a") = ["";"a"]] (Windows)}
    {- [segs (v "C:a") = ["a"]] (Windows)}
    {- [segs (v "C:\\a") = ["";"a"]] (Windows)}}

    {2:ex_filename {!filename}}

    {ul
    {- [filename (v "/a/b/") = ""]}
    {- [filename (v "/a/b") = "b"]}
    {- [filename (v "a") = "a"]}
    {- [filename (v "a/") = ""]}
    {- [filename (v "/") = ""]}
    {- [filename (v "C:\\") = ""] (Windows)}
    {- [filename (v "C:a") = "a"] (Windows)}}

    {2:ex_file_to_dir {!file_to_dir}}

    {ul
    {- [equal (file_to_dir @@ v "/a/b") (v "/a/b/")]}
    {- [equal (file_to_dir @@ v "/a/b/") (v "/a/b/")]}
    {- [equal (file_to_dir @@ v "a") (v "a/")]}
    {- [equal (file_to_dir @@ v "/") (v "/")]}
    {- [equal (file_to_dir @@ v "\\\\server\\share\\")
       (v "\\\\server\\share\\")]
       (Windows)}
    {- [equal (file_to_dir @@ v "C:a") (v "C:a/")] (Windows)}
    {- [equal (file_to_dir @@ v "C:\\") (v "C:\\")] (Windows)}}

    {2:ex_dir_to_file {!dir_to_file}}

    {ul
    {- [equal (dir_to_file @@ v "/a/b") (v "/a/b")]}
    {- [equal (dir_to_file @@ v "/a/b/") (v "/a/b")]}
    {- [equal (dir_to_file @@ v "a/") (v "a")]}
    {- [equal (dir_to_file @@ v "/") (v "/")]}
    {- [equal (dir_to_file @@ v "\\\\server\\share\\")
       (v "\\\\server\\share\\")]
       (Windows)}
    {- [equal (dir_to_file @@ v "C:a/") (v "C:a")] (Windows)}
    {- [equal (dir_to_file @@ v "C:\\") (v "C:\\")] (Windows)}}

    {2:ex_name {!name}}

    {ul
    {- [name (v "/a/b/") = "b"]}
    {- [name (v "/a/b") = "b"]}
    {- [name (v "a") = "a"]}
    {- [name (v "a/") = "a"]}
    {- [name (v "/") = ""]}
    {- [name (v "C:\\") = ""] (Windows)}
    {- [name (v "C:a") = "a"] (Windows)}}

    {2:ex_base {!base}}

    {ul
    {- [equal (base @@ v "/a/b/") (v "b")]}
    {- [equal (base @@ v "/a/b") (v "b")]}
    {- [equal (base @@ v "a") (v "a")]}
    {- [equal (base @@ v ".") (v ".")]}
    {- [equal (base @@ v "..") (v "..")]}
    {- [equal (base @@ v "/") (v "/")]}
    {- [equal (base @@ v "\\\\server\\share\\") (v "\\\\server\\share\\")]
       (Windows)}
    {- [equal (base @@ v "C:\\") (v "C:\\")] (Windows)}}

    {2:ex_parent {!parent}}

    {ul
    {- [equal (parent @@ v "/a/b") (v "/a")]}
    {- [equal (parent @@ v "/a/b/") (v "/a")]}
    {- [equal (parent @@ v "/a") (v "/")]}
    {- [equal (parent @@ v "/a/") (v "/")]}
    {- [equal (parent @@ v "a/b/") (v "a")]}
    {- [equal (parent @@ v "a/b") (v "a")]}
    {- [equal (parent @@ v "a") (v ".")]}
    {- [equal (parent @@ v "a/") (v ".")]}
    {- [equal (parent @@ v ".") (v ".")]}
    {- [equal (parent @@ v "..") (v ".")]}
    {- [equal (parent @@ v "/") (v "/")]}
    {- [equal (parent @@ v "\\\\server\\share\\") (v "\\\\server\\share\\")]
       (Windows)}
    {- [equal (parent @@ v "C:a") (v "C:.")] (Windows)}
    {- [equal (parent @@ v "C:\\") (v "C:\\")] (Windows)}}

    {2:ex_is_prefix {!is_prefix}}

    {ul
    {- [is_prefix (v "/a/b") (v "/a/b") = true]}
    {- [is_prefix (v "/a/b") (v "/a/b/") = true]}
    {- [is_prefix (v "/a/b") (v "/a/bc") = false]}
    {- [is_prefix (v "/a/b") (v "/a/b/c") = true]}
    {- [is_prefix (v "/a/b/") (v "/a/b") = false]}
    {- [is_prefix (v "a/b") (v "/a/b") = false]}
    {- [is_prefix (v "a/b") (v "a/b") = true]}
    {- [is_prefix (v "//a/b") (v "/a/b") = false]}
    {- [is_prefix (v "C:a") (v "a") = false] (Windows)}}

    {2:ex_find_prefix {!find_prefix}}

    {ul
    {- [find_prefix (v "a/b/c") (v "a/b/d")] is [Some (v "a/b/")]}
    {- [find_prefix (v "a/b/c") (v "a/b/cd")] is [Some (v "a/b/")]}
    {- [find_prefix (v "/a/b/c") (v "/a/b/d")] is [Some (v "/a/b/")]}
    {- [find_prefix (v "a/b") (v "e/f")] is [Some (v ".")]}
    {- [find_prefix (v "/a/b") (v "/e/f")] is [Some (v "/")]}
    {- [find_prefix (v "/a/b") (v "e/f")] is [None]}
    {- [find_prefix (v "C:\\a") (v "\\a")] is [None] (Windows)}}

    {2:ex_rem_prefix {!rem_prefix}}

    {ul
    {- [rem_prefix (v "/a/b") (v "/a/bc")] is [None]}
    {- [rem_prefix (v "/a/b") (v "/a/b")] is [Some (v ".")]}
    {- [rem_prefix (v "/a/b/") (v "/a/b")] is [None]}
    {- [rem_prefix (v "/a/b") (v "/a/b/")] is [Some (v ".")]}
    {- [rem_prefix (v "/a/b/") (v "/a/b/")] is [Some (v ".")]}
    {- [rem_prefix (v "/a/b") (v "/a/b/c")] is [Some (v "c")]}
    {- [rem_prefix (v "/a/b/") (v "/a/b/c")] is [Some (v "c")]}
    {- [rem_prefix (v "a") (v "a/b/c")] is [Some (v "b/c")]}}

    {2:ex_normalize {!normalize}}

    {ul
    {- [equal (normalize @@ v "./a/..") (v ".")]}
    {- [equal (normalize @@ v "/a/b/./..") (v "/a")]}
    {- [equal (normalize @@ v "/../..") (v "/")]}
    {- [equal (normalize @@ v "/a/../..") (v "/")]}
    {- [equal (normalize @@ v "./../..") (v "../..")]}
    {- [equal (normalize @@ v "../../a/") (v "../../a")]}
    {- [equal (normalize @@ v "/a/b/c/./../../g") (v "/a/g")]}
    {- [equal (normalize @@ v "\\\\?\\UNC\\server\\share\\..")
       (v "\\\\?\\UNC\\server\\share\\")] (Windows)}}

    {2:ex_rooted {!rooted}}

    {ul
    {- [rooted (v "/a/b") (v "c")] is [Some (v "/a/b/c")]}
    {- [rooted (v "/a/b") (v "/a/b/c")] is [Some (v "/a/b/c")]}
    {- [rooted (v "/a/b") (v "/a/b/c/")] is [Some (v "/a/b/c")]}
    {- [rooted (v "/a/b") (v "/a/b/c/.")] is [Some (v "/a/b/c")]}
    {- [rooted (v "/a/b") (v "../c")] is [None]}
    {- [rooted (v "a/b") (v "c")] is [Some (v "a/b/c")]}
    {- [rooted (v "a/b") (v "/c")] is [None]}
    {- [rooted (v "a/b") (v "../c")] is [None]}
    {- [rooted (v "a/b") (v "c/..")] is [Some (v "a/b")]}
    {- [rooted (v "a/b") (v "c/../..")] is [None]}}

    {2:ex_relativize {!relativize}}

    {ul
    {- [relativize (v "/a/b") (v "c")] is [None]}
    {- [relativize (v "/a/b") (v "/c")] is [Some (v "../../c")]}
    {- [relativize (v "/a/b") (v "/c/")] is [Some (v "../../c")]}
    {- [relativize (v "/a/b") (v "/a/b/c")] is [Some (v "c")]}
    {- [relativize (v "/a/b") (v "/a/b")] is [Some (v ".")]}
    {- [relativize (v "/a/b") (v "/a/b/")] is [Some (v ".")]}
    {- [relativize (v "a/b") (v "/c")] is [None].}
    {- [relativize (v "a/b") (v "c")] is [Some (v "../../c")]}
    {- [relativize (v "a/b") (v "c/")] is [Some (v "../../c")]}
    {- [relativize (v "a/b") (v "a/b/c")] is [Some (v "c")]}
    {- [relativize (v "a/b") (v "a/b")] is [Some (v ".")]}
    {- [relativize (v "a/b") (v "a/b/")] is [Some (v ".")]}
    {- [relativize (v "../a") (v "b")] is [None]}
    {- [relativize (v "../../a") (v "../b")] is [None]}
    {- [relativize (v "../a") (v "../../b")] is [(Some "../../b")]}}

    {2:ex_is_root {!is_root}}
    {ul
    {- [is_root (v "//") = true]}
    {- [is_root (v "/") = true]}
    {- [is_root (v "/a/..") = false]}
    {- [is_root (v "/a") = false]}
    {- [is_root (v "a") = false]}
    {- [is_root (v ".") = false]}
    {- [is_root (v "\\\\.\\dev\\") = true] (Windows)}
    {- [is_root (v "\\\\.\\dev\\a") = false] (Windows)}
    {- [is_root (v "\\\\server\\share\\") = true] (Windows)}
    {- [is_root (v "\\\\server\\share\\a") = false] (Windows)}
    {- [is_root (v "C:\\") = true] (Windows)}
    {- [is_root (v "C:a") = false] (Windows)}
    {- [is_root (v "C:\\a") = false] (Windows)}}

    {2:ex_is_current_dir {!is_current_dir}}
    {ul
    {- [is_current_dir (v ".") = true]}
    {- [is_current_dir (v "./") = true]}
    {- [is_current_dir (v "./a/..") = false]}
    {- [is_current_dir (v "/.") = false]}
    {- [is_current_dir (v "\\\\.\\dev\\.") = false] (Windows)}
    {- [is_current_dir (v "\\\\.\\dev\\.\\") = false] (Windows)}
    {- [is_current_dir (v "\\\\server\\share\\.") = false] (Windows)}
    {- [is_current_dir (v "\\\\server\\share\\.\\") = false] (Windows)}
    {- [is_current_dir (v "C:.") = true] (Windows)}
    {- [is_current_dir (v "C:./") = true] (Windows)}
    {- [is_current_dir (v "C:./a/..") = false] (Windows)}}

    {2:ex_get_ext {!get_ext}}

    {ul
    {- [get_ext (v "/a/b") = ""]}
    {- [get_ext (v "a/.") = ""]}
    {- [get_ext (v "a/..") = ""]}
    {- [get_ext (v "a/.ocamlinit") = ""]}
    {- [get_ext (v "/a/b.") = "."]}
    {- [get_ext (v "/a/b.mli") = ".mli"]}
    {- [get_ext (v "a.tar.gz") = ".gz"]}
    {- [get_ext (v "a/.emacs.d") = ".d"]}
    {- [get_ext ~multi:true (v "/a/b.mli") = ".mli"]}
    {- [get_ext ~multi:true (v "a.tar.gz") = ".tar.gz"]}
    {- [get_ext ~multi:true (v "a/.emacs.d") = ".d"]}}

    {2:ex_has_ext {!has_ext}}

    {ul
    {- [has_ext ".mli" (v "a/b.mli")  = true]}
    {- [has_ext "mli" (v "a/b.mli")  = true]}
    {- [has_ext "mli" (v "a/bmli")  = false]}
    {- [has_ext ".tar.gz" (v "a/f.tar.gz") = true]}
    {- [has_ext "tar.gz" (v "a/f.tar.gz") = true]}
    {- [has_ext ".tar" (v "a/f.tar.gz") = false]}}

    {2:ex_ext_exists {!ext_exists}}

    {ul
    {- [ext_exists (v "a/f") = false]}
    {- [ext_exists (v "a/f.") = true]}
    {- [ext_exists (v "a/f.gz") = true]}
    {- [ext_exists (v "a/f.tar.gz") = true]}
    {- [ext_exists (v ".emacs.d") = true]}
    {- [ext_exists ~multi:true (v "a/f.gz") = false]}
    {- [ext_exists ~multi:true (v "a/f.tar.gz") = true]}
    {- [ext_exists ~multi:true (v ".emacs.d") = false]}}

    {2:ex_add_ext {!add_ext}}

    {ul
    {- [equal (add_ext ".mli" (v "a/b")) (v "a/b.mli")]}
    {- [equal (add_ext "mli" (v "a/b")) (v "a/b.mli")]}
    {- [equal (add_ext "." (v "a/b")) (v "a/b.")]}
    {- [equal (add_ext "" (v "a/b")) (v "a/b")]}
    {- [equal (add_ext ".tar.gz" (v "a/f")) (v "a/f.tar.gz")]}
    {- [equal (add_ext "tar.gz" (v "a/f")) (v "a/f.tar.gz")]}
    {- [equal (add_ext ".gz" (v "a/f.tar") ) (v "a/f.tar.gz")]}
    {- [equal (add_ext "gz" (v "a/f.tar") ) (v "a/f.tar.gz")]}}

    {2:ex_rem_ext {!rem_ext}}

    {ul
    {- [equal (rem_ext @@ v "/a/b") (v "/a/b")]}
    {- [equal (rem_ext @@ v "/a/b.mli") (v "/a/b")]}
    {- [equal (rem_ext @@ v "a/.ocamlinit") (v "a/.ocamlinit")]}
    {- [equal (rem_ext @@ v "f.tar.gz") (v "f.tar")]}
    {- [equal (rem_ext ~multi:true @@ v "f.tar.gz") (v "f")]}} *)

(*---------------------------------------------------------------------------
   Copyright 2014 Daniel C. Bünzli.
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
