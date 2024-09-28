(*---------------------------------------------------------------------------
   Copyright (c) 2015 The fpath programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let tests () = Testing.run
    [ Test_fpath.suite; ]

let run () = tests (); Testing.log_results ()

let () = if run () then exit 0 else exit 1
