(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = {
  ppf : Format.formatter;
  now : unit -> float;
  wait : (float -> unit) option;
  dimensions : unit -> int * int;
  is_tty : bool;
  char_width : (Uchar.t -> int) option;
}

let v ~ppf ~now ?wait ~dimensions ~is_tty () =
  { ppf; now; wait; dimensions; is_tty; char_width = None }

let ppf t = t.ppf

let pp ppf t =
  let width, height = t.dimensions () in
  Fmt.pf ppf "<ctx %dx%d%s>" width height (if t.is_tty then " tty" else "")

let now t = t.now ()
let with_char_width t char_width = { t with char_width = Some char_width }

let dumb () =
  v ~ppf:Format.std_formatter
    ~now:(fun () -> 0.)
    ~dimensions:(fun () -> (80, 24))
    ~is_tty:false ()
