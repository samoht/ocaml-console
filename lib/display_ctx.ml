(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = {
  ppf : Format.formatter;
  now : unit -> float;
  wait : float -> unit;
  dimensions : unit -> int * int;
  is_tty : bool;
  char_width : (Uchar.t -> int) option;
}

let v ?char_width ~ppf ~now ~wait ~dimensions ~is_tty () =
  { ppf; now; wait; dimensions; is_tty; char_width }

let pp ppf t =
  let width, height = t.dimensions () in
  Fmt.pf ppf "<ctx %dx%d%s>" width height (if t.is_tty then " tty" else "")
